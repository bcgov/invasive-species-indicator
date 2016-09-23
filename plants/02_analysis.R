# Copyright 2016 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

require(dplyr) #data munging
require(rgdal) #mapping
require(maptools) #mapping
require(sp) #mapping
require(tidyr) #changing dataframe shape

options(stringsAsFactors = FALSE)

source("fun.R") #functions in fun.R file

## Loading stored shape files in tmp folder 
bgc_full <- readRDS("tmp/biozones.rds")
plantsites <- readRDS("tmp/plant.sites.rds")

## Load additional file with MAP_LABEL values for several sites where there were
## more than 16 species, which were truncated in the original file
## Contact EnvReportBC for this file (EnvReportBC@gov.bc.ca)

full_map_labels <- read.csv("Z:/plants_animals/invasives/2015/iapp/IAPP_sites_16_plus_spp_2015-07-15.csv", 
                            stringsAsFactors = FALSE, check.names = FALSE)
full_map_labels <- t(full_map_labels)[-1,]
full_map_labels <- apply(full_map_labels, 1, paste, collapse = " ")
full_map_labels <- gsub("([A-Z])\\s+$", "\\1", full_map_labels)
full_map_labels <- data.frame(SITE_ID = as.integer(names(full_map_labels)), 
                              MAP_LABEL2 = full_map_labels, row.names = NULL, 
                              stringsAsFactors = FALSE)

plantsites <- merge(plantsites, full_map_labels, by = "SITE_ID")
plantsites$MAP_LABEL[!is.na(plantsites$MAP_LABEL2)] <- plantsites$MAP_LABEL2[!is.na(plantsites$MAP_LABEL2)]

## Subsetting file to createdataframe for map 
plantsites <- subset(plantsites, select = c("SITE_ID", "MAP_LABEL", "FTRR", "BGC_LABEL"))

## number of NA for BGC column
missingBGC.count <- sum(is.na(plantsites$BGC_LABEL))

## number of unique sites in the dataframe
totalsites <-  length(unique(plantsites$SITE_ID))

## Count of species in concatenated MAP_LABEL column 
plantsites$no.species <- sapply(strsplit(plantsites$MAP_LABEL," "),
                                length)

plant_site_centroids <- SpatialPointsDataFrame(coordinates(plantsites), 
                                               data = plantsites@data, 
                                               proj4string = CRS(proj4string(plantsites)))

rm(plantsites)

## filling in the NA BGC 
missing <- which(is.na(plant_site_centroids$BGC_LABEL))
plant_site_centroids$BGC_LABEL[missing] <- over(plant_site_centroids[missing, ], 
                                                bgc_full)[["BGC_LABEL"]]

## Works, but still a few left over:
#View(plantsites[is.na(plantsites$BGC), ])

## Six 'left-overs' all BWBS and likely edge centroids not in BC
still_missing <- which(is.na(plant_site_centroids$BGC_LABEL))
plant_site_centroids$BGC_LABEL[still_missing] <- nearest(plant_site_centroids[still_missing, ], 
                                                         bgc_full)[["BGC_LABEL"]]

## Extract BGC from concatenated BGC_LABEL column 
plant_site_centroids$BGC <- gsub("(^[A-Z]{2,4}).*","\\1", 
                                 plant_site_centroids$BGC_LABEL)

## Converting the plantsites spatial file to a dataframe for dplyr work
plantdf <- as.data.frame(plant_site_centroids, stringsAsFactors = FALSE)

## Fixing column headings for merging
# plantdf$BGC[plantdf$BGC == "Coastal Western Hemlock"] <- "CWH"
# plantdf$BGC[plantdf$BGC == "Boreal White and Black Spruce"] <- "BWBS"

plantdf$no.species <- as.integer(plantdf$no.species)

## Number of records and sites of species per BEC
summary <- plantdf %>%
  group_by(BGC) %>%
  summarise(
    no.records = sum(no.species),
    no.sites = n())

summary <- as.data.frame(summary)

## Creating column with number of differnt plant species in each biogeoclimatic zone

## subset source dataframe for munging
species <- subset(plantdf, select = c("SITE_ID", "MAP_LABEL", "no.species", "BGC"))

## split concatenated species IDs into separate columns
n_cols <- max(species$no.species)
species.wide <- separate(species, MAP_LABEL, letters[1:n_cols],
                         sep = " ", extra = "drop")

## change into long format for using dplyr
species.long <- species.wide %>% 
  gather(species.list, species_id, a:p)

## sum the 3 of different species by BGC
species.count <- species.long %>%
  filter(!is.na(species_id)) %>%
  group_by(BGC) %>% 
  summarise(
    unique_species = n_distinct(species_id))

## Add unique_species column into summary dataframe
summary <- merge(summary, species.count, by = "BGC")

## create tidy version of summary for facetting
summary.tidy <- summary %>% 
  gather(metric, value, -BGC)


## Summarize number of speces and number of sites in each BGC polygon
bgc_full$n_spp_spat <- poly_summary(bgc_full, plant_site_centroids, "MAP_LABEL", n_spp)
bgc_full$n_sites_spat <- poly_summary(bgc_full, plant_site_centroids, "MAP_LABEL", n_sites)
rm(plant_site_centroids)

## Simplify bgc SpatialPolygonsDataFrame
bgc_simp <- simplify(bgc_full, keep = 0.01)
rm(bgc_full)
## Errant hole in polygons - fix it:
slot(bgc_simp, "polygons") <- lapply(slot(bgc_simp, "polygons"), checkPolygonsHoles)
## Then fix resulting topology errors (self-intersections)
bgc_simp <- gBuffer(bgc_simp, byid = TRUE, width = 0)

save.image("tmp/plant_analyses.rda")
