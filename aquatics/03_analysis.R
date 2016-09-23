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

## check for required packages and install if not there
pkgs <- c("rgeos", "rgdal", "maptools", "ggplot2", "scales", "grid", "envreportutils")
lapply(pkgs, function(x) if (!require(x, character.only = TRUE)) install.packages(x))

source("fun.R")

## Get year of observation, if not there, use year published
ais$year = get_year(ais$COL_DATE)
ais$year[is.na(ais$year)] <- ais$YEAR_PUBL[is.na(ais$year)]

## Get edus into ais data
ais@data <- cbind(ais@data, sp::over(ais, eaubc)[, c("FW_ECOREG", "EDU")])

## There are some points that don't fall into edus and thus didn't get a value.
nearest_edus <- nearest(ais[is.na(ais$EDU), ], eaubc)
ais$EDU[is.na(ais$EDU)] <- nearest_edus$EDU
ais$FW_ECOREG[is.na(ais$FW_ECOREG)] <- nearest_edus$FW_ECOREG

names(ais) <- tolower(names(ais))

## Find number of distinct species in each edu, making sure to get zeros 
## in edus where there are none
n_distinct_no_na <- function(x) n_distinct( x[!is.na(x)] )
unique_edu_taxon <- expand.grid(taxon_grp = unique(ais@data$taxon_grp), 
                                edu = eaubc@data$EDU, 
                                stringsAsFactors = FALSE)

ais_edu_grp <-  left_join(unique_edu_taxon, ais@data, by = c("edu", "taxon_grp")) %>% 
  group_by(tax_group = taxon_grp, edu) %>% 
  summarise(n_spp = n_distinct_no_na(science_nm)) %>% 
  mutate(n_spp = ifelse(n_spp == 0, NA_integer_, n_spp))

## Get first year species was observed in a edu and pad all subsequent years
## with that species in that edu.
max_year <- max(ais@data$year, na.rm = TRUE)
edu_spp_yr_padded <- ais@data %>% 
  group_by(edu, taxon_grp, science_nm) %>%
  filter(!is.na(year)) %>%
  summarise(min_year = min(year)) %>% 
  rowwise() %>% 
  do(expand.grid(edu = .$edu, tax_group = .$taxon_grp, 
                 science_name = .$science_nm, year = .$min_year:max_year, 
                 stringsAsFactors = FALSE))

## Calculate cumulative number of species detected by edu over time
ais_year_edu <- edu_spp_yr_padded %>% 
  group_by(tax_group, edu, year) %>%
  arrange(year) %>% 
  summarise(n_species = n())

## now do all the same for fw_ecoregions:
unique_fwer_taxon <- expand.grid(taxon_grp = unique(ais@data$taxon_grp), 
                                 fw_ecoreg = eaubc@data$FW_ECOREG, 
                                 stringsAsFactors = FALSE)

ais_fwer_grp <-  left_join(unique_fwer_taxon, ais@data, by = c("fw_ecoreg", "taxon_grp")) %>% 
  group_by(tax_group = taxon_grp, fw_ecoreg) %>% 
  summarise(n_spp = n_distinct_no_na(science_nm)) %>% 
  mutate(n_spp = ifelse(n_spp == 0, NA_integer_, n_spp))

## Get first year species was observed in a fw_ecoreg and pad all subsequent years
## with that species in that fw_ecoreg
max_year <- max(ais@data$year, na.rm = TRUE)
fwer_spp_yr_padded <- ais@data %>% 
  group_by(fw_ecoreg, taxon_grp, science_nm) %>%
  filter(!is.na(year)) %>%
  summarise(min_year = min(year)) %>% 
  rowwise() %>% 
  do(expand.grid(fw_ecoreg = .$fw_ecoreg, tax_group = .$taxon_grp, 
                 science_name = .$science_nm, year = .$min_year:max_year, 
                 stringsAsFactors = FALSE))

## Calculate cumulative number of species detected by fw_ecoreg over time
ais_year_fwer <- fwer_spp_yr_padded %>% 
  group_by(tax_group, fw_ecoreg, year) %>%
  arrange(year) %>% 
  summarise(n_species = n())

## And provincially:
unique_prov_taxon <- expand.grid(taxon_grp = unique(ais@data$taxon_grp), stringsAsFactors = FALSE)

ais_prov_grp <-  left_join(unique_prov_taxon, ais@data, by = "taxon_grp") %>% 
  group_by(tax_group = taxon_grp) %>% 
  summarise(n_spp = n_distinct_no_na(science_nm)) %>% 
  mutate(n_spp = ifelse(n_spp == 0, NA_integer_, n_spp))

## Get first year species was observed in a fw_ecoreg and pad all subsequent years
## with that species in that fw_ecoreg.
max_year <- max(ais@data$year, na.rm = TRUE)
prov_spp_yr_padded <- ais@data %>% 
  group_by(taxon_grp, science_nm) %>%
  filter(!is.na(year)) %>%
  summarise(min_year = min(year)) %>% 
  rowwise() %>% 
  do(expand.grid(tax_group = .$taxon_grp, 
                 science_name = .$science_nm, year = .$min_year:max_year, 
                 stringsAsFactors = FALSE))

## Calculate cumulative number of species detected by fw_ecoreg over time
ais_year_prov <- prov_spp_yr_padded %>% 
  group_by(tax_group, year) %>%
  arrange(year) %>% 
  summarise(n_species = n())

## Calculate number of species per edu per decade (include NAs)
fish_by_edu_by_decade <- ais_year_edu %>% 
  right_join(expand.grid(tax_group = unique(ais_year_edu$tax_group), 
                         edu = unique(eaubc@data$EDU), 
                         year = sort(unique(ais_year_edu$year))), 
             by = c("tax_group", "edu", "year")) %>% 
  filter(tax_group == "Fish", year %% 10 == 0 & year >= 1900 & year < 2010 | year == max(year))

save.image("tmp/aquatics_analyses.rda")
