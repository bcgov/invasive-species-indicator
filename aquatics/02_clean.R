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


library(dplyr) # data munging
source("fun.R") # many functions


## Check to make sure the files are in the same projection
compare_all(proj4string(ais), proj4string(eaubc), proj4string(eaubc_fw_eco))

check_tax <- ais@data %>%
  group_by(TAXON_GRP, SCIENCE_NM, ENGLISH_NM, ELCODE) %>%
  summarize(n=n()) %>%
  arrange(TAXON_GRP, SCIENCE_NM) %>%
  ungroup()

## Find inconsistencies in naming by looking for duplicated names and ELCODES
## in the check_tax dataframe, which should have only one for each 
## SciName/EnglName/ELCODE
get_dups <- function(df, col) {
  dupvals <- df[[col]][duplicated(df[[col]])]
  dupdf <- df[df[[col]] %in% dupvals, , drop = FALSE]
  return(dupdf)
}

dups <- lapply(names(check_tax)[2:4], function(x) get_dups(check_tax, x))
dups <- do.call(dplyr::union, dups)


## Do some fixes
ais$ENGLISH_NM[ais$SCIENCE_NM == "Alosa sapidissima"] <- "American Shad"
ais$SCIENCE_NM[ais$ELCODE == "IMGASE7010"] <- "Cipangopaludina chinensis"
ais$ELCODE[ais$SCIENCE_NM == "Cipangopaludina chinensis"] <- "IMGASE7010"
## Check for duplicated SPECIDs (should be unique identifier) 
OCCURR_ID_dups <- ais@data[ais@data$OCCURR_ID %in% ais@data$OCCURR_ID[duplicated(ais$OCCURR_ID)],] %>%
  arrange(OCCURR_ID)
## None found

## Look for missing SciNames
ais$ENGLISH_NM[(is.na(ais$SCIENCE_NM))] # None

## 9999 used as a missing value identifier in YEAR_PUBL column:
ais$YEAR_PUBL[ais$YEAR_PUBL == 9999] <- NA

## Pluralise taxonomic group names
tax_groups <- unique(ais@data$TAXON_GRP)
tax_plurals <- ifelse(tax_groups %in% c("Algae", "Fish"), tax_groups, paste0(tax_groups, "s"))
names(tax_plurals) <- tax_groups
ais@data$TAXON_GRP <- unname(tax_plurals[ais@data$TAXON_GRP])
## ais$TAXON_GRP[ais$TAXON_GRP == "Reptiles"] <- "Reptiles & Turtles"


## Look for (minorly) mispelled SciNames
sapply(unique(ais@data$SCIENCE_NM), function(x) agrep(x, unique(ais@data$SCIENCE_NM), max.distance = 2))
## None found
