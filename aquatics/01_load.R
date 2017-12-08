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

## Download aquatic invasives data from DataBC using BC Albers Shapefile: 
## http://catalogue.data.gov.bc.ca/dataset/aquatic-invasive-species-of-british-columbia
## Save in data dir 
## Aquatics data provided under the Open Government Licence - BC
## http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc

## Download EauBC units from DataBC use BC Albers Shapefile:
## http://catalogue.data.gov.bc.ca/dataset/eaubc-ecological-drainage-units
## Save in data directory
## EauBC units provided under BC Crown Copyright License
## http://www2.gov.bc.ca/gov/content/home/copyright

ais_zip <- "~/soe_data/plants_animals/invasives/2015/ais/AIS_DataBC_2015-06-12.zip" #match local data dir
eaubc_zip <- "~/soe_data/plants_animals/invasives/2015/EAUBC/EAUBC_EDU_DataBC.zip" #match local data dir
fw_ecoreg <- "~/soe_data/plants_animals/invasives/2015/EAUBC/EAUBC_FW_ECOREG.zip" #match local data dir

unzip(ais_zip, exdir = "aquatics/data")
unzip(eaubc_zip, exdir = "aquatics/data")
unzip(fw_ecoreg, exdir = "aquatics/data")


## Load data -------------------------------------------
library(rgdal) #read in shapefile as SP object
library(rmapshaper) #simplify SP object

ais <- readOGR("aquatics/data/FSHQTCNVSV", layer = "FSHQTCNVSV_point", stringsAsFactors = FALSE)
eaubc <- readOGR("aquatics/data/EABC_EC_DR", layer = "EABC_EC_DR_polygon", stringsAsFactors = FALSE)
eaubc_fw_eco <- readOGR("aquatics/data/EAUBC_ECR", layer = "EAUBC_ECR_polygon", stringsAsFactors = FALSE)

## Simplify polygons using rmapshaper
eaubc_simp <- ms_simplify(eaubc, keep = 0.001, keep_shapes = TRUE)
eaubc_fw_eco_simp <- ms_simplify(eaubc_fw_eco, keep = 0.001, keep_shapes = TRUE)

writeOGR(eaubc_simp, "aquatics/data", "eaubc_simp", "ESRI Shapefile")

