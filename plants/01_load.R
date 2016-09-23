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

## Download invasive plants data from DataBC using BC Albers Shapefile: 
## http://catalogue.data.gov.bc.ca/dataset/invasive-alien-plant-site
## Save in data dir
## Data provided under the BC Crown Copyright License
## http://www2.gov.bc.ca/gov/content/home/copyright

plants_zip <- "Z:/plants_animals/invasives/2015/iapp/IAP_SIT_PG.zip" #match local data dir
unzip(plants_zip, exdir = "plants/data")


## Load iapp INVASIVE ALIEN PLANT SITE SPATIAL data

library(rgdal) #loading a shapefile

dir.create("tmp", showWarnings = FALSE)

plant_site_rds <- "tmp/plant.sites.rds"
biozones_rds <- "tmp/biozones.rds"

if (!file.exists(plant_site_rds)) {
  plant.sites <- readOGR("plants/data/IAP_SIT_PG",
                         "IAP_SIT_PG_polygon", stringsAsFactors = FALSE) #as shape file
  saveRDS(plant.sites, plant_site_rds)
}

## Load biogeoclimatic zone data 

## Download full BGC Zones file from Geographic Warehouse
## https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3
## Save in data dir
## Data provided under the BC Crown Copyright License
## http://www2.gov.bc.ca/gov/content/home/copyright

bec_zip <- "Z:/plants_animals/invasives/2015/dbc_biozones/BEC_POLY.zip"  #match local data dir
unzip(bec_zip, exdir = "plants/data")


## Use mapshaper and to clip and explode full BEC shapefile polygons. 
## Must explode multipart polygons or simplify will remove some.
## It is a Node library, so you need to have 
## Node installed to use it: https://nodejs.org/download/
## Then install mapshaper on the command line with: 'npm install -g mapshaper'
## https://github.com/mbloch/mapshaper/wiki/Command-Reference

#devtools::install_github("bcgov/bcmaps")
library(bcmaps)
writeOGR(bc_bound_hres, "plants/data", "bc_bound", "ESRI Shapefile")
system("mapshaper plants/data/BEC_POLY/BEC_POLY_polygon.shp -clip plants/data/bc_bound.shp -o plants/data/bgc_clip.shp force")
system("mapshaper plants/data/bgc_clip.shp -explode -o plants/data/bgc_clip_explode.shp force")

if (!file.exists(biozones_rds)) {
  biozones <- readOGR("plants/data",
                      "bgc_clip_explode", stringsAsFactors = FALSE)
  saveRDS(biozones, biozones_rds)
}

