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

## General BC CDC exotics summary
source("species_counts.R")

## Aquatics
source("aquatics/01_load.R")
source("aquatics/02_clean.R")
source("aquatics/03_analysis.R")
source("aquatics/04_output.R")

## Plants
source("plants/01_load.R")
source("plants/02_analysis.R")
source("plants/03_output.R")

## Print version
out_file <- "EnvReportBC_Invasive_Species_Sept2015.pdf"
rmarkdown::render("print_ver/invasives.Rmd", output_file = out_file)
extrafont::embed_fonts(file.path("print_ver", out_file))
message("Please open ", out_file, " in Acrobat Pro and Save As 'Optimized PDF' to reduce the file size")
