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

# @knitr plants-graphs

require(ggplot2) #plotting
require(extrafont) #plot font
require(envreportutils) #theme_soe
require(grid) #

source("fun.R") #functions from fun.R

dir.create("out", showWarnings = FALSE)

## Transforming shapefile contents for use in ggplot2 
bgc_simp@data$id <- rownames(bgc_simp@data)
biozones.data.points <- fortify(bgc_simp, region = "id")
biozones.data.points <- left_join(biozones.data.points, bgc_simp@data, by = "id")
biozones.data.df <- left_join(biozones.data.points, summary, by = c("MAP_LAB" = "BGC"))

## Replace 0 with NA for displaying zeros differently on the map. If doing this, 
## add na.value = "grey90" (or whatever) tp scale_fill_gradient call
# biozones.data.points$n_spp_s[biozones.data.points$n_spp_s == 0] <- NA
# biozones.data.points$n_sts_s[biozones.data.points$n_sts_s == 0] <- NA

## Plotting plant sites map

map_theme <-   theme(axis.title = element_blank(),
                     axis.text = element_blank(), 
                     axis.ticks = element_blank(),
                     panel.grid = element_blank(),
                     legend.position = c(.2, .2),
                     legend.title = element_text(size = 11, 
                                                 face = "bold"),
                     plot.margin = unit(c(0,0,20,0),"mm"), 
                     text = element_text(family = "Verdana"))

sitesmap <- ggplot(biozones.data.points) +
  aes(long, lat, group = group, fill = n_sts_s) +
  geom_polygon() +
  coord_fixed() + 
  theme_minimal() +
  scale_fill_gradient(name = "Number of Known Invasive\nPlant Locations",
                      low = "#f7fcb9", high = "#004529", 
                      guide = guide_colourbar(reverse = FALSE,
                                              barwidth = 1.5, barheight = 7,
                                              title.position = "bottom")) + 
  map_theme
#plot(sitesmap)

sppmap <- ggplot(biozones.data.points) +
  aes(long, lat, group = group, fill = n_spp_s) +
  geom_polygon() +
  coord_fixed() + 
  theme_minimal() +
  scale_fill_gradient(name = "Number of Established\nInvasive Plant Species",
                      low = "#f7fcb9", high = "#004529", 
                      guide = guide_colourbar(reverse = FALSE,
                                              barwidth = 1.5, barheight = 7,
                                              title.position = "bottom")) + 
  map_theme
#plot(sppmap)


## @knitr stop

## OUPUTS ----------------------------------------------------------------



## invasive plant chloropleth of number of occurences and number of speciesby BGC map 
png(filename = "./out/plant.viz.png", width = 900, height = 430, units = "px")
multiplot(sitesmap, sppmap, cols = 2, widths = c(1, 1))
dev.off()

## Crunching some more numbers for the indicator page

## number of unique species and frequency
speciesfreq <- table(species.long$species_id)
speciesfreq <- as.data.frame(speciesfreq)

