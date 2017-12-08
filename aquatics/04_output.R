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

# @knitr aquatics-graphs

library(ggplot2)
library(dplyr)
library(rphylopic) # For image on fish chart
library(grid) # for unit()
library(rgdal)
library(scales)
library(envreportutils)
library(maptools)
library(rgeos)


## Load simple EAUBC EDUs for plotting
eaubc_simp <- readOGR("aquatics/data", layer = "eaubc_simp", verbose = FALSE, stringsAsFactors = FALSE)


theme_map <- function() {
  theme_bw() +     
    theme(axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(), 
          plot.margin = unit(c(5, 0, 5, 0), "mm"), 
          strip.background = element_blank(), 
          strip.text = element_text(vjust = 0, hjust = 0.4, size = rel(1.1))
    )  
}

ais_prov_grp$tax_group <- with(ais_prov_grp, reorder(tax_group, n_spp, max, na.rm = TRUE))
chart_prov_summary <- ggplot(ais_prov_grp, aes(x = tax_group, y = n_spp)) + 
  geom_bar(stat = "identity", fill = "#08519C") + 
  labs(x = "Taxonomic Group", y = "Number of Species") + 
  coord_flip() + 
  theme_soe() + 
  theme(legend.position = "none")

ais_fwer_grp$fw_ecoreg <- with(ais_fwer_grp, reorder(fw_ecoreg, n_spp, max, na.rm = TRUE))
ais_fwer_grp$tax_group <- with(ais_fwer_grp, reorder(tax_group, n_spp, max, na.rm = TRUE))
chart_fwer_summary <- ggplot(ais_fwer_grp, aes(x = tax_group, y = n_spp)) + 
  facet_wrap(~fw_ecoreg) + 
  geom_bar(stat = "identity", aes(fill = tax_group)) + 
  labs(x = "Taxonomic Group", y = "Number of Species") + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = "none")

## Plot edus with number of invasive species
eaubc.df <- fortify(eaubc_simp, region = "EDU")
eaubc.df.all <- left_join(eaubc.df, ais_edu_grp, by = c("id" = "edu"))

chart_facet_map <- ggplot(eaubc.df.all, aes(long, lat, group = group, fill = n_spp)) + 
  facet_wrap(~tax_group, ncol = 3) +  
  geom_polygon() + 
  scale_fill_continuous(name = "Number of\nKnown Invasive\nAquatic Species", low = "#56B1F7", 
                        high = "#132B43", na.value = "grey81", 
                        breaks = pretty_breaks(6), 
                        guide = guide_colourbar(draw.llim = TRUE, 
                                                draw.ulim = TRUE)) +
  geom_path(color = "white", size = 0.5) + 
  coord_equal() + 
  theme_map() + 
  theme(legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, 
                                    face = "bold"))

eaubc_fish_decade.df <- left_join(eaubc.df, fish_by_edu_by_decade, by = c("id" = "edu"))

chart_year_map <- ggplot(eaubc_fish_decade.df, aes(long, lat, group = group, fill = n_species)) + 
  facet_wrap(~year, ncol = 4) +  
  geom_polygon() + 
  scale_fill_continuous(name = "Number of\nKnown Invasive\nFreshwater\nFish Species", low = "#56B1F7", 
                        high = "#132B43", na.value = "grey80", 
                        breaks = pretty_breaks(6), 
                        guide = guide_colourbar(draw.llim = TRUE, 
                                                draw.ulim = TRUE)) +
  geom_path(color = "white", size = 0.5) + 
  ggtitle("Increase in Number and Distribution of Invasive Freshwater Fish (1900-2014)") + 
  coord_equal() + 
  theme_map() + 
  theme(legend.key.height = unit(2, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, 
                                    face = "bold"))


## Plot increase in fish spp over time
ais_year_edu$edu <- with(ais_year_edu, reorder(edu, n_species, max))
ais_year_edu$edu <- with(ais_year_edu, 
                         factor(edu, levels = rev(levels(edu))))

ais_year_fwer$fw_ecoreg <- with(ais_year_fwer, reorder(fw_ecoreg, n_species, max))
ais_year_fwer$fw_ecoreg <- with(ais_year_fwer, 
                                factor(fw_ecoreg, levels = rev(levels(fw_ecoreg))))

chart_edu_fish_trend <- ais_year_edu %>% filter(tax_group == "Fish") %>% 
  ggplot(aes(x = year, y = n_species)) + 
  facet_wrap(~edu) + 
  geom_point(colour = "#08519c", size = 1) + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(x = "Year", y = "Number of Invasive Freshwater Fish Species Detected") + 
  theme_bw()

chart_fwer_fish_trend <- ais_year_fwer %>% filter(tax_group == "Fish") %>% 
  ggplot(aes(x = year, y = n_species)) + 
  facet_wrap(~fw_ecoreg) + 
  geom_line(colour = "#08519c", size = 2) + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(x = "Year", y = "Number of Invasive Freshwater Fish Species Detected") + 
  theme_bw()

## Get sillhoutte of Black Bullhead (Ameiurus melas) from Phylopic:
## http://phylopic.org/image/7a6448e5-09c4-40c8-8378-599d7f974bfe/
black_bullhead_img <- image_data("7a6448e5-09c4-40c8-8378-599d7f974bfe", size = "512")[[1]]

chart_prov_fish_trend <- ais_year_prov %>% filter(tax_group == "Fish") %>% 
  ggplot(aes(x = year, y = n_species)) + 
  geom_line(colour = "#08519c", size = 1.5) + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(x = "Year", y = "Number of Known Invasive\nFreshwater Fish Species\n") + 
  annotate("text", x = 1910, y = 28, hjust = 0, vjust = 1, size = 5, 
           label = "The number of invasive freshwater fish species\ndetected in B.C. has increased steadily\nsince the early 1900s") +
  theme_soe() + 
  theme(panel.grid.major = element_blank(), axis.title.y = element_text(vjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  add_phylopic(black_bullhead_img, ysize = 25, x = 1985, y = 9)

# @knitr stop

# png("out/ais_edu_facet_map.png", width = 900, units = "px")
# chart_facet_map
# dev.off()
# 
# png("out/ais_fish_trend.png", width = 600, height = 400, units = "px", type = "cairo-png")
# chart_prov_fish_trend
# dev.off()
# 
# png("out/ais_fish_year_facet_map.png", width = 900, height = 600, units = "px")
# chart_year_map
# dev.off()

png_retina("out/ais_edu_facet_map.png", width = 900, units = "px")
chart_facet_map
dev.off()

png_retina("out/ais_fish_trend.png", width = 600, height = 400, units = "px", type = "cairo-png")
chart_prov_fish_trend
dev.off()

png_retina("out/ais_fish_year_facet_map.png", width = 900, height = 600, units = "px")
chart_year_map
dev.off()


