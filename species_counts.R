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

## R script to clean, analyze and plot BCSEE data on exotic species 

## loading libraries for script
library(dplyr) #data munging
library(magrittr) #pipe
library(ggplot2) #plotting
library(envreportutils) #multiplot function
library(rphylopic) #for black rabbit image 

## do not turn all strings in csv files into factors
options(stringsAsFactors=FALSE)


## reading in data file

## @knitr species-counts-pre

## CSV file downloaded from BC Data Catalogue
## https://catalogue.data.gov.bc.ca/dataset/bc-species-and-ecosystems-conservation-status-information 
## Accessed 22 October 2014
## Data provided under the Open Government Licence - BC
## http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc

exotics <- read.csv("https://catalogue.data.gov.bc.ca/dataset/d3651b8c-f560-48f7-a34e-26b0afc77d84/resource/39aa3eb8-da10-49c5-8230-a3b5fd0006a9/download/bcseeplantsanimals.csv",
                    header=TRUE, sep=",", strip.white=TRUE, 
                    na.string=c("-","","NA"), stringsAsFactors=FALSE)



## Subsetting the data 


## subsetting 2014 year and exotics data
exotics2014 <- exotics %>%
  filter(YEAR == 2014) %>%
  filter(Origin == "Exotic")

## subsetting columns, removing slender hawksbeard error, correcting Kingdom
## for fungus record and adding class english name for fungus
exotics.subset <- exotics2014 %>%
  select(Species.Level, Scientific.Name, English.Name, BC.List, Origin, Name.Category, Class..English.,
         Kingdom, Phylum, Class, Order, Family) %>%
  filter(English.Name != "slender hawksbeard" | is.na(English.Name)) ##not needed with updated data
exotics.subset$Class..English.[exotics.subset$Name.Category == "Fungus"] <- "fungus"
exotics.subset$Kingdom[exotics.subset$Name.Category == "Fungus"] <- "Fungi"

## Kingdom group subsets and creating/grouping common names for graphs
exotics.subset.plants <- exotics.subset %>%
  filter(Kingdom == "Plantae")
exotics.subset.plants$Graph.Name[exotics.subset.plants$Kingdom == "Plantae"] <- "All Plants"


exotics.subset.fungus <- exotics.subset %>%
  filter(Kingdom == "Fungi")
exotics.subset.fungus$Graph.Name[exotics.subset.fungus$Kingdom == "Fungi"] <- "Fungus"


exotics.subset.animals <- exotics.subset %>%
  filter(Kingdom == "Animalia")
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English %in% c("amphibians", "turtles", "reptiles")] <- "Amphibians\n& Reptiles"
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English %in% c("arachnids", "insects")] <- "Select Insects\n& Spiders"
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English == "birds"] <- "Birds"
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English %in% c("bivalves", "gastropods")] <- "Molluscs"
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English == "mammals"] <- "Mammals"
exotics.subset.animals$Graph.Name[exotics.subset.animals$Class..English == "ray-finned fishes"] <- "Freshwater\nFish"

## Creating factor ordered list for ordering bar chart
exotics.subset.animals$Graph.Name <- factor(exotics.subset.animals$Graph.Name,
                                            c("Select Insects\n& Spiders", "Molluscs",
                                              "Freshwater\nFish",
                                              "Birds","Mammals", "Amphibians\n& Reptiles"))

## Creating final file/table output of all species included in the summary
exotics.final.species <- rbind(exotics.subset.fungus, exotics.subset.animals,
                               exotics.subset.plants)
colnames(exotics.final.species)[colnames(exotics.final.species) == "Class..English."] <- "Class.English"


## Species Group Totals for direct labelling the bar charts 
animal.totals <- exotics.subset.animals %>%
  group_by(Graph.Name, Name.Category)%>%
  summarise(count = n())

plant.totals <- exotics.subset.plants %>%
  group_by(Kingdom)%>%
  summarise(count = n())

fungus.totals <- exotics.subset.fungus %>%
  group_by(Kingdom)%>%
  summarise(count = n())


## @knitr alien.totals


## Individual and Multiplot PLOTS


## plants bar chart
plantPal <- "#4daf4a"

exotics.plants.plot <- ggplot(data=exotics.subset.plants,
                              aes(x = Kingdom, fill = Kingdom)) + 
  geom_bar(stat= "count", alpha=.7) +
  geom_text(data=plant.totals, aes(y=count, label=count, vjust=-.5)) +
  ggtitle("Plants") +
  ylab ("Number of Species") +
  labs(caption = "\n\n") +
  scale_y_continuous(limits = c(0,1000), breaks=seq(0, 1000, 200),
                     expand=c(0,0)) +
  scale_fill_manual(values= plantPal, guide = FALSE) +
  scale_x_discrete(labels="All Plants\n") +
  theme_soe() +
  theme(panel.grid.major.x = (element_blank()),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = .5),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank())

## fungus bar chart
fungusPal <- "#ff7f00"

exotics.fungus.plot <- ggplot(data=exotics.subset.fungus,
                              aes(x = Kingdom, fill = Kingdom)) + 
  geom_bar(stat= "count", alpha=.7) +
  geom_text(data=fungus.totals, aes(y=count, label=count, vjust=-.5)) +
  ggtitle("Fungi") + 
  labs(caption = "\n\n") +
  scale_y_continuous(limits = c(0,10), breaks=seq(0, 10, 2),
                     expand=c(0,0)) +
  scale_fill_manual(values=fungusPal, guide = FALSE) +
  scale_x_discrete(labels="Fungus\n") +
  theme_soe() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = .5),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank())


## animals bar chart
animalPal <- c("#377eb8","#984ea3")

## Get sillhoutte of Black European Rabbit (Oryctolagus cuniculus) and 
## Black Cricket (Acheta) from Phylopic:
## http://phylopic.org/image/1e15411c-5394-4a9d-a209-76c8ac0c331d/
## http://phylopic.org/image/b80d830b-155a-4ca5-9119-9a9fde019cc6/

black_rabbit_img <- image_data("1e15411c-5394-4a9d-a209-76c8ac0c331d",
                               size = "512")[[1]]

black_cricket_img <- image_data("b80d830b-155a-4ca5-9119-9a9fde019cc6",
                                size = "512")[[1]]

exotics.animals.plot <- ggplot(data=exotics.subset.animals,
                               aes(x = Graph.Name, fill = Name.Category)) + 
  geom_bar(stat= "count", alpha=.7) +
  geom_text(data=animal.totals, aes(y=count, label=count, vjust=-.5)) +
  ggtitle("Animals") + 
  labs(caption = "\n*Note the different y-axis scales for the plants, animals and fungi charts above") + 
  scale_y_continuous(limits = c(0,100), breaks=seq(0, 100, 20),
                     expand=c(0,0)) +
  scale_fill_manual(values=animalPal,
                    labels=c("Invertebrates",
                             "Vertebrates")) +
  theme_soe() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = .5),
        plot.caption = element_text(size = 14),
        legend.text = element_text(size = 14),                         
        legend.position=c(.5,.9),
        axis.title = element_blank(),
        legend.title=element_blank(),
        legend.background = element_rect(fill="transparent"),
        panel.grid.major.x = element_blank(),
        legend.direction = ("horizontal")) +
  add_phylopic(black_rabbit_img, alpha = .7, color = "#984ea3",
               ysize = 20, x = 4.9, y = 28) +
  add_phylopic(black_cricket_img, alpha = .7, color = "#377eb8",
               ysize = 25, x = 1, y = 70) 
#plot(exotics.animals.plot)

## multiplot with all 3 bar charts
myplots <- list(exotics.plants.plot, exotics.animals.plot, exotics.fungus.plot)
multiplot(plotlist = myplots, cols=3,
          widths = c(.54, 2, .44), title ="")

## @knitr stop

## Printing multiplot

dir.create('out', showWarnings = FALSE)

png(filename = "./out/aliens.png", width=836, height=430, units="px", type = "cairo-png")
multiplot(plotlist = myplots, cols=3, widths = c(.5, 2, .44))
dev.off()

svg_px(file = "./out/aliens.svg", width=836, height=430)
multiplot(plotlist = myplots, cols=3, widths = c(.5, 2, .44))
dev.off()

