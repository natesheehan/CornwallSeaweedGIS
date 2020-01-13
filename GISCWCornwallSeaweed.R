# Ensure strings come in as character types
options(stringsAsFactors = FALSE)

# Install packages and load the following packages
install.packages("ggspatial")
library("ggspatial")
library(tidyverse)
library(sf)

#Read in Cornwall census data shape file and CSV
cornwallCensus <- st_read("~/Downloads/Cornwall/Shapefiles/Cornwall_lsoa11.shp")
cornwallCensusPopulationCsv <- read_csv("~/Downloads/Cornwall/Tables/QS102EW_LSOA11.csv") 
cornwallCensusProfessionsCsv <- read_csv("~/Downloads/Cornwall/Tables/KS606EW_LSOA11.csv") 

colnames(cornwallCensusPopulationCsv)[colnames(cornwallCensusPopulationCsv)=="pop"] <- "Population"
colnames(cornwallCensusProfessionsCsv)[colnames(cornwallCensusProfessionsCsv)=="GeographyCode"] <- "LSOA11CD"

#Merge Cornwall Census shapefile and CSV
cornwallCensusMerge <- merge(cornwallCensus,cornwallCensusPopulationCsv, by='LSOA11CD')
cornwallCensusMerge <- merge(cornwallCensusMerge,cornwallCensusProfessionsCsv, by='LSOA11CD')

#Read multicriteria shapefiles
tidalBoundry <- st_read("~/Downloads/E06000052/TidalBoundary.shp")
railway <- st_read("~/Downloads/E06000052/RailwayStation.shp")
roads <- st_read("~/Downloads/E06000052/road.shp")

colnames(cornwallCensusMerge)[colnames(cornwallCensusMerge)=="pop"] <- "Population"
colnames(cornwallCensusMerge)[colnames(cornwallCensusMerge)=="KS606EW0032"] <- "ScientificJob"

#plot population study area
ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = cornwallCensusMerge, aes(fill = Population))

#plot railway boundry of 5 of a mile
railwayBuffer <- 
  railway %>%
  st_buffer(8040)

ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=cornwallCensusMerge,fill= "grey40") +
  geom_sf(data=railwayBuffer, fill= NA, colour="black", size=2) +
  geom_sf(data=railway,fill= "dodgerblue4") 

#plot tidal boundry 1/4 miles away
tidalBoundryBuffer <-
  tidalBoundry %>%
  st_buffer(402)

ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=cornwallCensusMerge,fill= "grey40") +
  geom_sf(data=tidalBoundryBuffer, fill= NA, colour="darkgreen", size=2) +
  geom_sf(data=tidalBoundry,fill= "green")

#plot next to road
ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=cornwallCensusMerge,fill= "grey40") +
  geom_sf(data=cornwallCensusMerge[st_buffer(roads,50),],fill= "cyan1") +
  geom_sf(data=roads, colour="red", size=1)

roadsBuffer <- st_buffer(roads,50)

#population density and hectares of land
attributeSelect <-
  cornwallCensusMerge %>%
  mutate(acres = as.integer(hect) * 0.000247105) %>%
  filter(
    as.integer(hect) > 20 &
      pop_dens < median(pop_dens) &
      ScientificJob > median(ScientificJob)
    
  )

ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=cornwallCensusMerge,fill= "grey40") +
  geom_sf(data=cornwallCensusMerge[st_buffer(attributeSelect,50),],fill= "cyan1") +
  geom_sf(data=attributeSelect, colour="red", size=1)

roadsBuffer <- st_buffer(attributeSelect,50)

plot(cornwallCensusMerge[,1], col="black")
plot(attributeSelect, col="red", add=T)

#Final plotting 
dissolve <- 
  tidalBoundryBuffer %>% 
  group_by(classifica) %>% 
  summarize(first(classifica))

plot(dissolve[,1])

thoseBlocks <- attributeSelect[railwayBuffer, ]

thoseBlocks_Not_in_TidalBuffer <- st_disjoint(dissolve,thoseBlocks,sparse=F)
finalSelection <- thoseBlocks[thoseBlocks_Not_in_TidalBuffer, ]
plot(finalSelection[,1])

ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=finalSelection, fill="cyan1") +
  geom_sf(data=tidalBoundryBuffer,fill = NA, color= "black", size=1) +
  geom_sf(data=cornwallCensusMerge,fill= "grey40") +
  geom_sf(data=railwayBuffer,fill= "green4") +
  geom_sf(data=roadsBuffer, fill="red")

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

tide <- 
  tidalBoundry %>%
  select(geometry) %>%
  mutate(Legend = "Tidal buffer") 

tidalBoundryBuffer2 <- 
  tidalBoundry %>%
  select(geometry) %>%
  mutate(Legend = "Sea")

allTides <- rbind(tide,tidalBoundryBuffer2)

tidalDecisionFactor <- 
  ggplot() +
  geom_sf(data=cornwallCensusMerge,fill= "lightgreen", colour="lightgreen") +
  geom_sf(data=allTides, aes(fill= Legend, colour=Legend),size=1) +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values = c(NA,"darkblue")) +
  scale_colour_manual(values = c("darkblue",NA)) +
  labs(title="DF1: 1/4mi. Away From Sea Tide") +
  mapTheme() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

tidalDecisionFactor

ggsave("tidalDecisionFactor.png")

railwayBoundary2 <- 
  railway %>%
  select(geometry) %>%
  mutate(Legend = "5 mi. Railway Buffer") 

railwayBuffer2 <- 
  railwayBuffer %>%
  select(geometry) %>%
  mutate(Legend = "5 mi. Railway Buffer")

allRailways <- rbind(railwayBoundary2,railwayBuffer2)

railwayDecisionFactor <- 
  ggplot() +
  geom_sf(data=cornwallCensusMerge,fill= "lightgreen", colour="lightgreen") +
  geom_sf(data=allRailways, aes(fill= Legend, colour=Legend),size=1) +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values = c(NA,"black")) +
  scale_colour_manual(values = c("black",NA)) +
  labs(title="DF2: 5mi from Railways Stations") +
  mapTheme() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

railwayDecisionFactor

ggsave("railwayDecisionFactor.png")

roadBuffer2 <- 
  roadsBuffer %>%
  select(geometry) %>%
  mutate(Legend = "Roads")

roadsDecisionFactor <- 
  ggplot() +
  geom_sf(data=cornwallCensusMerge,fill= "lightgreen", colour="lightgreen") +
  geom_sf(data=roadBuffer2, aes(fill=Legend), colour="red") +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values = c("red")) +
  labs(title="DF 3: Adjacent to Roads") +
  mapTheme() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

roadsDecisionFactor

ggsave("roadsDecisionFactor.png")

attributeSelect2 <- 
  attributeSelect %>%
  select(geometry) %>%
  mutate(Legend = "Suitable Areas")

attributesDecisionFactor <- 
  ggplot() +
  geom_sf(data=cornwallCensusMerge,fill= "lightgreen", colour="lightgreen") +
  geom_sf(data=attributeSelect2, aes(fill=Legend), colour="mediumpurple4") +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_fill_manual(values = c("mediumpurple4")) +
  labs(title="DF 4A-D: Area, Population Density & Skills") +
  mapTheme() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

attributesDecisionFactor

ggsave("attributesDecisionFactor.png")

finalSelection2 <-
  finalSelection %>%
  select(geometry) %>%
  mutate(Legend = "Suitable Site")

combine <- rbind(finalSelection2,roadBuffer2, tide, railwayBoundary2 )

combine$Legend <- factor(combine$Legend, levels = c("Tide", "Railways", "Roads", "Suitable Site"))

combinationPlot <- 
  ggplot() +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data=cornwallCensusMerge,fill= "lightgreen", colour="lightgreen") +
  geom_sf(data=combine, aes(fill=Legend), colour="black") +
  scale_fill_manual(values = c("darkblue","dodgerblue4","red","cyan1")) +
  labs(title="Final Suitability Map") +
  theme(
    plot.title = element_text(size = 20,colour = "black",hjust=0.5),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    legend.position="bottom",
    legend.title=element_blank()
  ) 

combinationPlot

library(grid)
library(gridExtra)

gs <- list(tidalDecisionFactor, 
           railwayDecisionFactor, 
           roadsDecisionFactor,
           attributesDecisionFactor,
           combinationPlot)

lay2 <- rbind(c(1,2,5,5,5),
              c(1,2,5,5,5),
              c(3,4,5,5,5),
              c(3,4,5,5,5))

grid.arrange(grobs = gs, layout_matrix = lay2,
             top=textGrob("Seaweed farming business location in Cornwall, PA\n",gp=gpar(fontsize=30,fontface="bold")),
             bottom=textGrob("This map shows the result of a site suitability analysis. The four maps on the left show the 'decision factors' used to calculate\nthe final site suitability map on the right.",gp=gpar(fontsize=9,fontface="bold")))
