library(rgbif)
library(ggplot2)
library(RColorBrewer)  # for brewer.pal(...)

map <- map_data("france")

make_maps <- function(occs, year=""){
  locations <- as.data.frame(occs)
  locations <- locations[,c("acceptedScientificName","stateProvince", "year",
                            "recordedBy", "basisOfRecord",
                            "decimalLatitude","decimalLongitude")]
  
  ggplot(locations, aes(x=decimalLongitude, y=decimalLatitude)) + 
    stat_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon")+
    geom_path(data=map,aes(x=long, y=lat,group=group), colour="grey50")+
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))+
    coord_fixed() +
    ggtitle(paste("Heatmap de Hirundo rustica pour la décennie", year))
  
  ggsave(paste0("./maps/hirundo_heatmap_", year, ".png"))
}

# occs_1970 <- occ_search(scientificName="Hirundo rustica", limit=50000, country='FR', year="1970,1979", hasCoordinate=TRUE)
occs_1970 <- import("./data/Hirundo_rustica_1970/occurrence.txt", header=TRUE, sep="\t")
# occs_2010 <- occ_search(scientificName="Hirundo rustica", limit=50000, country='FR', year="2010,2019", hasCoordinate=TRUE)
occs_2010 <- import("./data/Hirundo_rustica_2010/occurrence.txt", header=TRUE, sep="\t")

make_maps(occs_1970$data, "1970")
make_maps(occs_2010$data, "2010")

# ----

library(tidyverse)

locations <- as.data.frame(occs_1970$data)

mean_locations <- locations %>%
  group_by(datasetKey) %>%
  summarise_at(vars("decimalLongitude", "decimalLatitude"), mean)

geom_sf(data=map) +
  geom_sf(data=mean_locations, color=rgb(0.2, 0.2, 0.2, alpha=0.4)) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                    colour = "#808080")
  )

library(maptools)
data("wrld_simpl")
SP <- SpatialPoints(coords=mean_locations[c("decimalLongitude","decimalLatitude")],
                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
plot(SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16)
plot(wrld_simpl, add=TRUE)

for (key in unique(locations$datasetKey)){
  SP <- SpatialPoints(coords=locations[locations$datasetKey==key,],
                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  mean_locations <- locations[locations$datasetKey==key,] %>%
    summarise_at(vars("decimalLongitude", "decimalLatitude"), mean)
  mean_SP <- SpatialPoints(coords=mean_locations,
                      proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  plot(SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16)
  plot(mean_SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16, add=TRUE)
  plot(wrld_simpl, add=TRUE)
}
