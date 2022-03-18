rm(list=ls())

library(rgbif)
library(raster)
library(ggplot2)
library(sf)
library(RColorBrewer)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- ne_countries(continent="europe", scale = "medium", returnclass = "sf")

make_maps <- function(occs, year=""){
  locations <- as.data.frame(occs)
  locations <- locations[,c("acceptedScientificName","stateProvince", "year",
                            "recordedBy", "basisOfRecord", "references",
                            "decimalLatitude","decimalLongitude")]
  
  loc_sf = st_as_sf(locations,
                    coords = c("decimalLongitude","decimalLatitude"), 
                    crs = 4326)
  
  # ggplot() + 
  #   geom_sf(data=world) +
  #   geom_sf(data=loc_sf, color=rgb(0.2, 0.2, 0.2, alpha=0.4)) +
  #   coord_sf(xlim = c(-25, 50), ylim = c(35, 70)) +
  #   annotation_scale(location = "bl", width_hint = 0.1) +
  #   theme(
  #     panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
  #                                     size = 1, linetype = "solid"),
  #     panel.grid.major = element_line(size = 0.2, linetype = "solid",
  #                                     colour = "#808080")
  #   ) +
  #   ggtitle(paste("Occurences de Picus viridis pour l'année", year))
  # 
  # ggsave(paste0("./maps/picus_occurences_", year, ".png"))
  
  ggplot(world) + 
    geom_sf(data=world) +
    stat_density2d(data=locations,
                   aes(x=decimalLongitude, y=decimalLatitude, fill=..level..),
                   geom='polygon', alpha=.1, bins=1000) +
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
    coord_sf(xlim = c(-25, 50), ylim = c(35, 70)) +
    annotation_scale(location = "bl", width_hint = 0.1) +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080")
    ) +
    ggtitle(paste("Heatmap de Picus viridis pour l'année", year))
  
  ggsave(paste0("./maps/picus_heatmap_", year, ".png"))
}

occs_recent <- occ_search(scientificName="Picus viridis", limit=5000) # might reduce limit

occs_1970 <- occ_search(scientificName="Picus viridis", limit=5000, year="1970", hasCoordinate=TRUE)
occs_2010 <- occ_search(scientificName="Picus viridis", limit=5000, year="2010", hasCoordinate=TRUE)

make_maps(occs_1970$data, "1970")
make_maps(occs_2010$data, "2010")

