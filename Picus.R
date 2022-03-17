rm(list=ls())

library(rgbif)
library(raster)
library(ggplot2)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

world <- ne_countries(scale = "medium", returnclass = "sf")


occs <- occ_search(scientificName="Picus viridis", limit=5000) # might reduce limit

locations <- as.data.frame(occs$data)
locations <- locations[,c("acceptedScientificName","stateProvince", "year",
                          "recordedBy", "basisOfRecord", "references",
                          "decimalLatitude","decimalLongitude")]

loc_sf = st_as_sf(locations,
                  coords = c("decimalLongitude","decimalLatitude"), 
                  crs = 4326)

ggplot() + 
  geom_sf(data=world) +
  geom_sf(data=loc_sf, color=rgb(0.2, 0.2, 0.2, alpha=0.4)) +
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70)) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                    colour = "#808080")
  )




