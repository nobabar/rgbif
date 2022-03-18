# recuperation de donnees d'oiseaux sur la BDD GBIF
#
# class: birds
# - Animalia                                        # Kingdom
#   - Chordata                                      # Phylum
#     - Aves                                        # Class
#
# specie: hirondelle rustique - Barn swallow
# - Passeriformes                                   # Order
#   - Hirundinidae                                  # Family
#     - Hirundo                                     # Genus
#       - Hirundo rustica
#
# specie: pivert - European green woodpecker
# - Piciformes                                      # Order
#   - Picidae                                       # Family
#     - Picus                                       # Genus
#       - Picus viridis


rm(list=ls())

library(tidyverse)
library(rgbif)
library(raster)
library(ggplot2)
library(sf)

library(maptools)
data("wrld_simpl")

get_birds <- function(specie_name, plot_occ=FALSE, plot_heat=FALSE){
  occs <- occ_search(scientificName=specie_name, limit=5000, hasCoordinate=TRUE)
  if (plot_occ){
    plot_occurences(occs)
  }
  
  if (plot_heat){
    plot_heatmap(specie_name) 
  }
  
  return(occs$data)
}


# plot occurence points
plot_occurences <- function(occs){
  locations <- as.data.frame(occs$data)
  locations <- locations[,c("acceptedScientificName","stateProvince", "year",
                            "recordedBy", "basisOfRecord", "references",
                            "decimalLatitude","decimalLongitude")]
  occs <- SpatialPoints(coords=locations[c("decimalLongitude","decimalLatitude")],
                        proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  plot(occs, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16)
  plot(wrld_simpl, add=TRUE)
}

# plot heatmap
plot_heatmap <- function(specie_name){
  specie_key <- name_backbone(name=specie_name, rank="species")$speciesKey
  
  map <- map_fetch(taxonKey=specie_key,
                   format="@4x.png",
                   srs="EPSG:3857",       # SRS for whole world, 3575 for Europe
                   bin="hex",
                   hexPerTile=60,
                   style="purpleYellow.poly")
  plot(map)
}


Hirundo.occs <- get_birds("Hirundo rustica", T, T)
Picus.occs <- get_birds("Picus viridis", T, T)

Picus.occs %>% count(stateProvince, sort = TRUE) %>% drop_na(stateProvince)  %>% filter(n > 50) %>%
  ggplot(aes(x = reorder(stateProvince, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Country of Organism", y = "Number of Occurrence") + 
  coord_flip()

# Picus.occs %>% drop_na(year) %>% count(year) %>%      # un peu osef
#   ggplot(aes(x = year, y = n, group = 1)) +
#   geom_line()


