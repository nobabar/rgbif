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

library(rgbif)
library(raster)
library(maptools)
data("wrld_simpl")


# Hirundo ----
hirundo <- name_backbone(name = "Hirundo rustica",
                         rank = "species")
hirundo_search <- occ_search(scientificName = "Hirundo rustica",
                             hasCoordinate = TRUE)
hirundo_get <- occ_get(key = hirundo$speciesKey)


hirundo_locations <- hirundo_search$data
hirundo_occs <- SpatialPoints(coords = hirundo_locations[c("decimalLongitude", "decimalLatitude")],
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
plot(hirundo_occs, col = "red", pch = 17)
plot(wrld_simpl, add = TRUE)

hirundo_map <- map_fetch(taxonKey = hirundo$speciesKey, srs = "EPSG:3857")
plot(hirundo_map)

# Picus ----
picus <- name_backbone(name = "Picus viridis",
                       rank = "species")
picus_search <- occ_search(scientificName = "Picus viridis")
picus_get <- occ_get(key = Picus$speciesKey) # idk why it doesn't work

picus_locations <- picus_search$data
picus_occs <- SpatialPoints(coords = picus_locations[c("decimalLongitude", "decimalLatitude")],
                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
plot(picus_occs, col = "red", pch = 17)
plot(wrld_simpl, add = TRUE)

picus_map <- map_fetch(taxonKey = picus$speciesKey, srs = "EPSG:3857")
plot(picus_map)
