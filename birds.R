# récupération de données d'oiseaux sur la BDD GBIF
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
Hirundo <- name_backbone(name="Hirundo rustica",
                         rank="species")
Hirundo.search <- occ_search(scientificName="Hirundo rustica",
                             hasCoordinate=TRUE)
Hirundo.get <- occ_get(key=Hirundo$speciesKey)


Hirundo.locations <- Hirundo.search$data
Hirundo.occs <- SpatialPoints(coords = Hirundo.locations[c("decimalLongitude","decimalLatitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
plot(Hirundo.occs, col="red", pch=17)
plot(wrld_simpl, add=TRUE)

Hirundo.map <- map_fetch(taxonKey=Hirundo$speciesKey, srs = "EPSG:3857")
plot(Hirundo.map)

# Picus ----
Picus <- name_backbone(name="Picus viridis",
                       rank="species")
Picus.search <- occ_search(scientificName="Picus viridis")
# Picus.get <- occ_get(key=Picus$speciesKey) # idk why it doesn't work

Picus.locations <- Picus.search$data
Picus.occs <- SpatialPoints(coords = Picus.locations[c("decimalLongitude","decimalLatitude")],
                              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
plot(Picus.occs, col="red", pch=17)
plot(wrld_simpl, add=TRUE)

Picus.map <- map_fetch(taxonKey=Picus$speciesKey, srs = "EPSG:3857")
plot(Picus.map)

