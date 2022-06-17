library(sf)
library(raster)

# river shape ----
rivers_shape <- list.files("./data/rivers/rivers_shape/", pattern = "gpkg$",
                           recursive = TRUE, full.names = TRUE)

# read only Loire
river_p <- st_read("./data/rivers/rivers_shape//euhydro_loire_v013_GPKG/euhydro_loire_v013.gpkg",
                   query='SELECT OBJECT_ID, AREA_GEO, Shape FROM River_Net_p')

plot(st_zm(river_p$Shape))

layers <- st_layers("./data/rivers/rivers_shape/euhydro_duero_v013_GPKG/euhydro_duero_v013.gpkg")
data.spatial <- as(st_zm(river_p), 'Spatial')
data.sf <- st_as_sf(data.spatial, "sf")


# riparian zones ----
france_tiles <- raster("./data/rivers/riparian_zones/france_tiles.tif")
tiles_data <- france_tiles %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()

interest_rivers = c("Loire"="rpz_DU017A", "Ebro"="rpz_DU019A")

riparian_zone_stats <- list.files("./data/rivers/riparian_zones", pattern = "gpkg$",
                                  recursive = TRUE, full.names = TRUE)

# only select Loire
riparian_zone_stats <- grep(interest_rivers["Loire"],
                            riparian_zone_stats,
                            value = TRUE)

for (riparian_zone in riparian_zone_stats){
  stats <- st_read(riparian_zone)
  stats <- stats[order(stats$zone),]
  
  tdata <- tiles_data[,2:3]
  tdata$value[tiles_data$france_tiles %in% stats$zone] <- stats$sum
  
  rdf <- rasterFromXYZ(tdata)
  crs(rdf) <- crs(france_tiles)
  
  path <- gsub("(/[^/]*){3}$", "",
               sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", riparian_zone))
  
  filename <- gsub("(_[^_]*){2}$", "",
                   sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", riparian_zone))
  
  zone <- sub("(^([^_]*_){2})(.+)(_stats)", "\\3",
               sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", riparian_zone))
  
  river <- names(interest_rivers[interest_rivers==filename])
  
  writeRaster(rdf,
              paste0(path, "/", river, "_", zone, ".tif"),
              overwrite=TRUE,
              format = "GTiff")
}



# create simulacrae layer ----
zfile <- raster("./data/BioClim/2000/bio1.tif")
zdata <- zfile %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()

zdata$bio1 <- NULL
zdata$value <- seq_len(nrow(zdata))

rdf <- rasterFromXYZ(zdata)
crs(rdf) <- crs(zfile)
writeRaster(rdf, "./data/rivers/riparian_zones/france_tiles.tif",
            overwrite=TRUE,
            format = "GTiff")
