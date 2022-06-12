library(sf)
library(raster)


rivers_shape <- list.files("./data/rivers/rivers_shape/", pattern = "gpkg$",
                           recursive = TRUE, full.names = TRUE)

data <- st_read("./data/rivers/rivers_shape/euhydro_duero_v013_GPKG/euhydro_duero_v013.gpkg",
                query='SELECT OBJECT_ID, AREA_GEO, Shape FROM River_Net_p')
plot(st_zm(data$Shape))


layers <- st_layers("./data/rivers/rivers_shape/euhydro_duero_v013_GPKG/euhydro_duero_v013.gpkg")

data.spatial <- as(st_zm(data), 'Spatial')

data.sf <- st_as_sf(data.spatial, "sf")

riparian_files_2012 <- list.files("./data/rivers/riparian_zones/2012/", pattern = "shp$",
                                  recursive = TRUE, full.names = TRUE)

riparian_zones_2012 <- rbind(lapply(riparian_files_2012, st_read))

riparian_longlat <- st_transform(riparian_zones_2012,
                                 "+proj=longlat +datum=WGS84 +no_defs")

ext <- extent(riparian_longlat)
rr <- raster(ext, res=0.005)
rr <- rasterize(riparian_longlat, rr, field=riparian_longlat$CODE_1_12)
