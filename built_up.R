library(raster)
library(dplyr)

france <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_0.shp")

built_up_1970 <- raster("./data/built_up_land/1970/GHS_BUILT_LDS1975_GLOBE_R2018A_54009_1K_V2_0.tif") %>%
  crop(., spTransform(france, crs(.))) %>%
  projectRaster(crs="+proj=longlat +datum=WGS84 +no_defs") %>%
  crop(bbox(france)) %>%
  mask(france) %>%
  writeRaster("./data/built_up_land/1970/GHS_BUILT_1975_FRANCE.tif",
              overwrite=TRUE,
              format = "GTiff",
              options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))

built_up_1970_france <- raster("./data/built_up_land/1970/GHS_BUILT_1975_FRANCE.tif")

built_up_2010 <- raster("./data/built_up_land/2010/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_1K_V2_0.tif") %>%
  crop(., spTransform(france, crs(.))) %>%
  projectRaster(crs="+proj=longlat +datum=WGS84 +no_defs") %>%
  crop(bbox(france)) %>%
  mask(france) %>%
  writeRaster("./data/built_up_land/2010/GHS_BUILT_2014_FRANCE.tif",
              overwrite=TRUE,
              format = "GTiff",
              options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))

built_up_2010_france <- raster("./data/built_up_land/2010/GHS_BUILT_2014_FRANCE.tif")

# 18th century roads and cities data ----

# library(cowplot)

# edge <- shapefile("./data/cassini_18th/edge/edge.shp")
# face <- shapefile("./data/cassini_18th/face/face.shp")
# cities <- shapefile("./data/cassini_18th/france_cassini_cities/france_cassini_cities.shp")
# roads <- shapefile("./data/cassini_18th/france_cassini_roads/france_cassini_roads.shp")
# 
# edge_plot <- ggplot() + geom_polygon(data = edge, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# face_plot <- ggplot() + geom_polygon(data = face, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# cities_plot <- ggplot() + geom_polygon(data = cities, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# roads_plot <- ggplot() + geom_polygon(data = roads, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# 
# plot_grid(edge_plot, face_plot, cities_plot, roads_plot)


settle_1970 <- raster("./data/settlement/1970/GHS_SMOD_POP1975_GLOBE_R2019A_54009_1K_V2_0.tif") %>%
  crop(., spTransform(france, crs(.))) %>%
  projectRaster(crs="+proj=longlat +datum=WGS84 +no_defs") %>%
  crop(bbox(france)) %>%
  mask(france) %>%
  writeRaster("./data/settlement/1970/GHS_SMOD_1970_FRANCE.tif",
              overwrite=TRUE,
              format = "GTiff",
              options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))

settle_2000 <- raster("./data/settlement/2000/GHS_SMOD_POP2000_GLOBE_R2019A_54009_1K_V2_0.tif") %>%
  crop(., spTransform(france, crs(.))) %>%
  projectRaster(crs="+proj=longlat +datum=WGS84 +no_defs") %>%
  crop(bbox(france)) %>%
  mask(france) %>%
  writeRaster("./data/settlement/2000/GHS_SMOD_2000_FRANCE.tif",
              overwrite=TRUE,
              format = "GTiff",
              options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))

settle_2010 <- raster("./data/settlement/2010/GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0.tif") %>%
  crop(., spTransform(france, crs(.))) %>%
  projectRaster(crs="+proj=longlat +datum=WGS84 +no_defs") %>%
  crop(bbox(france)) %>%
  mask(france) %>%
  writeRaster("./data/settlement/2010/GHS_SMOD_2010_FRANCE.tif",
              overwrite=TRUE,
              format = "GTiff",
              options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))
