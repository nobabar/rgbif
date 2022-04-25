library(raster)
library(dplyr)

france <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_0.shp")

# processing WorldClim data ----

input_dir <- "./data/WorldClim"
output_dir <- "./data/WorldClim_France"
avg_dir <- "./data/WorldClim_avg"

files_1970 <- list.files(paste0(input_dir, "/1970"),
                         pattern = ".tif$",
                         full.names = TRUE,
                         recursive = TRUE)


# precipitation for first month of 1970
precip_raster <- raster(grep(files_1970, pattern = "_prec_", value = TRUE)[1])
precip_raster <- crop(precip_raster, france)
precip_raster <- mask(precipRaster, france)

plot(precipRaster, legend = FALSE)


# preprocess all rasters ----

for (i in seq_len(length(files_1970))) {
  raster(files_1970[i]) %>%
    crop(france) %>%
    mask(france) %>%
    aggregate(fact = 2) %>%
    writeRaster(filename = sub("^(.*[/])",
                             paste0(output_dir, "/1970/"),
                             files_1970[i]),
                format = "GTiff",
                overwrite = TRUE,
                options = c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))
}


# averaging ----

tmin_stack_1970 <- grep("tmin", list.files(paste0(output_dir, "/1970/"),
                                           pattern = "tif$",
                                           full.names = TRUE),
                        value=TRUE) %>% stack()

tmax_stack_1970 <- grep("tmax", list.files(paste0(output_dir, "/1970/"),
                                           pattern = "tif$",
                                           full.names = TRUE),
                        value=TRUE) %>% stack()

prec_stack_1970 <- grep("prec", list.files(paste0(output_dir, "/1970/"),
                                           pattern = "tif$",
                                           full.names = TRUE),
                        value=TRUE) %>% stack()

tmin_mean_1970 <- calc(tmin_stack_1970, mean)
tmax_mean_1970 <- calc(tmax_stack_1970, mean)
tmean_1970 <- (tmin_mean_1970 + tmax_mean_1970) / 2
prec_mean_1970 <- calc(prec_stack_1970, mean)


writeRaster(tmin_mean_1970,
            filename = paste0(avg_dir, "/1970/avg_tmin_1970.tif"),
            format = "GTiff",
            overwrite = TRUE,
            options = c("COMPRESS=DEFLATE",
                        "PREDICTOR=2",
                        "ZLEVEL=6"))

writeRaster(tmax_mean_1970,
            filename = paste0(avg_dir, "/1970/avg_tmax_1970.tif"),
            format = "GTiff",
            overwrite = TRUE,
            options = c("COMPRESS=DEFLATE",
                        "PREDICTOR=2",
                        "ZLEVEL=6"))

writeRaster(tmean_1970,
            filename = paste0(avg_dir, "/1970/avg_tmean_1970.tif"),
            format = "GTiff",
            overwrite = TRUE,
            options = c("COMPRESS=DEFLATE",
                        "PREDICTOR=2",
                        "ZLEVEL=6"))

writeRaster(prec_mean_1970,
            filename = paste0(avg_dir, "/1970/avg_prec_1970.tif"),
            format = "GTiff",
            overwrite = TRUE,
            options = c("COMPRESS=DEFLATE",
                        "PREDICTOR=2",
                        "ZLEVEL=6"))
