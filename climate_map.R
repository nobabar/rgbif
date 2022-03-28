library(raster)
library(tidyverse)
library(viridis)


france <- getData('GADM', country='FR', level=2)

r <- getData("worldclim", var="bio", res=2.5)
plot(r[[1]])

poly <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_2.shp")
plot(poly)


r1 <- crop(r[[1]], bbox(france))
plot(r1)

r2 <- crop(r[[1]], bbox(poly))
plot(r2)


# processing WorldClim data ----

inputDir <- "./data/WorldClim"
outputDir <- "./data/WorldClim_France"

files_1970 <- list.files(paste0(inputDir, "/1970"),
                         pattern='.tif$',
                         full.names=TRUE,
                         recursive=TRUE)


# precipitation for first month of 1970
precipRaster <- raster(grep(files_1970, pattern='_prec_', value=TRUE)[1])
precipRaster <- crop(precipRaster, bbox(poly))
precipRaster <- mask(precipRaster, poly)

plot(precipRaster, legend = FALSE)

# preprocess all rasters
for (i in 1:length(files_1970)) {
  raster(files_1970[i]) %>% 
    crop(poly) %>% 
    mask(poly) %>% 
    aggregate(fact=2) %>%
    writeRaster(filename=sub("^(.*[/])",
                             paste0(outputDir, "/1970/"),
                             files_1970[i]),
                format="GTiff",
                overwrite=TRUE,
                options=c("COMPRESS=DEFLATE",
                          "PREDICTOR=2",
                          "ZLEVEL=6"))
}

# create raster layer for mean temperature

tmin_stack <- list.files(paste0(outputDir, "/1970"),
                         pattern='tmin',
                         full.names=TRUE) %>% stack()

tmax_stack <- list.files(paste0(outputDir, "/1970"),
                         pattern='tmax',
                         full.names=TRUE) %>% stack()

prec_stack <- list.files(paste0(outputDir, "/1970"),
                         pattern='prec',
                         full.names=TRUE) %>% stack()

tmin_mean <- calc(tmin_stack, mean)
tmax_mean <- calc(tmax_stack, mean)
tmean = (tmin_mean + tmax_mean) / 2
prec_mean <- calc(prec_stack, mean)

par(mfrow=c(2, 2))
plot(tmin_mean, main="minimal temperatures")
plot(tmax_mean, main="maximal temperatures")
plot(tmean, main="average temperatures")
plot(prec_mean, main="average precipiration")
par(mfrow=c(1, 1))

