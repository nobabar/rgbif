library(foreach)
library(raster)
library(dplyr)

# download links
database <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/"

tmin_1970 <- paste0(database, "wc2.1_2.5m_tmin_1970-1979.zip")
tmax_1970 <- paste0(database, "wc2.1_2.5m_tmax_1970-1979.zip")
prec_1970 <- paste0(database, "wc2.1_2.5m_prec_1970-1979.zip")

url_1970 <- c(tmin_1970, tmax_1970, prec_1970)

tmin_2010 <- paste0(database, "wc2.1_2.5m_tmin_2010-2018.zip")
tmax_2010 <- paste0(database, "wc2.1_2.5m_tmax_2010-2018.zip")
prec_2010 <- paste0(database, "wc2.1_2.5m_prec_2010-2018.zip")

url_2010 <- c(tmin_2010, tmax_2010, prec_2010)

downloadto <- file.path(normalizePath(tempdir(), "/"), "WorldClim")
dir.create(downloadto)


crop_mask_cluster <- parallel::makeCluster(6)
doParallel::registerDoParallel(crop_mask_cluster)

files <- foreach(
  url = c(url_1970, url_2010)
) %dopar% {
  zipfile <- file.path(downloadto, basename(url))
  download.file(url, destfile = zipfile)
  files <- unzip(zipfile, list=TRUE) # zip files too large
}

parallel::stopCluster(crop_mask_cluster)


for (file in files_1970) {
  raster(file) %>%
    crop(france) %>%
    mask(france) %>%
    writeRaster(filename = sub("^(.*[/])",
                               output_dir,
                               files),
                format = "GTiff",
                overwrite = TRUE)
}