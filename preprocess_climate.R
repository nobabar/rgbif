# qsub -cwd -V -N preprocess_climate -pe thread 5 -b y "Rscript preprocess_climate.R"

if (!require("foreach")) install.packages("foreach"); library(foreach)
if (!require("raster")) install.packages("raster"); library(raster)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)


# directories and base files ----

# france borders
france <- shapefile("~/save/data/gadm40_FRA_shp/gadm40_FRA_0.shp")

input_dir <- "~/work/WorldClim"
output_dir <- "~/save/data/WorldClim_France"
bioclim_dir <- "~/save/data/BioClim"
avg_dir <- "~/save/data/WorldClim_avg"

# all Worldclim files
files_1970 <- list.files(file.path(input_dir, "1970"),
                         pattern = ".tif$",
                         full.names = TRUE,
                         recursive = TRUE)

files_2000 <- list.files(file.path(input_dir, "2000"),
                         pattern = ".tif$",
                         full.names = TRUE,
                         recursive = TRUE)

files_2010 <- list.files(file.path(input_dir, "2010"),
                         pattern = ".tif$",
                         full.names = TRUE,
                         recursive = TRUE)


# crop files to france shape ----
crop_mask <- function(files, decade, country, output_dir){
  if (!dir.exists(bioclim_dir)) dir.create(output_dir)
  
  crop_mask_cluster <- parallel::makeCluster(5)
  doParallel::registerDoParallel(crop_mask_cluster)
  
  foreach(
    file = files,
    .packages = c("raster", "dplyr")
  ) %dopar% {
    raster(file) %>%
      crop(country) %>%
      mask(country) %>%
      writeRaster(filename = paste0(file.path(output_dir,
                                              decade,
                                              names(.)),
                                    ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
  }
  parallel::stopCluster(crop_mask_cluster)
}

crop_mask(files_1970, "1970", france, output_dir)
crop_mask(files_2000, "2000", france, output_dir)
crop_mask(files_2010, "2010", france, output_dir)


# calculate bioclim variables ----
bioclim_decade <- function(source_dir, decade_range){
  bioclim_cluster <- parallel::makeCluster(5)
  doParallel::registerDoParallel(bioclim_cluster)
  
  bioc_res <- foreach(
    year = decade_range,
    .packages = c("raster", "dismo")
  ) %dopar% {
    tmin_stack <- stack(list.files(source_dir,
                                   full.names = TRUE,
                                   pattern = paste0("(.*tmin)",
                                                    "(.*", year, ")",
                                                    "(.*tif$)")))
    
    tmax_stack <- stack(list.files(source_dir,
                                   full.names = TRUE,
                                   pattern = paste0("(.*tmax)",
                                                    "(.*", year, ")",
                                                    "(.*tif$)")))
    
    prec_stack <- stack(list.files(source_dir,
                                   full.names = TRUE,
                                   pattern = paste0("(.*prec)",
                                                    "(.*", year, ")",
                                                    "(.*tif$)")))
    
    # generate all bioclim variables
    bioc <- biovars(prec_stack, tmin_stack, tmax_stack)
  }
  parallel::stopCluster(bioclim_cluster)
  return(bioc_res)
}

bioclim_1970 <- bioclim_decade(file.path(output_dir, "1970"), 1970:1979)
bioclim_2000 <- bioclim_decade(file.path(output_dir, "2000"), 2000:2009)
bioclim_2010 <- bioclim_decade(file.path(output_dir, "2010"), 2010:2018)


# merge yearly bioclim layers ----
combine_bioclim <- function(bioclim_res, output_dir){
  if (!dir.exists(bioclim_dir)) dir.create(output_dir)
  
  bioclim_cluster <- parallel::makeCluster(5)
  doParallel::registerDoParallel(bioclim_cluster)
  
  foreach(
    layer = names(bioclim_res[[1]]),
    .packages = c("dplyr", "raster")
  ) %dopar% {
    stack(bioclim_res) %>%
      subset(grep(layer, names(.), value = TRUE)) %>%
      calc(fun = mean, na.rm = TRUE) %>%
      writeRaster(filename = paste0(file.path(output_dir,
                                              layer),
                                    ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
  }
  parallel::stopCluster(bioclim_cluster)
}

combine_bioclim(bioclim_1970, file.path(bioclim_dir, "1970"))
combine_bioclim(bioclim_2000, file.path(bioclim_dir, "2000"))
combine_bioclim(bioclim_2010, file.path(bioclim_dir, "2010"))
