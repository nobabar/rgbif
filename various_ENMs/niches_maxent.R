# qsub -cwd -V -N maxent_predict -pe thread 4 -b y "Rscript niches_maxent.R"

# set correct option for the java VM
# otherwise cluster might crash
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
gc()

if (!require("dismo")) install.packages("dismo")
library(dismo)
library(raster)
library(tidyverse)
library(data.table)

# climatic predictors
climatic_stack_1970 <- list.files("./data/WorldClim_avg/1970",
                                  full.names = TRUE) %>% stack()

climatic_stack_2010 <- list.files("./data/WorldClim_avg/2010",
                                  full.names = TRUE) %>% stack()

# columns to keep in dataset
to_keep <- c("gbifID", "datasetKey", "recordedBy",
             "individualCount", "occurrenceStatus",
             "eventDate", "year", "month", "day",
             "countryCode", "stateProvince", "county", "municipality",
             "decimalLatitude", "decimalLongitude",
             "issue", "hasCoordinate", "hasGeospatialIssues",
             "level0Gid", "level0Name", "level1Gid", "level1Name",
             "level2Gid", "level2Name", "level3Gid", "level3Name")

rename_vec <- c(lon = "decimalLongitude", lat = "decimalLatitude")

# import and filter data
occs_2010 <- fread("./data/occurrences/Hirundo_rustica_2010/occurrence.txt",
                   data.table = FALSE,
                   select = to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate) %>%
  filter(occurrenceStatus == "PRESENT") %>%
  filter(if_any(issue, ~!grepl(pattern = "FOOTPRINT_WKT_INVALID|COORDINATE_ROUNDED.*GEODETIC_DATUM_ASSUMED_WGS84", .))) %>%
  dplyr::select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec))


# is there duplicate points
dup <- duplicated(occs_2010)
if (any(dup)) {
  occs_2010 <- occs_2010[!dup, ]
}

# is there points on the same pixel
cell <- cellFromXY(climatic_stack_2010[[1]], occs_2010[c("lon", "lat")])
dup <- duplicated(cell)
if (any(dup)) {
  occs_2010 <- occs_2010[!dup, ]
}

# aggregate points within a minimum distance
r <- raster(extent(range(occs_2010$lon), range(occs_2010$lat)))
min_dist <- 0.5
res(r) <- min_dist
r <- extend(r, extent(r) + min_dist)
agg_occs_2010 <- dismo::gridSample(occs_2010[c("lon", "lat")], r, n = 1)


to_test <- sample(seq_len(nrow(occs_2010)), size = floor(0.20 * nrow(occs_2010)))


occtest <- occs_2010[to_test, ]
occtrain <- occs_2010[-to_test, ]

# useful args
# -P            create some response curves
# -J            jackknife for relative importance of each predictor
# -j directory  environmental variables use for projection
# -threads 4    number of cores to use
# -v            verbose output for debugging
mod <- dismo::maxent(x=climatic_stack_2010, p=occtrain[c("lon", "lat")],
                     path="./results/2010_proj_1970_maxent",
                     args=c("-o ./results/2010_proj_1970_maxent",
                            "-P", "-J", "-j ./data/WorldClim/1970",
                            "threads=4", "-v"))

mod <- dismo::maxent(x=climatic_stack_2010, p=occtrain[c("lon", "lat")],
                     path="./results/2010_proj_1970_maxent",
                     args=c("-P", "-J", "threads=4", "-v"))


bg <- randomPoints(climatic_stack_2010, 1000)

eval_mod <- dismo::evaluate(occtest[c("lon", "lat")], bg, 
                            mod, climatic_stack_2010)
saveRDS(eval_mod, "./results/2010_proj_1970_maxent/evaluation.RDS")

mod_cont <- dismo::predict(mod, climatic_stack_1970)
writeRaster(mod_cont, "./results/2010_proj_1970_maxent/prediction.tif", format="GTiff")
