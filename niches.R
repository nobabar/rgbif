rm(list=ls())


library(raster)
library(tidyverse)


# get occurence data ----

dup_obs <- function(occs){
  occs$individualCount[is.na(occs$individualCount)] <- round(mean(occs$individualCount, na.rm=TRUE))
  idx <- rep(1:nrow(occs), occs$individualCount)
  
  return(occs[idx,])
}

to_keep <- c("gbifID", "identifier", "occurrenceID", "catalogNumber",
             "recordedBy", "individualCount", "occurrenceStatus", "eventDate",
             "year", "month", "day", "countryCode", "stateProvince", "county",
             "municipality", "decimalLatitude", "decimalLongitude", "datasetKey",
             "hasCoordinate", "hasGeospatialIssues", "level0Gid", "level0Name",
             "level1Gid", "level1Name", "level2Gid", "level2Name", "level3Gid",
             "level3Name", "iucnRedListCategory")

rename_vec <- c(lon = "decimalLongitude", lat = "decimalLatitude")

occs_1970 <- data.table::fread("./data/occurrences/Hirundo_rustica_1970/occurrence.txt",
                               data.table=FALSE,
                               fill=FALSE,
                               encoding="UTF-8",
                               select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec)) %>%
  dup_obs

occs_2010 <- data.table::fread("./data/occurrences/Hirundo_rustica_2010/occurrence.txt",
                               data.table=FALSE,
                               fill=FALSE,
                               encoding="UTF-8",
                               select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec)) %>%
  dup_obs



# get climatic data ----

tmin_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='tmin',
                              full.names=TRUE) %>% stack()

tmax_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='tmax',
                              full.names=TRUE) %>% stack()

prec_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='prec',
                              full.names=TRUE) %>% stack()

tmin_mean_1970 <- calc(tmin_stack_1970, mean)
tmax_mean_1970 <- calc(tmax_stack_1970, mean)
tmean_1970 = (tmin_mean_1970 + tmax_mean_1970) / 2
prec_mean_1970 <- calc(prec_stack_1970, mean)


# modelling ----

library(modleR)

# cleaning and settig up the data
sdm_data <- setup_sdmdata(species_name = "Hirundo_rustica",
                          occurrences = occs_1970,
                          predictors = prec_stack_1970,
                          models_dir = "./results/modleR",
                          partition_type = "crossvalidation",
                          cv_partitions = 5,   # number of partitions in the crossvalidation
                          cv_n = 1,            # number of crossvalidation runs
                          seed = 512,
                          buffer_type = "mean",
                          png_sdmdata = TRUE,
                          n_back = 500,         # number of pseudoabsence points
                          clean_dupl = TRUE,    # removes points with the same longitude and latitude
                          clean_uni = TRUE,     # selects only one point per pixel
                          clean_nas = TRUE,     # removes points that are outside the bounds of the raster
                          geo_filt = FALSE,     # delete occurrences that are too close to each other
                          geo_filt_dist = 10)   # the distance of the geographic filter in the unit of the predictor raster

# fitting a model

sp_maxent <- do_any(species_name = "Hirundo_rustica",
                    algorithm = "maxent",
                    predictors = prec_stack_1970,
                    models_dir = "./results/modleR",
                    png_partitions = TRUE,
                    write_bin_cut = FALSE,
                    equalize = TRUE,
                    write_rda = TRUE)

library(ENMTML)

model <- ENMTML(pred_dir="./data/WorldClim_France/1970",
                result_dir="./results/ENMTML",
                occ_file="./data/occurrences/Hirundo_rustica_1970/occurrence.txt", 
                sp="acceptedScientificName",
                x="decimalLongitude",
                y="decimalLatitude", 
                colin_var=c(method='PCA'), 
                pseudoabs_method=c(method='RND'), 
                pres_abs_ratio = 1, 
                part=c(method='KFOLD', folds='5'), 
                algorithm="MXD", 
                thr=c(type='MAX_TSS'), 
                msdm=c(method='KER'), 
                cores = 1)

library(dismo)  

xm <- maxent(prec_stack_1970, select(occs_1970, c("lon", "lat")))

px <- predict(prec_stack_1970, xm)

plot(px)

