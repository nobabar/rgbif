# qsub -cwd -V -N modleR_model -pe thread 1 -b y "Rscript niches_modleR.R"

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
gc()

if (!require("modleR")) install.packages("modleR")
library(modleR)
library(raster)
library(tidyverse)
library(data.table)


to_keep <- c("gbifID", "datasetKey", "recordedBy",
             "individualCount", "occurrenceStatus",
             "eventDate", "year", "month", "day",
             "countryCode", "stateProvince", "county", "municipality",
             "decimalLatitude", "decimalLongitude",
             "issue", "hasCoordinate", "hasGeospatialIssues",
             "level0Gid", "level0Name", "level1Gid", "level1Name",
             "level2Gid", "level2Name", "level3Gid", "level3Name")

rename_vec <- c(lon = "decimalLongitude", lat = "decimalLatitude")

occs_2010 <- fread("~/save/data/occurrences/Hirundo_rustica_2010/occurrence.txt",
                   data.table = FALSE,
                   fill = FALSE,
                   encoding = "UTF-8",
                   select = to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate) %>%
  filter(occurrenceStatus == "PRESENT") %>%
  dplyr::select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec))


climatic_stack_2010 <- list.files("~/save/data/WorldClim_avg/2010",
                                  full.names = TRUE) %>% stack()

# cleaning and settig up the data
sdm_data <- setup_sdmdata(species_name = "Hirundo_rustica",
                          occurrences = occs_2010,
                          predictors = climatic_stack_2010,
                          models_dir = "~/work/results/2010",
                          partition_type = "crossvalidation",
                          cv_partitions = 5,
                          cv_n = 1,
                          seed = 512,
                          buffer_type = "mean",
                          png_sdmdata = TRUE,
                          n_back = 500,
                          clean_dupl = TRUE,
                          clean_uni = TRUE,
                          clean_nas = TRUE,
                          geo_filt = FALSE,
                          geo_filt_dist = 10)

# fitting a model
sp_maxent <- do_any(species_name = "Hirundo_rustica",
                    algorithm = "maxent",
                    predictors = climatic_stack_2010,
                    models_dir = "~/work/results/2010",
                    png_partitions = TRUE,
                    write_bin_cut = FALSE,
                    equalize = TRUE,
                    write_rda = TRUE)

# joining partitions
final_model(species_name = "Hirundo_rustica",
            algorithms = c("maxent"),
            models_dir = "~/work/results/2010",
            which_models = c("raw_mean",
                             "bin_mean",
                             "bin_consensus"),
            consensus_level = 0.5,
            uncertainty = TRUE,
            png_final = TRUE,
            overwrite = TRUE)
