# qsub -cwd -V -N maxent_predict -pe thread 4 -b y "Rscript niches_maxent.R"

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
gc()

if (!require("dismo")) install.packages("dismo")
library(dismo)
library(raster)
library(tidyverse)
library(data.table)


climatic_stack_1970 <- list.files("~/save/data/WorldClim_avg/1970",
                                  full.names = TRUE) %>% stack()

climatic_stack_2010 <- list.files("~/save/data/WorldClim_avg/2010",
                                  full.names = TRUE) %>% stack()



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
min_dist <- 0.5
r <- raster(extent(range(occs_2010[, "lon"]), range(occs_2010[, "lat"])) + min_dist)
occs_2010 <- dismo::gridSample(occs_2010[c("lon", "lat")], r, n = 1)




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
                     path="~/work/results/2010_proj_1970_maxent",
                     args=c("-o ~/work/results/2010_proj_1970_maxent",
                            "-P", "-J", "-j ~/save/data/WorldClim/1970",
                            "threads=4", "-v"))

mod <- dismo::maxent(x=climatic_stack_2010, p=occtrain[c("lon", "lat")],
                     path="~/work/results/2010_proj_1970_maxent",
                     args=c("-P", "-J", "threads=4", "-v"))


bg <- randomPoints(climatic_stack_2010, 1000)

eval_mod <- dismo::evaluate(occtest[c("lon", "lat")], bg, 
                            mod, climatic_stack_2010)
saveRDS(eval_mod, "~/work/results/2010_proj_1970_maxent/evaluation.RDS")

mod_cont <- dismo::predict(mod, climatic_stack_1970)
writeRaster(mod_cont, "~/work/results/2010_proj_1970_maxent/prediction.tif", format="GTiff")


# eval_df <- data.frame(threshold = eval_mod@t, 
#                       eval_mod@confusion, prevalence = eval_mod@prevalence, 
#                       ODP = eval_mod@ODP, CCR = eval_mod@CCR, TPR = eval_mod@TPR, 
#                       TNR = eval_mod@TNR, FPR = eval_mod@FPR, FNR = eval_mod@FNR, 
#                       PPP = eval_mod@PPP, NPP = eval_mod@NPP, MCR = eval_mod@MCR, 
#                       OR = eval_mod@OR, kappa = eval_mod@kappa, 
#                       TSS = (eval_mod@TPR + eval_mod@TNR) - 1, FScore = 1/((1/eval_mod@TPR + 
#                                                                               1/eval_mod@PPP)/2))
# eval_df$Jaccard <- eval_df$tp/(eval_df$fn + 
#                                  eval_df$tp + eval_df$fp)
# th_table <- dismo::threshold(eval_mod, sensitivity = 0.9)
# proc <- kuenm::kuenm_proc(occ.test = occtest, 
#                           model = mod_cont, threshold = 0.5)
# 
# 
# th_table$presencenb <- eval_mod@np  # number of presence points
# th_table$correlation <- eval_mod@cor
# th_table$pvaluecor <- eval_mod@pcor
# th_table$AUC <- eval_mod@auc
# th_table$AUC_pval <- ifelse(length(eval_mod@pauc) == 0, NA, eval_mod@pauc)
# th_table$AUCratio <- eval_mod@auc/0.5
# th_table$pROC <- proc$pROC_summary[1]
# th_table$pROC_pval <- proc$pROC_summary[2]
# th_table$TSSmax <- max(eval_df$TSS)
# th_table$KAPPAmax <- max(eval_df$kappa)
# 
# dismo_threshold <- "spec_sens"
# th_table$dismo_threshold <- as.character(dismo_threshold)
# th_mod <- th_table[, dismo_threshold]
# th_table$prevalence.value <- eval_mod@prevalence[which(eval_mod@t == th_mod)]
# th_table$PPP <- eval_mod@PPP[which(eval_mod@t == th_mod)]
# th_table$NPP <- eval_mod@NPP[which(eval_mod@t == th_mod)]
# th_table$TPR <- eval_mod@TPR[which(eval_mod@t == th_mod)]
# th_table$TNR <- eval_mod@TNR[which(eval_mod@t == th_mod)]
# th_table$FPR <- eval_mod@FPR[which(eval_mod@t == th_mod)]
# th_table$FNR <- eval_mod@FNR[which(eval_mod@t == th_mod)]
# th_table$CCR <- eval_mod@CCR[which(eval_mod@t == th_mod)]
# th_table$Kappa <- eval_mod@kappa[which(eval_mod@t == th_mod)]
# th_table$F_score <- eval_df$FScore[which(eval_mod@t == th_mod)]
# th_table$Jaccard <- eval_df$Jaccard[which(eval_mod@t == th_mod)]

