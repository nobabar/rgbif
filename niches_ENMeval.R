library(ENMeval)
library(raster)
library(dplyr)
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

occs_1970 <- fread("./data/occurrences/Hirundo_rustica_1970/occurrence.txt",
                   data.table = FALSE,
                   fill = FALSE,
                   encoding = "UTF-8",
                   select = to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate) %>%
  filter(occurrenceStatus == "PRESENT") %>%
  filter(if_any(issue, ~!grepl(pattern = "FOOTPRINT_WKT_INVALID|COORDINATE_ROUNDED.*GEODETIC_DATUM_ASSUMED_WGS84", .))) %>%
  dplyr::select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec))

climatic_stack_1970 <- list.files("./data/WorldClim_avg/1970",
                              full.names = TRUE) %>% stack()


occs_z <- raster::extract(climatic_stack_1970, occs_1970[c("lon", "lat")])

occs.sim <- similarity(climatic_stack_1970, occs_z)

occs.mess <- occs.sim$similarity_min

occs.sp <- sp::SpatialPoints(occs_1970[c("lon", "lat")])

rasterVis::levelplot(occs.mess, main = "Environmental similarity", margin = FALSE) + 
     latticeExtra::layer(sp.points(occs.sp, col="black"))

myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

