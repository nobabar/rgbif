library(raster)
library(dplyr)
library(data.table)
library(CoordinateCleaner)

france0 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_0.shp")

clean_occs <- function(infile, outfile){
  to_keep <- c("gbifID", "datasetKey", "recordedBy",
               "individualCount", "occurrenceStatus",
               "eventDate", "year", "month", "day",
               "countryCode", "stateProvince", "county", "municipality",
               "decimalLatitude", "decimalLongitude",
               "issue", "hasCoordinate", "hasGeospatialIssues", "species",
               "level0Gid", "level0Name", "level1Gid", "level1Name",
               "level2Gid", "level2Name", "level3Gid", "level3Name")
  
  rename_vec <- c(lon = "decimalLongitude", lat = "decimalLatitude")
  
  occs <- fread(infile,
                data.table = FALSE,
                fill = FALSE,
                encoding = "UTF-8",
                select = to_keep) %>%
    rename(all_of(rename_vec)) %>%
    filter(!hasGeospatialIssues & hasCoordinate) %>%
    filter(occurrenceStatus == "PRESENT") %>%
    filter(if_any(issue, ~!grepl(pattern = "GEODETIC_DATUM_ASSUMED_WGS84;FOOTPRINT_WKT_INVALID", .))) %>%
    filter(cc_sea(., lon = "lon", lat = "lat", ref = france0, value = "flagged")) %>%
    filter(cc_dupl(., lon = "lon", lat = "lat", value = "flagged")) %>%
    select(c(lon, lat))
  
  # if there are too many occurences, resample
  if (nrow(occs) > 1000){
    occs <- occs[sample(seq_len(nrow(occs)), size = 1000),]
  }
  
  write.csv(occs, outfile, row.names = FALSE)
  return (occs)
}

filter_decade <- function(infile, outfile, decade_start, decade_end = (decade_start + 9)){
  occs <- fread(infile,
                data.table = FALSE,
                fill = FALSE,
                encoding = "UTF-8") %>%
    filter(decade_start <= year & year <= decade_end)
  
  write.csv(occs, outfile, row.names = FALSE)
  return (occs)
}


h1970 <- clean_occs("./data/occurrences/Hirundo_rustica_1970/occurrence.txt",
                    "./data/occurrences/occs_hirundo_1970.txt")
h2010 <- clean_occs("./data/occurrences/Hirundo_rustica_2010/occurrence.txt",
                    "./data/occurrences/occs_hirundo_2010.txt")

p1970 <- clean_occs("./data/occurrences/Picus_viridis_1970/occurrence.txt",
                    "./data/occurrences/occs_picus_1970.txt")
p2010 <- clean_occs("./data/occurrences/Picus_viridis_2010/occurrence.txt",
                    "./data/occurrences/occs_picus_2010.txt")

m2000 <- filter_decade("./data/occurrences/Margaritifera_auricularia/occurrence.txt",
                       "./data/occurrences/Margaritifera_auricularia/occ_2000.txt",
                       2000)

m2010 <- filter_decade("./data/occurrences/Margaritifera_auricularia/occurrence.txt",
                       "./data/occurrences/Margaritifera_auricularia/occ_2010.txt",
                       2010)

m2000 <- clean_occs("./data/occurrences/Margaritifera_auricularia/occ_2000.txt",
                    "./data/occurrences/occs_margaritifera_2000.txt")

m2010 <- clean_occs("./data/occurrences/Margaritifera_auricularia/occ_2010.txt",
                    "./data/occurrences/occs_margaritifera_2010.txt")
