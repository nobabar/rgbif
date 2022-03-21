library(rgbif)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(data.table)
library(RColorBrewer)  # for brewer.pal(...)

map <- map_data("france")

dup_obs <- function(occs){
  occs$individualCount[is.na(occs$individualCount)] <- round(mean(occs$individualCount, na.rm=TRUE))
  idx <- rep(1:nrow(occs), occs$individualCount)
  
  return(occs[idx,])
}

make_maps <- function(occs, year=""){
  locations <- as.data.frame(occs)
  
  ggplot(locations, aes(x=decimalLongitude, y=decimalLatitude)) + 
    stat_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon") +
    geom_path(data=map, aes(x=long, y=lat,group=group), colour="grey50") +
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
    coord_fixed() +
    ggtitle(paste("Heatmap de Hirundo rustica pour la décennie", year))
  
  ggsave(paste0("./maps/hirundo/hirundo_heatmap_", year, ".png"))
}


to_keep <- c("gbifID", "identifier", "occurrenceID", "catalogNumber",
             "recordedBy", "individualCount", "occurrenceStatus", "eventDate",
             "year", "month", "day", "countryCode", "stateProvince", "county",
             "municipality", "decimalLatitude", "decimalLongitude", "datasetKey",
             "hasCoordinate", "hasGeospatialIssues", "level0Gid", "level0Name",
             "level1Gid", "level1Name", "level2Gid", "level2Name", "level3Gid",
             "level3Name", "iucnRedListCategory")

# occs_1970 <- occ_search(scientificName="Hirundo rustica", limit=50000, country='FR', year="1970,1979", hasCoordinate=TRUE)
occs_1970 <- data.table::fread("./data/Hirundo_rustica_1970/occurrence.txt",
                               data.table=FALSE,
                               fill=FALSE,
                               encoding="UTF-8",
                               select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  dup_obs

# occs_2010 <- occ_search(scientificName="Hirundo rustica", limit=50000, country='FR', year="2010,2019", hasCoordinate=TRUE)
occs_2010 <- data.table::fread("./data/Hirundo_rustica_2010/occurrence.txt",
                               data.table=FALSE,
                               fill=FALSE,
                               encoding="UTF-8",
                               select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  dup_obs

make_maps(occs_1970, "1970")
make_maps(occs_2010, "2010")

# moyennage ----

mean_map <- function(occs, decennie, level){
  mean_locations <- occs %>%
    group_by_("eventDate", paste0("level", level, "Gid")) %>%
    summarise_at(vars("decimalLongitude", "decimalLatitude"), mean)
  
  levels = c("régionales", "départementales", "communales")
  
  ggplot(mean_locations, aes(x=decimalLongitude, y=decimalLatitude)) + 
    stat_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon") +
    geom_path(data=map, aes(x=long, y=lat,group=group), colour="grey50") +
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
    coord_fixed() +
    ggtitle(paste("Heatmap de moyennes", levels[level],
                  "\nde Hirundo rustica pour la décennie", decennie))
  
  ggsave(paste0("./maps/hirundo/hirundo_mean_level_", levels[level], "_", decennie, ".png"))
}

mean_map(occs_1970, "1970", 1)
mean_map(occs_1970, "1970", 2)
mean_map(occs_1970, "1970", 3)
mean_map(occs_2010, "2010", 1)
mean_map(occs_2010, "2010", 2)
mean_map(occs_2010, "2010", 3)

# ----

# library(maptools)
# data("wrld_simpl")
# SP <- SpatialPoints(coords=mean_locations[c("decimalLongitude","decimalLatitude")],
#                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
# plot(SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16)
# plot(wrld_simpl, add=TRUE)
# 
# for (key in unique(locations$datasetKey)){
#   SP <- SpatialPoints(coords=locations[locations$datasetKey==key,],
#                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
#   mean_locations <- locations[locations$datasetKey==key,] %>%
#     summarise_at(vars("decimalLongitude", "decimalLatitude"), mean)
#   mean_SP <- SpatialPoints(coords=mean_locations,
#                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
#   plot(SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16)
#   plot(mean_SP, col=rgb(red = 0.2, green = 0.2, blue = 0.2, alpha = 0.4), pch=16, add=TRUE)
#   plot(wrld_simpl, add=TRUE)
# }


# normalization ----

total_1970 <- occ_count(country="FR", year="1970,1979")
total_2010 <- occ_count(country="FR", year="2010,2019")


picus_key <- name_backbone(name="Picus viridis", rank="species")$speciesKey
total_picus_1970 <- occ_count(taxonKey=picus_key, country="FR", year="1970,1979")
total_picus_2010 <- occ_count(taxonKey=picus_key, country="FR", year="2010,2019")

occs_1970 %>%
  group_by(level2Name) %>% count(level2Name) %>% mutate(n = n / total_picus_1970) %>% filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") + 
  coord_flip()

occs_2010 %>%
  group_by(level2Gid) %>% count(level2Name) %>% mutate(n = n / total_picus_2010) %>% filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") + 
  coord_flip()



