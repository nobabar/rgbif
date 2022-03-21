rm(list=ls())

library(rgbif)
library(raster)
library(ggplot2)
library(tidyverse)
library(data.table)
# library(sf)
library(RColorBrewer)

# library(rnaturalearth)
# library(rnaturalearthdata)
# library(ggspatial)

# world <- ne_countries(scale = "medium", returnclass = "sf")
# europe <- ne_countries(continent="europe", scale = "medium", returnclass = "sf")
france <- map_data("france")

# duplicate by number of observation
dup_obs <- function(occs){
  occs$individualCount[is.na(occs$individualCount)] <- round(mean(occs$individualCount, na.rm=TRUE))
  idx <- rep(1:nrow(occs), occs$individualCount)
  
  return(occs[idx,])
}

make_maps <- function(occs, year=""){
  locations <- as.data.frame(occs)
  locations <- locations[,c("acceptedScientificName","stateProvince", "year",
                            "recordedBy", "basisOfRecord", "references",
                            "decimalLatitude","decimalLongitude")]
  
  loc_sf = st_as_sf(locations,
                    coords = c("decimalLongitude","decimalLatitude"), 
                    crs = 4326)
  
  # ggplot() + 
  #   geom_sf(data=world) +
  #   geom_sf(data=loc_sf, color=rgb(0.2, 0.2, 0.2, alpha=0.4)) +
  #   coord_sf(xlim = c(-25, 50), ylim = c(35, 70)) +
  #   annotation_scale(location = "bl", width_hint = 0.1) +
  #   theme(
  #     panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
  #                                     size = 1, linetype = "solid"),
  #     panel.grid.major = element_line(size = 0.2, linetype = "solid",
  #                                     colour = "#808080")
  #   ) +
  #   ggtitle(paste("Occurences de Picus viridis pour l'année", year))
  # 
  # ggsave(paste0("./maps/picus_occurences_", year, ".png"))
  
  ggplot(world) + 
    geom_sf(data=world) +
    stat_density2d(data=locations,
                   aes(x=decimalLongitude, y=decimalLatitude, fill=..level..),
                   geom='polygon', alpha=.1, bins=1000) +
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
    coord_sf(xlim = c(-25, 50), ylim = c(35, 70)) +
    annotation_scale(location = "bl", width_hint = 0.1) +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080")
    ) +
    ggtitle(paste("Heatmap de Picus viridis pour l'année", year))
  
  ggsave(paste0("./maps/picus/picus_heatmap_", year, ".png"))
}

# occs_recent <- occ_search(scientificName="Picus viridis", limit=5000)

to_keep <- c("gbifID", "identifier", "occurrenceID", "catalogNumber",
             "recordedBy", "individualCount", "occurrenceStatus", "eventDate",
             "year", "month", "day", "countryCode", "stateProvince", "county",
             "municipality", "decimalLatitude", "decimalLongitude", "datasetKey",
             "hasCoordinate", "hasGeospatialIssues", "level0Gid", "level0Name",
             "level1Gid", "level1Name", "level2Gid", "level2Name", "level3Gid",
             "level3Name", "iucnRedListCategory")

# occs_1970 <- occ_search(scientificName="Picus viridis", limit=5000, year="1970,1979", hasCoordinate=TRUE)
occs_1970 <- fread("./data/Picus_viridis_1970/occurrence.txt",
                   data.table=FALSE,
                   fill=FALSE,
                   encoding="UTF-8",
                   select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  dup_obs()

# occs_2010 <- occ_search(scientificName="Picus viridis", limit=5000, year="2010,2019", hasCoordinate=TRUE)
occs_2010 <- fread("./data/Picus_viridis_2010/occurrence.txt",
                   data.table=FALSE,
                   fill=FALSE,
                   encoding="UTF-8",
                   select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  dup_obs()

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
    geom_path(data=france, aes(x=long, y=lat,group=group), colour="grey50") +
    scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) +
    coord_fixed() +
    ggtitle(paste("Heatmap de moyennes", levels[level],
                  "\nde Picus viridis pour la décennie", decennie))
  
  ggsave(paste0("./maps/picus/picus_mean_", levels[level], "_", decennie, ".png"))
}

mean_map(occs_1970, "1970", 1)
mean_map(occs_1970, "1970", 2)
mean_map(occs_1970, "1970", 3)
mean_map(occs_2010, "2010", 1)
mean_map(occs_2010, "2010", 2)
mean_map(occs_2010, "2010", 3)

# normalization ----

total_1970 <- occ_count(country="FR", year="1970,1979")
total_2010 <- occ_count(country="FR", year="2010,2019")

occs_1970 %>%
  group_by(level2Name) %>% count(level2Name) %>% mutate(n = n / total_1970) %>% filter(n > 1e-6) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(x = "Region of Organism", y = "Number of Occurrence") + 
    coord_flip()

occs_2010 %>%
  group_by(level2Gid) %>% count(level2Name) %>% mutate(n = n / total_2010) %>% filter(n > 1e-6) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") + 
  coord_flip()
