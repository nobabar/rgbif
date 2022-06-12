library(rgbif)
library(ggplot2)
library(ggtext)
library(dplyr)
library(data.table)
library(raster)

france0 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_0.shp")
france1 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_1.shp")
france2 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_2.shp")


make_maps <- function(occs, year="") {
  locations <- as.data.frame(occs)

  ggplot(locations, aes(x = lon, y = lat)) +
    stat_density2d(aes(fill = ..level..),
                   alpha = 0.6,
                   geom = "polygon") +
    geom_path(data = france1,
              aes(x = long, y = lat, group = group),
              colour = "grey30") +
    scale_fill_viridis_c() +
    coord_fixed() +
    labs(title = "", x = "longitude", y = "latitude") +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080"),
      legend.position = "none"
    )

  ggsave(paste0("./maps/hirundo/hirundo_heatmap_", year, ".png"))
}

occs_1970 <- fread("./data/occurrences/occs_hirundo_1970.txt",
                   data.table = FALSE)

occs_2010 <- fread("./data/occurrences/occs_hirundo_2010.txt",
                   data.table = FALSE)

make_maps(occs_1970, "1970")
make_maps(occs_2010, "2010")

# moyennage ----

mean_map <- function(occs, decennie, level) {
  mean_locations <- occs %>%
    group_by_("eventDate", paste0("level", level, "Gid")) %>%
    summarise_at(vars("lon", "lat"), mean)

  levels <- c("regionales", "departementales", "communales")

  ggplot(mean_locations, aes(x = lon, y = lat)) +
    stat_density2d(aes(fill = ..level..),
                   alpha = 0.5,
                   geom = "polygon") +
    geom_path(data = france1,
              aes(x = long, y = lat, group = group),
              colour = "grey30") +
    scale_fill_viridis_c() +
    coord_fixed() +

  ggsave(paste0("./maps/hirundo/hirundo_mean_level_",
                levels[level], "_", decennie, ".png"))
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
# for (key in unique(locations$datasetKey)) {
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

total_1970 <- occ_count(country = "FR", year = "1970,1979")
total_2010 <- occ_count(country = "FR", year = "2010,2019")


picus_key <- name_backbone(name = "Picus viridis", rank = "species")$speciesKey
total_picus_1970 <- occ_count(taxonKey = picus_key,
                              country = "FR",
                              year = "1970,1979")
total_picus_2010 <- occ_count(taxonKey = picus_key,
                              country = "FR",
                              year = "2010,2019")

norm_occs_1970 <- occs_1970 %>%
  group_by(level2Gid, decimalLatitude, decimalLongitude) %>%
  count(level2Gid) %>%
  mutate(n = n / total_picus_1970) %>%
  select(-level2Gid)

occs_1970 %>%
  group_by(level2Name) %>%
  count(level2Name) %>%
  mutate(n = n / total_picus_1970) %>%
  filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") +
  coord_flip()

norm_occs_2010 <- occs_2010 %>%
  group_by(level2Gid, decimalLatitude, decimalLongitude) %>%
  count(level2Gid) %>%
  mutate(n = n / total_picus_2010) %>%
  select(-level2Gid)

occs_2010 %>%
  group_by(level2Gid) %>%
  count(level2Name) %>%
  mutate(n = n / total_picus_2010) %>%
  filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") +
  coord_flip()

# mapping after normalization ----

ggplot(norm_occs_1970, aes(x = decimalLongitude, y = decimalLatitude)) +
  stat_density2d(aes(fill = ..level..), alpha = 0.5, geom = "polygon") +
  geom_path(data = france0,
            aes(x = long, y = lat, group = group),
            colour = "grey50") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_fixed()

ggplot(norm_occs_2010, aes(x = decimalLongitude, y = decimalLatitude)) +
  stat_density2d(aes(fill = ..level..),
                 alpha = 0.5,
                 geom = "polygon") +
  geom_path(data = france0,
            aes(x = long, y = lat, group = group),
            colour = "grey50") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_fixed()

# tendance plot

# data.df <- occs.decade1
# 
# ji <- function(xy, origin=c(0,0), cellsize=c(0.5,0.5)) {
#   t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
# }
# JI <- ji(cbind(data.df$lon, data.df$lat), c(0.01, 0.01))
# data.df$X <- JI[, 1]
# data.df$Y <- JI[, 2]
# data.df$Cell <- paste(data.df$X, data.df$Y)
# 
# counts <- by(data.df, data.df$Cell, function(d) c(d$X[1], d$Y[1], nrow(d)))
# counts.m <- matrix(unlist(counts), nrow=3)
# rownames(counts.m) <- c("X", "Y", "Count")
# 
# count.max <- max(counts.m["Count",])
# colors = sapply(counts.m["Count",], function(n) hsv(sqrt(n/count.max), .7, .7, .8))
# 
# plot(france1, border = "grey30")
# points(counts.m["X",], counts.m["Y",], cex=sqrt(counts.m["Count",]/3),
#      pch = 19, col=colors)