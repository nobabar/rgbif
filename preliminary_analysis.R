library(rgbif)
library(ggplot2)
library(ggtext)
library(dplyr)
library(data.table)
library(raster)


# france shapefiles
france0 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_0.shp")
france1 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_1.shp")
france2 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_2.shp")


# draw heatmaps of occurences ----
make_maps <- function(occs, year="", save=NULL) {
  occs %>%
    as.data.frame() %>%
    ggplot(aes(x = lon, y = lat)) +
    stat_density2d(aes(fill = ..level..),
                   alpha = 0.6,
                   geom = "polygon") +               # density heatmap
    geom_path(data = france1,
              aes(x = long, y = lat, group = group),
              colour = "grey30") +                   # add france borders
    scale_fill_viridis_c() +                         # use viridis color scale
    coord_fixed() +
    labs(title = "", x = "longitude", y = "latitude") +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080"),
      legend.position = "none"
    )
  
  if (!is.null(save)){
    ggsave(paste0(save, "/hirundo_heatmap_", year, ".png")) 
  }
}


# averaging occurences based on country levels ----
mean_map <- function(occs, decade, level, save=NULL) {
  mean_locations <- occs %>%
    group_by_("eventDate", paste0("level", level, "Gid")) %>%
    summarise_at(vars("lon", "lat"), mean)   # summarise occurences by date and place
  
  levels <- c("regionales", "departementales", "communales")
  
  ggplot(mean_locations, aes(x = lon, y = lat)) +
    stat_density2d(aes(fill = ..level..),
                   alpha = 0.5,
                   geom = "polygon") +               # density heatmap
    geom_path(data = france1,
              aes(x = long, y = lat, group = group),
              colour = "grey30") +                   # add france borders
    scale_fill_viridis_c() +                         # use viridis color scale
    coord_fixed() +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080"),
      legend.position = "none"
    )
    
    if (!is.null(save)){
      ggsave(paste0(save, "/hirundo_mean_level_",
                    levels[level], "_", decade, ".png"))
    }
}


# import occurrence data ----
hirundo_occs_1970 <- fread("./data/occurrences/occs_hirundo_1970.txt",
                           data.table = FALSE)
hirundo_occs_2010 <- fread("./data/occurrences/occs_hirundo_2010.txt",
                           data.table = FALSE)
picus_occs_1970 <- fread("./data/occurrences/occs_picus_1970.txt",
                         data.table = FALSE)
picus_occs_2010 <- fread("./data/occurrences/occs_picus_2010.txt",
                         data.table = FALSE)

# make directories
hirundo_maps <- "./maps/hirundo"
picus_maps <- "./maps/picus"
if (!dir.exists(hirundo_maps)) dir.create(hirundo_maps)
if (!dir.exists(picus_maps)) dir.create(picus_maps)

# create all maps ----
make_maps(hirundo_occs_1970, "1970", hirundo_maps)
make_maps(hirundo_occs_2010, "2010", hirundo_maps)

make_maps(picus_occs_1970, "1970", picus_maps)
make_maps(picus_occs_2010, "2010", picus_maps)

mean_map(hirundo_occs_1970, "1970", 2, hirundo_maps)
mean_map(hirundo_occs_1970, "1970", 3, hirundo_maps)
mean_map(hirundo_occs_2010, "2010", 2, hirundo_maps)
mean_map(hirundo_occs_2010, "2010", 3, hirundo_maps)

mean_map(picus_occs_1970, "1970", 2, picus_maps)
mean_map(picus_occs_1970, "1970", 3, picus_maps)
mean_map(picus_occs_2010, "2010", 2, picus_maps)
mean_map(picus_occs_2010, "2010", 3, picus_maps)


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

norm_hirundo_occs_1970 <- hirundo_occs_1970 %>%
  group_by(level2Gid, decimalLatitude, decimalLongitude) %>%
  count(level2Gid) %>%
  mutate(n = n / total_picus_1970) %>%
  select(-level2Gid)

hirundo_occs_1970 %>%
  group_by(level2Name) %>%
  count(level2Name) %>%
  mutate(n = n / total_picus_1970) %>%
  filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") +
  coord_flip()

norm_hirundo_occs_2010 <- hirundo_occs_2010 %>%
  group_by(level2Gid, decimalLatitude, decimalLongitude) %>%
  count(level2Gid) %>%
  mutate(n = n / total_picus_2010) %>%
  select(-level2Gid)

hirundo_occs_2010 %>%
  group_by(level2Gid) %>%
  count(level2Name) %>%
  mutate(n = n / total_picus_2010) %>%
  filter(n > 2e-1) %>%
  ggplot(aes(x = reorder(level2Name, n), y = n, fill = n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Region of Organism", y = "Number of Occurrence") +
  coord_flip()


# mapping after normalization ----
ggplot(norm_hirundo_occs_1970, aes(x = decimalLongitude, y = decimalLatitude)) +
  stat_density2d(aes(fill = ..level..), alpha = 0.5, geom = "polygon") +
  geom_path(data = france0,
            aes(x = long, y = lat, group = group),
            colour = "grey50") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_fixed()

ggplot(norm_hirundo_occs_2010, aes(x = decimalLongitude, y = decimalLatitude)) +
  stat_density2d(aes(fill = ..level..),
                 alpha = 0.5,
                 geom = "polygon") +
  geom_path(data = france0,
            aes(x = long, y = lat, group = group),
            colour = "grey50") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_fixed()


# tendance plot ---
ji <- function(xy, origin=c(0, 0), cellsize=c(0.5, 0.5)) {
  t(apply(xy, 1, function(z) cellsize/2 + origin + cellsize * (floor((z - origin)/cellsize))))
}

tendance_plot <- function(occs){
  JI <- ji(cbind(occs$lon, occs$lat), c(0.01, 0.01))
  occs$X <- JI[, 1]
  occs$Y <- JI[, 2]
  occs$Cell <- paste(occs$X, occs$Y)
  
  counts <- by(occs, occs$Cell, function(d) c(d$X[1], d$Y[1], nrow(d)))
  counts.m <- matrix(unlist(counts), nrow=3)
  rownames(counts.m) <- c("X", "Y", "Count")
  
  count.max <- max(counts.m["Count",])
  colors = sapply(counts.m["Count",], function(n) hsv(sqrt(n/count.max), .7, .7, .8))
  
  plot(france1, border = "grey30")
  points(counts.m["X",], counts.m["Y",], cex=sqrt(counts.m["Count",]/3),
         pch = 19, col=colors)  
}

tendance_plot(hirundo_occs_1970)
