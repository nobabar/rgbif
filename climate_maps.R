rm(list=ls())


library(raster)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(rasterVis)
library(latticeExtra)

france1 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_1.shp")
france2 <- shapefile("./data/gadm40_FRA_shp/gadm40_FRA_2.shp")

# averaging raster layers ----

tmin_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='tmin',
                              full.names=TRUE) %>% stack()

tmax_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='tmax',
                              full.names=TRUE) %>% stack()

prec_stack_1970 <- list.files("./data/WorldClim_France/1970",
                              pattern='prec',
                              full.names=TRUE) %>% stack()

tmin_mean_1970 <- raster("./data/WorldClim_avg/1970/avg_tmin_1970.tif", as.is=TRUE)
tmax_mean_1970 <- raster("./data/WorldClim_avg/1970/avg_tmax_1970.tif", as.is=TRUE)
tmean_1970 <- raster("./data/WorldClim_avg/1970/avg_tmean_1970.tif", as.is=TRUE)
prec_mean_1970 <- raster("./data/WorldClim_avg/1970/avg_prec_1970.tif", as.is=TRUE)


par(mfrow=c(2, 2), mar=c(2,2,2,2))
plot(tmin_mean_1970, main="minimal temperatures")
plot(tmax_mean_1970, main="maximal temperatures")
plot(tmean_1970, main="average temperatures")
plot(prec_mean_1970, main="average precipiration")
par(mfrow=c(1, 1))

tmin_mean_1970 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  setNames(c("value", "x", "y")) %>%
  ggplot(aes(x=x, y=y, fill=value)) +
  geom_tile(alpha=0.8) + 
  geom_polygon(data=france1, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) +
  coord_fixed()

tmax_mean_1970 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  setNames(c("value", "x", "y")) %>%
  ggplot(aes(x=x, y=y, fill=value)) +
  geom_tile(alpha=0.8) + 
  geom_polygon(data=france1, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) +
  coord_fixed()

tmean_1970 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  setNames(c("value", "x", "y")) %>%
  ggplot(aes(x=x, y=y, fill=value)) +
  geom_tile(alpha=0.8) + 
  geom_polygon(data=france1, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) +
  coord_fixed()

prec_mean_1970 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  setNames(c("value", "x", "y")) %>%
  ggplot(aes(x=x, y=y, fill=value)) +
  geom_tile(alpha=0.8) + 
  geom_polygon(data=france1, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_gradientn(colours=brewer.pal(9, "YlGnBu")) +
  coord_fixed()

levelplot(tmin_mean_1970, 
          margin=FALSE,                       
          colorkey=list(
            space='bottom',
            axis.line=list(col='black')       
          ),    
          par.settings=list(
            axis.line=list(col='transparent') 
          ),
          scales=list(draw=FALSE),            
          col.regions=viridis) +           
  layer(sp.polygons(france1, lwd=0.5, col='black'))

# import occurence data ----

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
  rename(all_of(rename_vec))

occs_2010 <- data.table::fread("./data/occurrences/Hirundo_rustica_2010/occurrence.txt",
                               data.table=FALSE,
                               fill=FALSE,
                               encoding="UTF-8",
                               select=to_keep) %>%
  filter(!hasGeospatialIssues & hasCoordinate & occurrenceStatus=="PRESENT") %>%
  select(-c(hasGeospatialIssues, hasCoordinate, occurrenceStatus)) %>%
  rename(all_of(rename_vec))


pts <- st_as_sf(occs_1970, coords = c("lon", "lat"),remove=FALSE)
pts100 <- st_is_within_distance(pts, dist = 100)
pts_agg <- aggregate(pts,
                     pts$geometry,
                     FUN = mean, 
                     join = function(x, y) st_is_within_distance(x, y, dist = 100))

mean_locations <- occs_1970 %>%
  group_by(eventDate, level2Gid) %>%
  summarise_at(vars("lon", "lat"), mean)

tmean_1970 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  setNames(c("value", "x", "y")) %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=value), alpha=0.8) + 
  geom_polygon(data=france1, aes(x=long, y=lat, group=group), 
               fill=NA, color="grey50", size=0.25) +
  scale_fill_gradientn(colours=rev(brewer.pal(11,"Spectral"))) +
  geom_point(data=pts_agg, aes(x=lon, y=lat)) + 
  coord_fixed()
