# qsub -cwd -V -N niches_ENMeval_hirundo_2010 -pe thread 10 -b y "Rscript niches_ENMeval.R"

if (!require("ENMeval")) install.packages("ENMeval"); library(ENMeval)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("ggspatial")) install.packages("ggspatial"); library(ggspatial)
if (!require("viridisLite")) install.packages("viridisLite"); library(viridisLite)
if (!require("raster")) install.packages("raster"); library(raster)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("data.table")) install.packages("data.table"); library(data.table)

occs_2010 <- fread("~/save/data/occurrences/occs_hirundo_2010.txt",
                   data.table = FALSE,
                   fill = FALSE,
                   encoding = "UTF-8")

pts_2010 <- occs_2010[c("lon", "lat")]

bioclim_stack_2010 <- list.files("~/save/data/BioClim/2010",
                                  pattern = ".tif$",
                                  full.names = TRUE) %>% stack()

built_up_2010 <- raster("~/save/data/settlement/2010/GHS_SMOD_2010_FRANCE.tif") %>%
  projectRaster(bioclim_stack_2010, method = "bilinear")

# built_up_2010@data@values[xor(is.na(getValues(bioclim_stack_2010[[1]])), is.na(getValues(built_up_2010)))] <- NA

predictors <- stack(bioclim_stack_2010, built_up_2010)


# extract the predictor values for each occurence
occs_z <- cbind(pts_2010, raster::extract(predictors, pts_2010))


# calculate environmental similarity
# occs.sim <- similarity(predictors, occs_z)
# occs.mess <- occs.sim$similarity_min
# 
# occs.sp <- sp::SpatialPoints(pts_2010)
# 
# myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
# rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) +
#   latticeExtra::layer(sp.points(occs.sp, col="black"))

# generate background points
bg <- dismo::randomPoints(predictors, 10000)
colnames(bg) <- colnames(pts_2010)

# extract the predictor values for each background point
bg_z <- cbind(bg, raster::extract(predictors, bg))

# modeling ----
e.mx <- ENMevaluate(occs = pts_2010, envs = predictors, bg = bg, 
                    algorithm = 'maxnet',
                    partitions = 'randomkfold',
                    partition.settings = list(kfolds = 12),
                    tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))


# overlap <- calc.niche.overlap(e.mx@predictions, overlapStat = "D")

svg("~/save/fig/hirundo/2010/evalplot.svg")
evalplot.stats(e = e.mx, color = "fc", x.var = "rm",
               stats = c("or.10p", "auc.val"))
dev.off()

# select best model based on highest AUC and lowest or.mtp
opt.seq <- e.mx %>%
  eval.results() %>%
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg)) %>%
  tail(1)

mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]

svg("~/save/fig/hirundo/2010/mod_seq.svg")
plot(mod.seq, type = "cloglog")
dev.off()

pred.seq <- eval.predictions(e.mx)[[opt.seq$tune.args]]

svg("~/save/fig/pred.svg")
pred.seq %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  setNames(c("value", "lon", "lat")) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = viridis(20),
                       limits=c(0,1)) +
  coord_sf(crs = sf::st_crs(4326)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                    colour = "#808080")
  )
dev.off()


svg("~/save/fig/hirundo/2010/pred_occ.svg")
pred.seq %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  setNames(c("value", "lon", "lat")) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = viridis(20),
                       limits=c(0,1)) +
  geom_count(data=eval.occs(e.mx), size = 1.5, shape = 1) +
  coord_sf(crs = sf::st_crs(4326)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                    colour = "#808080")
  )
dev.off()

# null model
mod.null <- ENMnulls(e.mx, mod.settings = list(fc = toString(opt.seq$fc), rm =  as.numeric(opt.seq$rm)), no.iter = 10, parallel = TRUE)

svg("~/save/fig/hirundo/2010/nullmod.svg")
evalplot.nulls(mod.null, stats = c("or.10p", "auc.val"), plot.type = "violin")
dev.off()
