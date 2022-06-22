# Rscript niches_margaritifera.R
# qsub -cwd -V -N niches_margaritifera -pe thread 24 -b y "Rscript niches_margaritifera.R"

# making sure all packages are installed and loaded
if (!require("ENMeval")) install.packages("ENMeval"); library(ENMeval)
if (!require("raster")) install.packages("raster"); library(raster)
if (!require("maxnet")) install.packages("maxnet"); library(maxnet)
if (!require("terra")) install.packages("terra")
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("ggspatial")) install.packages("ggspatial"); library(ggspatial)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("foreach")) install.packages("foreach"); library(foreach)

# making sure a scale is between 0 and 1
transform_scale <- function(raster_layer){
  (raster_layer - cellStats(raster_layer, min)) / 
    (cellStats(raster_layer, max) - cellStats(raster_layer, min))
}

# make the ENM model
niche_model <- function(occs, bioclim, predictors){
  modeling.cluster <- parallel::makeCluster(24)
  doParallel::registerDoParallel(modeling.cluster)
  e.mx <- foreach(
    predictor = c(bioclim, predictors),
    .packages = "ENMeval"
  ) %dopar% {
    ENMevaluate(occs = occs, envs = predictor,
                tune.args = list(fc = c("L","LQ","LQH","H"),  # accept multiple feature classes and combination of feature classes
                                 rm = 1:5),                   # test regularization multipliers between 1 and 5
                algorithm = 'maxnet',
                partitions = 'randomkfold',
                partition.settings = list(kfolds = 12),       # make 12 k-fold partitions
                parallel = TRUE, numCores = 12)               # allow parallelization on 12 cores
  }
  parallel::stopCluster(modeling.cluster)
  
  return (e.mx)
}

# make null models
null_model <- function(e.mx.bioc, e.mx.smod, opt.seq){
  null_cluster <- parallel::makeCluster(20)
  doParallel::registerDoParallel(null_cluster)
  mod.null <- foreach(
    model = c(e.mx.bioc, e.mx.smod),
    .packages = "ENMeval"
  ) %dopar% {
    ENMnulls(model, no.iter = 10,                               # loop 10 times
             parallel = TRUE, numCores = 10,                    # allow parallelization on 10 cores
             mod.settings = list(fc = opt.seq$fc,               # use selected feature classes combination
                                 rm = as.numeric(opt.seq$rm)))  # use selected regularization multipliers
  }
  parallel::stopCluster(null_cluster)
  
  return (mod.null)
}

# making facetted evaluation plots
plot_eval <- function(eval, save = NULL){
  evalplot <- ggplot(eval, aes_string(x = "rm", y = "avg",           # use regularization multipliers as x
                                      color = "fc", group = "fc")) + # draw a line feature classes combination
    geom_point(position = position_dodge(width = 0.1)) +             # draw lines and points with a dodge to avoid overlapping
    geom_line(position = position_dodge(width = 0.1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5,      # add error bars
                  position = position_dodge(width = 0.1)) +
    facet_grid(rows = vars(metric), cols = vars(source),             # draw each condition on faceted plots
               scales = "free_y", switch = "y") +
    theme_bw()
  
  if (!is.null(save)){
    ggsave(file = save, plot = evalplot)
  }
}

# making maps for predictions
plot_pred <- function(preds, save = NULL, occ = NULL, color_scale = "viridis"){
  p <- preds %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() %>%                                    # transform into a three columns dataframe
    setNames(c("suitability", "lon", "lat")) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = suitability)) +                   # color based on suitability score
    scale_fill_viridis_c() +                               # using the viridis color scale
    coord_sf(crs = sf::st_crs(4326)) +                     # make sure to use the correct 
    annotation_scale(location = "bl", width_hint = 0.3) +  # add distance scale bar
    labs(title = "", x = "longitude", y = "latitude") +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080")
    )
  
  # use correct color scale
  if (color_scale == "viridis"){
    p <- p + scale_fill_viridis_c()
  } else if (color_scale == "spectral"){
    p <- p + scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")))
  }
  
  if (!is.null(occ)){
    p <- p + geom_point(data = occ, size = 1.5, shape = 1)
  }
  
  if (!is.null(save)){
    ggsave(p, file = save)
  }
  
  return (p)
}

# main ----

fig_dir <- file.path("./results/margaritifera")

# load data ----
occs.2000 <- fread("./data/occurrences/occs_margaritifera_2000.txt",
                   select = c("lon", "lat"),
                   data.table = FALSE)

occs.2010 <- fread("./data/occurrences/occs_margaritifera_2010.txt",
                   select = c("lon", "lat"),
                   data.table = FALSE)

riparian.2000 <- list.files("./data/rivers/riparian_zones/2000",
                            pattern = "france_west.+tif$", full.names = TRUE) %>% stack()

riparian.2010 <- list.files("./data/rivers/riparian_zones/2010",
                            pattern = "france_west.+tif$", full.names = TRUE) %>% stack()

river.shape <- raster("./data/rivers/rivers_shape/euhydro_loire_v013_GPKG/euhydro_loire_v013.tif") %>%
  projectRaster(riparian.2000, method = "bilinear")
river.shape[is.na(river.shape[])] <- 0 

bioclim.2000 <- list.files("./data/BioClim/2000",
                           pattern = "tif$", full.names = TRUE) %>% stack() %>%
  projectRaster(riparian.2000, method = "bilinear")

bioclim.2010 <- list.files("./data/BioClim/2010",
                           pattern = "tif$", full.names = TRUE) %>% stack() %>%
  projectRaster(riparian.2000, method = "bilinear")

predictors.2000 <- stack(bioclim.2000, riparian.2000)

predictors.2010 <- stack(bioclim.2010, riparian.2010)

# modeling ----
print("starting modeling on first decade data")
e.mx.2000 <- niche_model(occs.2000, bioclim.2000, predictors.2000)
print(e.mx.2000)
e.mx.2000.bioc <- e.mx.2000[[1]]
e.mx.2000.river <- e.mx.2000[[2]]
print("done")

print("starting modeling on second decade data")
e.mx.2010 <- niche_model(occs.2010, bioclim.2010, predictors.2010)
print(e.mx.2010)
e.mx.2010.bioc <- e.mx.2010[[1]]
e.mx.2010.river <- e.mx.2010[[2]]
print("done")

# save models to Rdata file for future uses
save(e.mx.2000.bioc, e.mx.2000.river,
     e.mx.2010.bioc, e.mx.2010.river,
     file = file.path(fig_dir, "models.RData"))

# evaluation plot on models' statistics ----
eval.2000.bioc <- evalplot.stats(e = e.mx.2000.bioc,
                                 color = "fc", x.var = "rm",
                                 stats = c("or.10p", "auc.val"),
                                 return.tbl = TRUE)
eval.2000.river <- evalplot.stats(e = e.mx.2000.river,
                                  color = "fc", x.var = "rm",
                                  stats = c("or.10p", "auc.val"),
                                  return.tbl = TRUE)

eval.2000 <- bind_rows(list(bioc = eval.2000.bioc,
                            river = eval.2000.river),
                       .id = "source")
plot_eval(eval.2000, file.path(fig_dir, 2000, "evalplot.svg"))

eval.2010.bioc <- evalplot.stats(e = e.mx.2010.bioc,
                                 color = "fc", x.var = "rm",
                                 stats = c("or.10p", "auc.val"),
                                 return.tbl = TRUE)
eval.2010.river <- evalplot.stats(e = e.mx.2010.river,
                                  color = "fc", x.var = "rm",
                                  stats = c("or.10p", "auc.val"),
                                  return.tbl = TRUE)

eval.2010 <- bind_rows(list(bioc = eval.2010.bioc,
                            river = eval.2010.river),
                       .id = "source")
plot_eval(eval.2010, file.path(fig_dir, 2010, "evalplot.svg"))

# best model ----
# bind evaluation results of all models
opt <- do.call(rbind, lapply(c(e.mx.2000.bioc, e.mx.2000.river,
                               e.mx.2010.bioc, e.mx.2010.river),
                             eval.results))
# compute the mean
opt <- aggregate(opt[-c(1:3)],
                 as.list(opt[, c("fc", "rm", "tune.args")]),
                 mean)
# select best model based on highest AUC and lowest or.10p
opt.seq <- opt %>%
  filter(auc.val.avg > quantile(auc.val.avg, 0.75)) %>%
  filter(or.10p.avg == min(or.10p.avg)) %>%
  tail(1)

# marginal response curves plot on best model ----
mod.seq.2000.bioc <- eval.models(e.mx.2000.bioc)[[opt.seq$tune.args]]
mod.seq.2000.river <- eval.models(e.mx.2000.river)[[opt.seq$tune.args]]

svg(file.path(fig_dir, 2000, "mod_seq_bioc.svg"))
plot(mod.seq.2000.bioc, type = "cloglog")
dev.off()
svg(file.path(fig_dir, 2000, "mod_seq_river.svg"))
plot(mod.seq.2000.river, type = "cloglog")
dev.off()

mod.seq.2010.bioc <- eval.models(e.mx.2010.bioc)[[opt.seq$tune.args]]
mod.seq.2010.river <- eval.models(e.mx.2010.river)[[opt.seq$tune.args]]

svg(file.path(fig_dir, 2010, "mod_seq_bioc.svg"))
plot(mod.seq.2010.bioc, type = "cloglog")
dev.off()
svg(file.path(fig_dir, 2010, "mod_seq_river.svg"))
plot(mod.seq.2010.river, type = "cloglog")
dev.off()

# prediction ---- 
pred.seq.2000.bioc <- eval.predictions(e.mx.2000.bioc)[[opt.seq$tune.args]]
plot_pred(pred.seq.2000.bioc, file.path(fig_dir, 2000, "pred_bioc.svg"))
plot_pred(pred.seq.2000.bioc, file.path(fig_dir, 2000, "pred_bioc_occ.svg"),
          eval.occs(e.mx.2000.bioc))

pred.seq.2000.river <- eval.predictions(e.mx.2000.river)[[opt.seq$tune.args]]
plot_pred(pred.seq.2000.river, file.path(fig_dir, 2000, "pred_river.svg"))
plot_pred(pred.seq.2000.river, file.path(fig_dir, 2000, "pred_river_occ.svg"),
          eval.occs(e.mx.2000.river))

pred.seq.2010.bioc <- eval.predictions(e.mx.2010.bioc)[[opt.seq$tune.args]]
plot_pred(pred.seq.2010.bioc, file.path(fig_dir, 2010, "pred_bioc.svg"))
plot_pred(pred.seq.2010.bioc, file.path(fig_dir, 2010, "pred_bioc_occ.svg"),
          eval.occs(e.mx.2010.bioc))

pred.seq.2010.river <- eval.predictions(e.mx.2010.river)[[opt.seq$tune.args]]
plot_pred(pred.seq.2010.river, file.path(fig_dir, 2010, "pred_river.svg"))
plot_pred(pred.seq.2010.river, file.path(fig_dir, 2010, "pred_river_occ.svg"),
          eval.occs(e.mx.2010.river))

pred.seq.bioc <- pred.seq.2010.bioc**2 - pred.seq.2000.bioc**2
plot_pred(pred.seq.bioc, file.path(fig_dir, "preds_bioc.svg"), color_scale = "spectral")

pred.seq.river <- pred.seq.2010.river**2 - pred.seq.2000.river**2
plot_pred(pred.seq.river, file.path(fig_dir, "preds_river.svg"), color_scale = "spectral")

# prediction across decades ----
pred.2000.2010.bioc <- terra::predict(bioclim.2010,
                                      e.mx.2000.bioc@models[[opt.seq$tune.args]]) %>%
  transform_scale()
plot_pred(pred.2000.2010.bioc, paste0(fig_dir, "/pred_2000_on_2010_bioc.svg"))

pred.2000.2010.river <- terra::predict(predictors.2010,
                                       e.mx.2000.river@models[[opt.seq$tune.args]]) %>%
  transform_scale()
plot_pred(pred.2000.2010.river, paste0(fig_dir, "/pred_2000_on_2010_river.svg"))

pred.2010.2000.bioc <- terra::predict(bioclim.2000,
                                      e.mx.2010.bioc@models[[opt.seq$tune.args]]) %>%
  transform_scale()
plot_pred(pred.2010.2000.bioc, paste0(fig_dir, "/pred_2010_on_2000_bioc.svg"))

pred.2010.2000.river <- terra::predict(predictors.2000,
                                       e.mx.2010.river@models[[opt.seq$tune.args]]) %>%
  transform_scale()
plot_pred(pred.2010.2000.river, paste0(fig_dir, "/pred_2010_on_2000_river.svg"))

# null model ----
# issue in null models: Error in x[[jj]][iseq] <- vjj : replacement has length zero
# https://githubhot.com/repo/jamiemkass/ENMeval/issues/120
# jamiemkass (package owner seems pretty reactive so might want to ask him directly)

print("starting modeling on first decade data")
mod.null.2000 <- null_model(e.mx.2000.bioc, e.mx.2000.river, opt.seq)
mod.null.2000.bioc <- mod.null.2000[[1]]
mod.null.2000.river <- mod.null.2000[[2]]
print("done")

print("starting modeling on second decade data")
mod.null.2010 <- null_model(e.mx.2010.bioc, e.mx.2010.river, opt.seq)
mod.null.2010.bioc <- mod.null.2010[[1]]
mod.null.2010.river <- mod.null.2010[[2]]
print("done")

eval.null.2000.bioc <- evalplot.nulls(mod.null.2000.bioc,
                                      stats = c("or.10p", "auc.val"),
                                      plot.type = "violin")
ggsave(file = file.path(fig_dir, 2000, "nullmod_bioc.svg"),
       plot = eval.null.2000.bioc)

eval.null.2000.river <- evalplot.nulls(mod.null.2000.river,
                                       stats = c("or.10p", "auc.val"),
                                       plot.type = "violin")
ggsave(file = file.path(fig_dir, 2000, "nullmod_river.svg"),
       plot = eval.null.2000.river)

eval.null.2010.bioc <- evalplot.nulls(mod.null.2010.bioc,
                                      stats = c("or.10p", "auc.val"),
                                      plot.type = "violin")
ggsave(file = file.path(fig_dir, 2010, "nullmod_bioc.svg"),
       plot = eval.null.2010.bioc)

eval.null.2010.river <- evalplot.nulls(mod.null.2010.river,
                                       stats = c("or.10p", "auc.val"),
                                       plot.type = "violin")
ggsave(file = file.path(fig_dir, 2010, "nullmod_river.svg"),
       plot = eval.null.2010.river)

# save null models to Rdata file for future uses
save(mod.null.2000.bioc, mod.null.2000.river,
     mod.null.2010.bioc, mod.null.2010.river,
     file = file.path(fig_dir, "null_models.RData"))