# Rscript niches_ENMeval.R
# qsub -cwd -V -N niches_ENMeval -pe thread 24 -b y "Rscript niches_ENMeval.R"

if (!require("ENMeval")) install.packages("ENMeval"); library(ENMeval)
if (!require("raster")) install.packages("raster"); library(raster)
if (!require("maxnet")) install.packages("maxnet"); library(maxnet)
if (!require("terra")) install.packages("terra")
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("ggspatial")) install.packages("ggspatial"); library(ggspatial)
if (!require("data.table")) install.packages("data.table"); library(data.table)
if (!require("foreach")) install.packages("foreach"); library(foreach)


transform_scale <- function(raster_layer){
  (raster_layer - cellStats(raster_layer, min)) / 
    (cellStats(raster_layer, max) - cellStats(raster_layer, min))
}

niche_model <- function(occs, bioclim, predictors){
  modeling.cluster <- parallel::makeCluster(24)
  doParallel::registerDoParallel(modeling.cluster)
  e.mx <- foreach(
    predictor = c(bioclim, predictors),
    .packages = "ENMeval"
  ) %dopar% {
    ENMevaluate(occs = occs, envs = predictor,
                tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5),
                algorithm = 'maxnet', partitions = 'randomkfold',
                partition.settings = list(kfolds = 12),
                parallel = TRUE, numCores = 12)
  }
  parallel::stopCluster(modeling.cluster)
  
  return (e.mx)
}

null_model <- function(e.mx.bioc, e.mx.smod, opt.seq){
  null_cluster <- parallel::makeCluster(20)
  doParallel::registerDoParallel(null_cluster)
  mod.null <- foreach(
    model = c(e.mx.bioc, e.mx.smod),
    .packages = "ENMeval"
  ) %dopar% {
    ENMnulls(model, no.iter = 10, parallel = TRUE, numCores = 10,
             mod.settings = list(fc = opt.seq$fc, rm =  as.numeric(opt.seq$rm)))
  }
  parallel::stopCluster(null_cluster)
  
  return (mod.null)
}

plot_eval <- function(eval, save = NULL){
  evalplot <- ggplot(eval, aes_string(x = "rm", y = "avg",
                                      color = "fc", group = "fc")) + 
    geom_point(position = position_dodge(width = 0.1)) + 
    geom_line(position = position_dodge(width = 0.1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5,
                  position = position_dodge(width = 0.1)) +
    facet_grid(rows = vars(metric), cols = vars(source),
               scales = "free_y", switch = "y") +
    theme_bw()
  
  if (!is.null(save)){
    ggsave(file = save, plot = evalplot)
  }
}

plot_pred <- function(preds, save = NULL, occ = NULL){
  p <- preds %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() %>%
    setNames(c("suitability", "lon", "lat")) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = suitability)) +
    scale_fill_viridis_c() +
    coord_sf(crs = sf::st_crs(4326)) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(title = "", x = "longitude", y = "latitude") +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080")
    )
  
  if (!is.null(save)){
    if (!is.null(occ)){
      p = p + geom_point(data = occ, size = 1.5, shape = 1)
    }
    ggsave(p, file = save)
  }
  
  return (p)
}

plot_diff <- function(preds, save = NULL){
  p <- preds %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() %>%
    setNames(c("suitability", "lon", "lat")) %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = suitability)) +
    scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral"))) +
    coord_sf(crs = sf::st_crs(4326)) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(title = "", x = "longitude", y = "latitude") +
    theme(
      panel.background = element_rect(fill = "#FFFFFF", colour = "#000000",
                                      size = 1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.2, linetype = "solid",
                                      colour = "#808080")
    )
  
  if (!is.null(save)){
    ggsave(p, file = save)
  }
  
  return (p)
}

main <- function(specie, decade1, decade2){
  fig_dir <- file.path("~/save/fig", specie)
  
  # data ----
  occs.decade1 <- fread(paste0("~/save/data/occurrences/occs_",
                               specie, "_", decade1, ".txt"),
                        select = c("lon", "lat"),
                        data.table = FALSE)
  
  occs.decade2 <- fread(paste0("~/save/data/occurrences/occs_",
                               specie, "_", decade2, ".txt"),
                        select = c("lon", "lat"),
                        data.table = FALSE)
  
  bioclim.decade1 <- list.files(file.path("~/save/data/BioClim", decade1),
                                pattern = "tif$", full.names = TRUE) %>% stack()
  
  bioclim.decade2 <- list.files(file.path("~/save/data/BioClim", decade2),
                                pattern = "tif$", full.names = TRUE) %>% stack()
  
  builtup.decade1 <- raster(paste0("~/save/data/settlement/", decade1,
                                   "/GHS_SMOD_", decade1, "_FRANCE.tif")) %>%
    projectRaster(bioclim.decade1, method = "bilinear") %>%
    setNames("GHS_SMOD")
  
  builtup.decade2 <- raster(paste0("~/save/data/settlement/", decade2,
                                   "/GHS_SMOD_", decade2, "_FRANCE.tif")) %>%
    projectRaster(bioclim.decade2, method = "bilinear") %>%
    setNames("GHS_SMOD")
  
  predictors.decade1 <- stack(bioclim.decade1, builtup.decade1)
  
  predictors.decade2 <- stack(bioclim.decade2, builtup.decade2)
  
  # modeling ----
  print("starting modeling on first decade data")
  e.mx.decade1 <- niche_model(occs.decade1, bioclim.decade1, predictors.decade1)
  print(e.mx.decade1)
  e.mx.decade1.bioc <- e.mx.decade1[[1]]
  e.mx.decade1.smod <- e.mx.decade1[[2]]
  print("done")
  
  print("starting modeling on second decade data")
  e.mx.decade2 <- niche_model(occs.decade2, bioclim.decade2, predictors.decade2)
  print(e.mx.decade2)
  e.mx.decade2.bioc <- e.mx.decade2[[1]]
  e.mx.decade2.smod <- e.mx.decade2[[2]]
  print("done")
  
  save(e.mx.decade1.bioc, e.mx.decade1.smod,
       e.mx.decade2.bioc, e.mx.decade2.smod,
       file = file.path(fig_dir, "models.RData"))
  
  # evaluation plot on models' statistics ----
  eval.decade1.bioc <- evalplot.stats(e = e.mx.decade1.bioc,
                                      color = "fc", x.var = "rm",
                                      stats = c("or.10p", "auc.val"),
                                      return.tbl = TRUE)
  eval.decade1.smod <- evalplot.stats(e = e.mx.decade1.smod,
                                      color = "fc", x.var = "rm",
                                      stats = c("or.10p", "auc.val"),
                                      return.tbl = TRUE)
  
  eval.decade1 <- bind_rows(list(bioc = eval.decade1.bioc,
                                 smod = eval.decade1.smod),
                            .id = "source")
  plot_eval(eval.decade1, file.path(fig_dir, decade1, "evalplot.svg"))
  
  eval.decade2.bioc <- evalplot.stats(e = e.mx.decade2.bioc,
                                      color = "fc", x.var = "rm",
                                      stats = c("or.10p", "auc.val"),
                                      return.tbl = TRUE)
  eval.decade2.smod <- evalplot.stats(e = e.mx.decade2.smod,
                                      color = "fc", x.var = "rm",
                                      stats = c("or.10p", "auc.val"),
                                      return.tbl = TRUE)
  
  eval.decade2 <- bind_rows(list(bioc = eval.decade2.bioc,
                                 smod = eval.decade2.smod),
                            .id = "source")
  plot_eval(eval.decade2, file.path(fig_dir, decade2, "evalplot.svg"))
  
  # best model ----
  # bind evaluation results of all models
  opt <- do.call(rbind, lapply(c(e.mx.decade1.bioc, e.mx.decade1.smod,
                                 e.mx.decade2.bioc, e.mx.decade2.smod),
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
  
  # evaluation plot on best model
  mod.seq.decade1.bioc <- eval.models(e.mx.decade1.bioc)[[opt.seq$tune.args]]
  mod.seq.decade1.smod <- eval.models(e.mx.decade1.smod)[[opt.seq$tune.args]]
  
  svg(file.path(fig_dir, decade1, "mod_seq_bioc.svg"))
  plot(mod.seq.decade1.bioc, type = "cloglog")
  dev.off()
  svg(file.path(fig_dir, decade1, "mod_seq_smod.svg"))
  plot(mod.seq.decade1.smod, type = "cloglog")
  dev.off()
  
  mod.seq.decade2.bioc <- eval.models(e.mx.decade2.bioc)[[opt.seq$tune.args]]
  mod.seq.decade2.smod <- eval.models(e.mx.decade2.smod)[[opt.seq$tune.args]]
  
  svg(file.path(fig_dir, decade2, "mod_seq_bioc.svg"))
  plot(mod.seq.decade2.bioc, type = "cloglog")
  dev.off()
  svg(file.path(fig_dir, decade2, "mod_seq_smod.svg"))
  plot(mod.seq.decade2.smod, type = "cloglog")
  dev.off()
  
  # prediction ---- 
  pred.seq.decade1.bioc <- eval.predictions(e.mx.decade1.bioc)[[opt.seq$tune.args]]
  plot_pred(pred.seq.decade1.bioc, file.path(fig_dir, decade1, "pred_bioc.svg"))
  plot_pred(pred.seq.decade1.bioc, file.path(fig_dir, decade1, "pred_bioc_occ.svg"),
            eval.occs(e.mx.decade1.bioc))
  
  pred.seq.decade1.smod <- eval.predictions(e.mx.decade1.smod)[[opt.seq$tune.args]]
  plot_pred(pred.seq.decade1.smod, file.path(fig_dir, decade1, "pred_smod.svg"))
  plot_pred(pred.seq.decade1.smod, file.path(fig_dir, decade1, "pred_smod_occ.svg"),
            eval.occs(e.mx.decade1.smod))
  
  pred.seq.decade2.bioc <- eval.predictions(e.mx.decade2.bioc)[[opt.seq$tune.args]]
  plot_pred(pred.seq.decade2.bioc, file.path(fig_dir, decade2, "pred_bioc.svg"))
  plot_pred(pred.seq.decade2.bioc, file.path(fig_dir, decade2, "pred_bioc_occ.svg"),
            eval.occs(e.mx.decade2.bioc))
  
  pred.seq.decade2.smod <- eval.predictions(e.mx.decade2.smod)[[opt.seq$tune.args]]
  plot_pred(pred.seq.decade2.smod, file.path(fig_dir, decade2, "pred_smod.svg"))
  plot_pred(pred.seq.decade2.smod, file.path(fig_dir, decade2, "pred_smod_occ.svg"),
            eval.occs(e.mx.decade2.smod))
  
  pred.seq.bioc <- pred.seq.decade2.bioc**2 - pred.seq.decade1.bioc**2
  plot_diff(pred.seq.bioc, file.path(fig_dir, "preds_bioc.svg"))
  
  pred.seq.smod <- pred.seq.decade2.smod**2 - pred.seq.decade1.smod**2
  plot_diff(pred.seq.smod, file.path(fig_dir, "preds_smod.svg"))
    
  # prediction across decades ----
  pred.decade1.decade2.bioc <- terra::predict(bioclim.decade2,
                                              e.mx.decade1.bioc@models[[opt.seq$tune.args]]) %>%
    transform_scale()
  plot_pred(pred.decade1.decade2.bioc, paste0(fig_dir, "/pred_",
                                              decade1, "_on_",
                                              decade2, "_bioc.svg"))
  
  pred.decade1.decade2.smod <- terra::predict(predictors.decade2,
                                              e.mx.decade1.smod@models[[opt.seq$tune.args]]) %>%
    transform_scale()
  plot_pred(pred.decade1.decade2.smod, paste0(fig_dir, "/pred_",
                                              decade1, "_on_",
                                              decade2, "_smod.svg"))
  
  pred.decade2.decade1.bioc <- terra::predict(bioclim.decade1,
                                              e.mx.decade2.bioc@models[[opt.seq$tune.args]]) %>%
    transform_scale()
  plot_pred(pred.decade2.decade1.bioc, paste0(fig_dir, "/pred_",
                                              decade2, "_on_",
                                              decade1, "_bioc.svg"))
  
  pred.decade2.decade1.smod <- terra::predict(predictors.decade1,
                                              e.mx.decade2.smod@models[[opt.seq$tune.args]]) %>%
    transform_scale()
  plot_pred(pred.decade2.decade1.smod, paste0(fig_dir, "/pred_",
                                              decade2, "_on_",
                                              decade1, "_smod.svg"))
  
  # null model ----
  print("starting modeling on first decade data")
  mod.null.decade1 <- null_model(e.mx.decade1.bioc, e.mx.decade1.smod, opt.seq)
  mod.null.decade1.bioc <- mod.null.decade1[[1]]
  mod.null.decade1.smod <- mod.null.decade1[[2]]
  print("done")
  
  print("starting modeling on second decade data")
  mod.null.decade2 <- null_model(e.mx.decade2.bioc, e.mx.decade2.smod, opt.seq)
  mod.null.decade2.bioc <- mod.null.decade2[[1]]
  mod.null.decade2.smod <- mod.null.decade2[[2]]
  print("done")
  
  eval.null.decade1.bioc <- evalplot.nulls(mod.null.decade1.bioc,
                                           stats = c("or.10p", "auc.val"),
                                           plot.type = "violin")
  ggsave(file = file.path(fig_dir, decade1, "nullmod_bioc.svg"),
         plot = eval.null.decade1.bioc)
  
  eval.null.decade1.smod <- evalplot.nulls(mod.null.decade1.smod,
                                           stats = c("or.10p", "auc.val"),
                                           plot.type = "violin")
  ggsave(file = file.path(fig_dir, decade1, "nullmod_smod.svg"),
         plot = eval.null.decade1.smod)
  
  eval.null.decade2.bioc <- evalplot.nulls(mod.null.decade2.bioc,
                                           stats = c("or.10p", "auc.val"),
                                           plot.type = "violin")
  ggsave(file = file.path(fig_dir, decade2, "nullmod_bioc.svg"),
         plot = eval.null.decade2.bioc)
  
  eval.null.decade2.smod <- evalplot.nulls(mod.null.decade2.smod,
                                           stats = c("or.10p", "auc.val"),
                                           plot.type = "violin")
  ggsave(file = file.path(fig_dir, decade2, "nullmod_smod.svg"),
         plot = eval.null.decade2.smod)
  
  save(mod.null.decade1.bioc, mod.null.decade1.smod,
       mod.null.decade2.bioc, mod.null.decade2.smod,
       file = file.path(fig_dir, "null_models.RData"))
}

args = as.list(commandArgs(trailingOnly=TRUE))

if (length(args) %in% c(2, 3)){
  names(args) <- c("specie", "decade1", "decade2")
  do.call(main, args)
} else {
  stop("Error: wrong number of arguments, must specify specie name and between one and two decades")
}
