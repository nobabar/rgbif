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

pts_1970 <- occs_1970[c("lon", "lat")]

climatic_stack_1970 <- list.files("./data/WorldClim_avg/1970",
                                  pattern = ".tif$",
                                  full.names = TRUE) %>% stack()

built_up_1970 <- raster("./data/built_up_land/1970/GHS_BUILT_1975_FRANCE.tif") %>%
  projectRaster(climatic_stack_1970, method = "bilinear")

built_up_1970@data@values[is.na(built_up_1970@data@values)] <- 0

predictors <- stack(climatic_stack_1970, built_up_1970)

occs_z <- cbind(pts_1970, raster::extract(predictors, pts_1970))

occs.sim <- similarity(predictors, occs_z)

occs.mess <- occs.sim$similarity_min

occs.sp <- sp::SpatialPoints(pts_1970)

rasterVis::levelplot(occs.mess, main = "Environmental similarity", margin = FALSE) + 
     latticeExtra::layer(sp.points(occs.sp, col="black"))

myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

bg <- dismo::randomPoints(predictors, 5000)
colnames(bg) <- colnames(pts_1970)

bg_z <- cbind(bg, raster::extract(predictors, bg))


# partitioning using random k-fold ----

rand <- get.randomkfold(pts_1970, bg, k = 5)
evalplot.grps(pts = pts_1970, pts.grp = rand$occs.grp, envs = predictors)

evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs_z, 
                     bg.z = bg_z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp)

evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = predictors, occs.z = occs_z, 
                    bg.z = bg_z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp, 
                    bb.buf = 7)


# running maxent/maxnet ----

# e.mx.l <- ENMevaluate(occs = pts_1970, envs = predictors, bg = bg, 
#                       algorithm = 'maxnet', partitions = 'randomkfold', 
#                       tune.args = list(fc = "L", rm = 1:5))

e.mx <- ENMevaluate(occs = pts_1970, envs = predictors, bg = bg, 
                    algorithm = 'maxnet', partitions = 'randomkfold',
                    tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))

overlap <- calc.niche.overlap(e.mx@predictions, overlapStat = "D")

svg("./fig/1970/evalplot.svg")
evalplot.stats(e = e.mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")
dev.off()

res <- eval.results(e.mx)
opt.aicc <- res %>% filter(delta.AICc == 0)
opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))

svg("./fig/1970/mod_seq.svg")
mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]
plot(mod.seq, type = "cloglog")
dev.off()

svg("./fig/1970/mod_simple.svg")
mod.simple <- eval.models(e.mx)[['fc.L_rm.5']]
plot(mod.simple, type = "cloglog")
dev.off()

svg("./fig/1970/mod_complex.svg")
mod.complex <- eval.models(e.mx)[['fc.LQH_rm.1']]
plot(mod.complex, type = "cloglog")
dev.off()

svg("./fig/1970/pred.svg")
pred.seq <- eval.predictions(e.mx)[[opt.seq$tune.args]]
plot(pred.seq)
dev.off()

svg("./fig/1970/pred_occ.svg")
plot(pred.seq)
points(eval.bg(e.mx), pch = 3, col = eval.bg.grp(e.mx), cex = 0.1)
points(eval.occs(e.mx), pch = 21, bg = eval.occs.grp(e.mx), cex = 0.8)
dev.off()

svg("./fig/1970/fc.LQH.svg")
par(mfrow=c(2,2))
plot(eval.predictions(e.mx)[['fc.LQH_rm.1']],
     legend = FALSE, main = 'LQH_1 prediction')
plot(eval.predictions(e.mx)[['fc.LQH_rm.2']],
     legend = FALSE, main = 'LQH_2 prediction')
plot(eval.predictions(e.mx)[['fc.LQH_rm.3']],
     legend = FALSE, main = 'LQH_3 prediction')
plot(eval.predictions(e.mx)[['fc.LQH_rm.4']],
     legend = FALSE, main = 'LQH_4 prediction')
dev.off()

mod.null <- ENMnulls(e.mx, mod.settings = list(fc = "LQ", rm = 5), no.iter = 10)

svg("./fig/1970/hist_nullmod.svg")
evalplot.nulls(mod.null, stats = c("or.10p", "auc.val"), plot.type = "histogram")
dev.off()

svg("./fig/1970/violin_nullmod.svg")
evalplot.nulls(mod.null, stats = c("or.10p", "auc.val"), plot.type = "violin")
dev.off()
