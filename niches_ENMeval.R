# qsub -cwd -V -N niches_ENMeval -pe thread 10 -b y "Rscript niches_ENMeval.R"

library(ENMeval)
library(raster)
library(dplyr)
library(data.table)


occs_2010 <- fread("~/save/data/occurrences/occs_picus_2010.txt",
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

occs_z <- cbind(pts_2010, raster::extract(predictors, pts_2010))

occs.sim <- similarity(predictors, occs_z)

occs.mess <- occs.sim$similarity_min

occs.sp <- sp::SpatialPoints(pts_2010)

rasterVis::levelplot(occs.mess, main = "Environmental similarity", margin = FALSE) + 
     latticeExtra::layer(sp.points(occs.sp, col="black"))

myScale <- seq(cellStats(occs.mess, min), cellStats(occs.mess, max), length.out = 100)
rasterVis::levelplot(occs.mess, main = "Environmental similarity", at = myScale, margin = FALSE) + 
  latticeExtra::layer(sp.points(occs.sp, col="black"))

bg <- dismo::randomPoints(predictors, 10000)
colnames(bg) <- colnames(pts_2010)

bg_z <- cbind(bg, raster::extract(predictors, bg))


# partitioning using random k-fold ----

# rand <- get.randomkfold(pts_2010, bg, k = 5)
# evalplot.grps(pts = pts_2010, pts.grp = rand$occs.grp, envs = predictors)
# 
# evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = occs_z, 
#                      bg.z = bg_z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp)
# 
# evalplot.envSim.map(sim.type = "mess", ref.data = "occs", envs = predictors, occs.z = occs_z, 
#                     bg.z = bg_z, occs.grp = rand$occs.grp, bg.grp = rand$bg.grp, 
#                     bb.buf = 7)

e.mx <- ENMevaluate(occs = pts_2010, envs = predictors, bg = bg, 
                    algorithm = 'maxnet', partitions = 'randomkfold',
                    tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))

# e.mx <- ENMevaluate(occs = pts_2010, envs = predictors, bg = bg, 
#                     algorithm = 'maxnet', partitions = 'randomkfold',
#                     tune.args = list(fc = c("L","LQ"), rm = 1:5))
# 
# e.mx <- ENMevaluate(occs = pts_2010, envs = predictors, bg = bg, 
#                     algorithm = 'maxent.jar', partitions = 'randomkfold',
#                     tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))

overlap <- calc.niche.overlap(e.mx@predictions, overlapStat = "D")

svg("~/save/fig/picus/2010/evalplot.svg")
evalplot.stats(e = e.mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")
dev.off()

res <- eval.results(e.mx)
opt.aicc <- res %>% filter(delta.AICc == 0)
opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))

svg("~/save/fig/picus/2010/mod_seq.svg")
mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]
plot(mod.seq, type = "cloglog")
dev.off()

svg("~/save/fig/picus/2010/mod_simple.svg")
mod.simple <- eval.models(e.mx)[['fc.L_rm.5']]
plot(mod.simple, type = "cloglog")
dev.off()

svg("~/save/fig/picus/2010/mod_complex.svg")
mod.complex <- eval.models(e.mx)[['fc.LQH_rm.1']]
plot(mod.complex, type = "cloglog")
dev.off()

svg("~/save/fig/picus/2010/pred.svg")
pred.seq <- eval.predictions(e.mx)[[opt.seq$tune.args]]
plot(pred.seq)
dev.off()

svg("~/save/fig/picus/2010/pred_occ.svg")
plot(pred.seq)
points(eval.bg(e.mx), pch = 3, col = eval.bg.grp(e.mx), cex = 0.1)
points(eval.occs(e.mx), pch = 21, bg = eval.occs.grp(e.mx), cex = 0.8)
dev.off()

svg("~/save/fig/picus/2010/fc.LQH.svg")
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

svg("~/save/fig/picus/2010/hist_nullmod.svg")
evalplot.nulls(mod.null, stats = c("or.10p", "auc.val"), plot.type = "histogram")
dev.off()

svg("~/save/fig/picus/2010/violin_nullmod.svg")
evalplot.nulls(mod.null, stats = c("or.10p", "auc.val"), plot.type = "violin")
dev.off()
