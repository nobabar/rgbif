rm(list=ls())


library(raster)
library(tidyverse)


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

tmin_mean_1970 <- calc(tmin_stack_1970, mean)
tmax_mean_1970 <- calc(tmax_stack_1970, mean)
tmean_1970 = (tmin_mean_1970 + tmax_mean_1970) / 2
prec_mean_1970 <- calc(prec_stack_1970, mean)


par(mfrow=c(2, 2))
plot(tmin_mean, main="minimal temperatures")
plot(tmax_mean, main="maximal temperatures")
plot(tmean, main="average temperatures")
plot(prec_mean, main="average precipiration")
par(mfrow=c(1, 1))