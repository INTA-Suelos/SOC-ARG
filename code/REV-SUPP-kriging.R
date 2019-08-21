setwd("~/Documents/SOC-ARG/")

library(raster)
library(gstat)

pred <- raster("results/REV-prediction.tif")
dat <- read.csv("data/REV-alldata.csv")

coordinates(dat) <- ~ X + Y

dat <- extract(pred, dat, sp = TRUE)

dat$res <- dat$OCSKGM2015 - dat$REV.prediction

dat <- dat[!is.na(dat$res),]

var <- variogram(res~1, dat)
