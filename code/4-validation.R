setwd("~/GSOC-Argentina/submission/")

library(raster)

pred <- raster("results/OCSKGM2015.tif")

dat <- read.csv("data/validation.csv")

dat <- dat[complete.cases(dat),]

coordinates(dat) <- ~ X + Y

library(Metrics)
dat$pred <- extract(pred, dat)

dat$observed <- dat$OCSKGM
dat$residual <- dat$observed - dat$pred

### automap
out = list()
# mean error, ideally 0:
out$mean_error = mean(dat$residual)
# correlation observed and predicted, ideally 1
out$cor_obspred = cor(dat$observed, dat$observed - dat$residual)
# correlation predicted and residual, ideally 0
out$cor_predres = cor(dat$observed - dat$residual, dat$residual)
# RMSE, ideally small
out$RMSE = sqrt(sum(dat$residual^2) / length(dat$residual))
# AVE
out$AVE <- 1 - sum(dat$residual^2, na.rm=TRUE) / 
  sum( (dat$observed - mean(dat$observed, na.rm = TRUE))^2, 
       na.rm = TRUE)



out <- unlist(out)
out

# con umsef
# mean_error cor_obspred cor_predres        RMSE         AVE 
# -0.02534178  0.54507750 -0.14980483  1.85314094  0.28083897


#### TIMELESS MODEL ##### 

library(raster)

pred <- raster("results/OCSKGM2015_no_tempharm.tif")

dat <- read.csv("data/validation.csv")


dat <- dat[complete.cases(dat),]

coordinates(dat) <- ~ X + Y

library(Metrics)
dat$pred <- extract(pred, dat)


dat$observed <- dat$OCSKGM
dat$residual <- dat$observed - dat$pred



out = list()
# mean error, ideally 0:
out$mean_error = mean(dat$residual)
# correlation observed and predicted, ideally 1
out$cor_obspred = cor(dat$observed, dat$observed - dat$residual)
# correlation predicted and residual, ideally 0
out$cor_predres = cor(dat$observed - dat$residual, dat$residual)
# RMSE, ideally small
out$RMSE = sqrt(sum(dat$residual^2) / length(dat$residual))
# AVE
out$AVE <- 1 - sum(dat$residual^2, na.rm=TRUE) / 
  sum( (dat$observed - mean(dat$observed, na.rm = TRUE))^2, 
       na.rm = TRUE)



out <- unlist(out)
out

