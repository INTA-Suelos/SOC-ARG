setwd("~/Documents/SOC-ARG//")
library(raster)

pred <- raster("results/REV-prediction.tif")
mask <- raster("results/mask.tif")


mask[!is.na(mask)] <- 10000
mask[is.na(mask)] <- 1
mask[mask == 10000] <- NA

pred <- mask(pred, mask)

pred.proj <- projectRaster(pred, crs=CRS("+init=epsg:22173"), res=1000)

pred.proj

sum(values(pred.proj), na.rm = T)
