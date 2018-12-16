setwd("~/GSOC-Argentina/submission/")


pred <- raster("results/OCSKGM2015.tif")

pred

pred.proj <- projectRaster(pred, crs=CRS("+init=epsg:22173"), res=1000)

pred.proj

sum(values(pred.proj), na.rm = T)
