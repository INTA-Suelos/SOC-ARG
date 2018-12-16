setwd("~/GSOC-Argentina/submission/")


# Graph Fig scatterplots observed vs. predicted

library(lattice)
library(latticeExtra)
library(reshape2)


dat <- read.csv("data/validation.csv")

coordinates(dat) <- ~ X + Y
sph <- raster("results/OCSKGM2015.tif")
spnh <- raster("results/OCSKGM2015_no_tempharm.tif")

dat$sph <- extract(sph, dat)
dat$spnh <- extract(spnh, dat)

a <- dat[,c("OCSKGM", "sph")]
b <- dat[,c("OCSKGM", "spnh")]

a$model <- "sp h"
b$model <- "sp nh"

names(a)[2] <- "pred"
names(b)[2] <- "pred"

d <- rbind(a@data, b@data)

names(d) <- c("Observed", "Predicted", "Model")
melt(d)


png(filename = "Fig5.png",
   width = 1500, height = 1000, res =  300)
xyplot(Predicted ~ Observed | Model, data=d,  abline=c(0,1),
               type=c('p', "g"),asp = 1,.aspect.ratio = 1, 
               default.scales = list(tick.number=3, tck = 1, minlength = 3),
               scales = list(alternating= FALSE,
                             limits=c(0,13)),
       strip = strip.custom(factor.levels = c(expression(italic(sp_h)),
                                              expression(italic(sp_nh)))),
               par.settings=list(grid.pars=list(fontfamily="serif")),
               pch = ".", cex = 3, alpha = 0.5, col = "black",
               xlab = "Observed", ylab = "Predicted"
)
dev.off()
