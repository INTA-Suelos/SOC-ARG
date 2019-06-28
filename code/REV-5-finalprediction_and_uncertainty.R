setwd("~/Documents/SOC-ARG/")

#### Import point data and covariates ####

set.seed(123)

library(caret)
# Load data
dat <- read.csv("data/REV-alldata.csv")

dat$Date <- strptime(dat$Date, format =  "%Y-%m-%d")

library(sp)

# Promote to spatialPointsDataFrame
coordinates(dat) <- ~ X + Y
dat@proj4string <- CRS(projargs = "+init=epsg:4326")

library(raster)

covs <- stack("stack1000.tif")
names(covs) <- readRDS("namesStack1000.rds")

covs[[24]] <- as.factor(covs[[24]])

dat <- extract(covs, dat, sp=TRUE)
dat <- as.data.frame(dat)

dat <- dat[complete.cases(dat),]


### Fit model ####
library(randomForest)
library(doParallel)

training <- dat
fm = as.formula(paste("OCSKGM2015", "~", paste0(names(covs),
                                                collapse = "+")))
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
set.seed(1)
training <- training[!is.na(training[,10]),]
rfmodel <- rfe(x=training[,names(covs)], y=training[,10], sizes=2^(2:4), rfeControl=control2)
# testing$residuals <- testing$OCSKGM - predict(rfmodel, testing)
stopCluster(cl = cl)

prediction <- predict(covs, rfmodel)

writeRaster(prediction, "results/REV-prediction.tif")


