setwd("~/GSOC-Argentina/submission/")

set.seed(123)

library(caret)
# Load data
dat <- read.csv("data/data_temporalharm.csv")

dat$Date <- strptime(dat$Date, format =  "%Y-%m-%d")
dat <- dat[dat$Date < "2014-01-01",]


library(sp)

# Promote to spatialPointsDataFrame
coordinates(dat) <- ~ X + Y

dat@proj4string <- CRS(projargs = "+init=epsg:4326")

library(raster)

covs <- stack("covariates/stack1000.tif")
names(covs) <- readRDS("covariates/namesStack1000.rds")

covs[[24]] <- as.factor(covs[[24]])


# For its use on R we need to define a model formula
dat@data <- dat@data[c("OCSKGM", names(covs))]

dat <- dat[complete.cases(dat@data),]


inTrain <- createDataPartition(y = dat@data$OCSKGM, p = .80, list = FALSE)
training <- dat@data[ inTrain,]
testing <- dat@data[-inTrain,]

fm = as.formula(paste("OCSKGM ~", paste0(names(covs),
                                         collapse = "+")))

library(randomForest)


library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, repeats=10)

names(dat)
set.seed(1)
(rfmodel <- rfe(x=training[,2:32], y=training[,1], sizes=2^(2:4), rfeControl=control2))

testing$residuals <- testing$OCSKGM - predict(rfmodel, testing)
stopCluster(cl = cl)

## Mapa de valos mas probable
beginCluster()

library(randomForest)


(predRFE <- clusterR(covs, predict, args=list(model=rfmodel)))

plot(predRFE, col=colorRampPalette(c("gray", "brown", "blue"))(255))

writeRaster(predRFE, file="results/OCSKGM2015_no_tempharm.tif", 
            overwrite=TRUE)


endCluster()

rfmodel