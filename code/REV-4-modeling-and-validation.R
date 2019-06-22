setwd("~/Documents/SOC-ARG/")

set.seed(123)

library(caret)
# Load data
dat <- read.csv("data/REV-cal.csv")

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


## load validation data
val <- read.csv("data/REV-val.csv")
# Promote to spatialPointsDataFrame
coordinates(val) <- ~ X + Y
val@proj4string <- CRS(projargs = "+init=epsg:4326")
val <- extract(covs, val, sp=TRUE)


library(randomForest)
library(doParallel)

models <- list()


for(i in 1:10){
  print(names(dat[9+i]))
  inTrain <- createDataPartition(y = dat$OCSKGM, p = .80, list = FALSE)
  training <- dat[ inTrain,]
  testing <- dat[-inTrain,]
  fm = as.formula(paste(names(dat[9+i]), "~", paste0(names(covs),
                                           collapse = "+")))
  cl <- makeCluster(detectCores(), type='PSOCK')
  registerDoParallel(cl)
  control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, repeats=10)
  set.seed(1)
  training <- training[!is.na(training[,9+i]),]
  rfmodel <- rfe(x=training[,names(covs)], y=training[,9+i], sizes=2^(2:4), rfeControl=control2)
  # testing$residuals <- testing$OCSKGM - predict(rfmodel, testing)
  stopCluster(cl = cl)
  models[[i]] <- rfmodel
  names(models)[[i]] <- names(dat[9+i])
}

save(models, file="models.RData")









