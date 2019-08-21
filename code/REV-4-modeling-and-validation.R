setwd("~/Documents/SOC-ARG/")

#### Import point data and covariates ####

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


#### Fitting models ####

library(randomForest)
library(doParallel)

models <- list()


for(i in 1:10){
  print(names(dat[9+i]))
  training <- dat
  fm = as.formula(paste(names(dat[9+i]), "~", paste0(names(covs),
                                           collapse = "+")))
  cl <- makeCluster(detectCores(), type='PSOCK')
  registerDoParallel(cl)
  control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
  set.seed(1)
  training <- training[!is.na(training[,9+i]),]
  rfmodel <- rfe(x=training[,names(covs)], y=training[,9+i], sizes=2^(2:4), rfeControl=control2)
  # testing$residuals <- testing$OCSKGM - predict(rfmodel, testing)
  stopCluster(cl = cl)
  models[[i]] <- rfmodel
  names(models)[[i]] <- names(dat[9+i])
}

### Without temp harmonization
training <- dat
fm = as.formula(paste("OCSKGM", "~", paste0(names(covs),
                                                   collapse = "+")))
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
set.seed(1)
training <- training[!is.na(training[,5]),]
rfmodel <- rfe(x=training[,names(covs)], y=training[,5], sizes=2^(2:4), rfeControl=control2)
# testing$residuals <- testing$OCSKGM - predict(rfmodel, testing)
stopCluster(cl = cl)
models[[11]] <- rfmodel
names(models)[[11]] <- names(dat[5])



save(models, file="models.RData")

load("models.RData")

#### Validation ####
val <- as.data.frame(val)

val$Date <- strptime(val$Date, format =  "%Y-%m-%d")

### without temporal harmonization
val$predNH <- predict(models$OCSKGM, val)

### with temporal harmonization
val$year <- format(val$Date, "%Y")
val$year <- as.numeric(val$year)

years <- 2006:2015

val$year[val$year > 2015] <- 2015

for(i in 1:10){
  print(i)
  val$predH[val$year == years[i]] <- predict(models[[i]], val[val$year == years[i],])
}


#### Validation statistics
library(DescTools)

## NH
val$observed <- val$OCSKGM
val$residual <- val$predNH - val$observed

### automap
out = list()
# mean error, ideally 0:
out$mean_error = mean(val$residual)
# correlation observed and predicted, ideally 1
out$cor_obspred = cor(val$observed, val$observed - val$residual)
# correlation predicted and residual, ideally 0
out$cor_predres = cor(val$observed - val$residual, val$residual)
# RMSE, ideally small
out$RMSE = sqrt(sum(val$residual^2) / length(val$residual))
# AVE
out$AVE <- 1 - sum(val$residual^2, na.rm=TRUE) / 
  sum( (val$observed - mean(val$observed, na.rm = TRUE))^2, 
       na.rm = TRUE)
# CCC
tmp.ccc <- CCC(val$observed, val$predNH, ci = "z-transform", conf.level = 0.95)
out$CCC <- mean(tmp.ccc$blalt$delta)
out$CCC.sd <- sqrt(var(tmp.ccc$blalt$delta))

out <- unlist(out)
out

# mean_error cor_obspred cor_predres        RMSE         AVE         CCC      CCC.sd 
# 1.6630461   0.8399918  -0.8259728   3.2863046  -0.2454508  -1.6630461   2.8350676 

## TH
val$observed <- val$OCSKGM
val$residual <- val$predH - val$observed

### automap
out = list()
# mean error, ideally 0:
out$mean_error = mean(val$residual)
# correlation observed and predicted, ideally 1
out$cor_obspred = cor(val$observed, val$observed - val$residual)
# correlation predicted and residual, ideally 0
out$cor_predres = cor(val$observed - val$residual, val$residual)
# RMSE, ideally small
out$RMSE = sqrt(sum(val$residual^2) / length(val$residual))
# AVE
out$AVE <- 1 - (sum(val$residual^2, na.rm=TRUE) / 
  sum( (val$observed - mean(val$observed, na.rm = TRUE))^2, 
       na.rm = TRUE))
# CCC
tmp.ccc <- CCC(val$observed, val$predH, ci = "z-transform", conf.level = 0.95)
out$CCC <- mean(tmp.ccc$blalt$delta)
out$CCC.sd <- sqrt(var(tmp.ccc$blalt$delta))

out <- unlist(out)
out

# mean_error cor_obspred cor_predres        RMSE         AVE         CCC      CCC.sd 
# 0.8613567   0.8683793  -0.8400517   2.8262056   0.0788752  -0.8613567   2.6923390 

write.csv(val, "results/REV-validation.csv", row.names = F)
