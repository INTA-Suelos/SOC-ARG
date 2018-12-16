setwd("~/GSOC-Argentina/submission/")

set.seed(123)

library(caret)
# Load data
dat <- read.csv("data/data_temporalharm.csv")

dat$Date <- strptime(dat$Date, format =  "%Y-%m-%d")
dat <- dat[dat$Date < "2014-01-01",]

dat$OCSKGM <- dat$OCSKGM2015

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

writeRaster(predRFE, file="results/OCSKGM2015.tif", 
            overwrite=TRUE)


endCluster()

rfmodel


###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

imp <- as.data.frame(rfmodel$fit$importance)
imp$var <- row.names(imp)
names(imp)[1] <- "IncMSE"

ggplot(imp, aes(x=reorder(var, IncMSE), y=IncMSE,fill=IncMSE))+ 
     geom_bar(stat="identity", position="dodge")+ coord_flip()+
     ylab("Variable Importance")+
     xlab("")+
     ggtitle("Information Value Summary")+
     guides(fill=F)+
     scale_fill_gradient(low="red", high="blue")




################################################# uncertainty assesment ####


library(quantregForest)

model <- quantregForest(y=testing$residuals, x=testing[,2:32], ntree=500, keep.inbag=TRUE)

beginCluster(6,type="SOCK")

#Estimate model uncertainty

unc <- clusterR(covs[[names(testing[,2:32])]], predict, args=list(model=model,what=sd))



plot(unc, col=colorRampPalette(c("gray", "brown", "blue"))(255))

endCluster()

unc <- mask(unc, predRFE)

writeRaster(unc, "results/qrf_residuals.tif")


### Sensitivity

library(Metrics)

#Generate an empty dataframe
library(caret)
validation <- data.frame(rmse=numeric(), r2=numeric())

pred <- predRFE
ctrl <- trainControl(method = "cv", savePred=T)

#Sensitivity to the dataset

#Start a loop with 10 model realizations

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

for (i in 1:10){
  # We will build 10 models using random samples of 25%
  smp_size <- floor(0.25 * nrow(dat))
  train_ind <- sample(seq_len(nrow(dat)), size = smp_size)
  train <- dat[train_ind, ]
  test <- dat[-train_ind, ]
  modn <- caret::train(fm, data=train@data, method = "rf", trControl = ctrl)
  
  pred <- stack(pred, predict(covs, modn))
  test$pred <- extract(pred[[i+1]], test)
  # Store the results in a dataframe
}

#The sensitivity map is the dispersion of all individual models

sensitivity <- calc(pred[[-1]], sd)

sensitivity <- mask(sensitivity, predRFE)

writeRaster(sensitivity, file="results/sensitivity.tif", 
            overwrite=TRUE)

# The total uncertainty is the sum of sensitivity and model

# uncertainty

sum_unc <- unc + sensitivity

plot(sum_unc, col=colorRampPalette(c("gray", "brown", "blue"))(255))

writeRaster(sum_unc, file="summed_uncertainty.tif", overwrite=TRUE)
