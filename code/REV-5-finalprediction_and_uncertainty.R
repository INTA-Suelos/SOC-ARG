setwd("~/Documents/SOC-ARG/")

#### Import point data and covariates ####

set.seed(123)

library(caret)
# Load data
dat <- read.csv("data/REV-alldata.csv")

dat$Date <- strptime(dat$Date, format =  "%Y-%m-%d")wir

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

save(rfmodel, file ="results/final_prediction_model.RData")
writeRaster(prediction, "results/REV-prediction.tif")

imp <- as.data.frame(rfmodel$fit$importance)
imp$var <- row.names(imp)
names(imp)[1] <- "IncMSE"

library(ggplot2)

ggplot(imp, aes(x=reorder(var, IncMSE), y=IncMSE,fill=IncMSE))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  theme(text=element_text(size=12,  family="serif"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  theme(panel.spacing = unit(2.5, "lines")) + 
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")









pred.proj <- projectRaster(prediction, crs=CRS("+init=epsg:22173"), res=1000)
pred.proj
sum(values(pred.proj), na.rm = T)
prof_covsprof_covs
