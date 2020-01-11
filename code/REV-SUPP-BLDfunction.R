bdfiod <- read.csv("~/Dropbox/globalBLD2/WOSIS_latest/wosis_latest_bdfiod.csv")
clay <- read.csv("~/Dropbox/globalBLD2/WOSIS_latest/wosis_latest_clay.csv")
clay$clay_value_avg <- as.numeric(as.character(clay$clay_value_avg))

orgc <- read.csv("~/Dropbox/globalBLD2/WOSIS_latest/wosis_latest_orgc.csv")
sand <- read.csv("~/Dropbox/globalBLD2/WOSIS_latest/wosis_latest_sand.csv")
silt <- read.csv("~/Dropbox/globalBLD2/WOSIS_latest/wosis_latest_silt.csv")
silt$silt_value_avg <- as.numeric(as.character(silt$silt_value_avg))

wosis <- merge(bdfiod,  clay, by = "profile_layer_id")
wosis <- merge(wosis, orgc, by = "profile_layer_id")
wosis <- merge(wosis, sand, by = "profile_layer_id")
wosis <- merge(wosis, silt, by = "profile_layer_id")

wosis <- wosis[,unique(names(wosis))]

cols <- c("profile_layer_id", "geom.y", "upper_depth.x", "bdfiod_value_avg", 
          "clay_value_avg", "orgc_value_avg", "sand_value_avg", "silt_value_avg")

wosis <- wosis[,cols]
nrow(wosis)

#filter row with valid wkt field
library(sf)
wosis <- wosis[grep("POINT", wosis$geom.y),]

wosis <- st_as_sf(wosis, wkt = "geom.y")
wosis <- as(wosis, "Spatial")
## X Y are swapped
wosis <- as.data.frame(wosis)
library(sp)
coordinates(wosis) <- ~ coords.x2 + coords.x1
wosis@proj4string <- CRS("+init=epsg:4326")
plot(wosis)


library(raster)
landcover <- raster("~/Dropbox/globalBLD2/globcover2.3/GLOBCOVER_L4_200901_200912_V2.3.tif")
names(landcover) <- "landcover"

wosis <- extract(landcover, wosis, sp = T)

wosis$landcover <- factor(wosis$landcover)

meantemp <- raster("~/Documents/worldclim/wc2.0_bio_30s_01.tif")
names(meantemp) <- "meantemp"
wosis <- extract(meantemp, wosis, sp = T)

precip <- raster("~/Documents/worldclim/wc2.0_bio_30s_12.tif")
names(precip) <- "precip"
wosis <- extract(precip, wosis, sp = T)

soiltypes <- raster("~/Dropbox/globalBLD2/")


wosis <- as.data.frame(wosis)
names(wosis) <- c("ID", "top", "BD", "CLAY", "OC", "SAND", "SILT", "X", "Y", 
                  "landcover", "meantemp", "precip")

wosis <- wosis[complete.cases(wosis),]
summary(wosis)


library(caret)
inTrain <- createDataPartition(y = wosis$BD, p = .6, list = FALSE)
dat.training <- wosis[inTrain,] # n 39451
dat.validation <- wosis[-inTrain,] # n 13148
dat <- dat.training
dat <- dat[complete.cases(dat),]
nrow(dat.training)
nrow(dat.validation)

## Fitting and tuning the randomForest model


library(randomForestSRC)
library(randomForest)
library(Metrics)

fm <- BD ~ OC  + meantemp + precip + landcover

# ctrl <- trainControl(method = "cv", savePred=T, number = 10)
# 
# model.rf <- train(fm, data=dat, method = "rf", trControl = ctrl, 
#                  importance=TRUE) # wait for it..... 


model.rf <- randomForest(fm, data = dat, ntree = 150)

model.rf

save(model.rf, dat, file = "BLDrfmodel.RData")