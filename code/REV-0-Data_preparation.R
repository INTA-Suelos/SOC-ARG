library(raster)
library(GSIF)
library(aqp)
library(plyr)
library(rgdal)
library(sp)
library(gstat)


#Carpeta de trabajo (en windows algo as?? como c:/CDS.., en UNIX /home..)
#En la carpeta de arriba, debe estar presente el archivo spline_functions.RData
setwd("~/Documents/SOC-ARG/")


input <- read.csv("data/Splines_CO.csv")
input <- input[!is.na(input$CO_0.30),]
input$Date <- strptime(input$Date, "%Y-%m-%d")
input$top <- 0
input$bottom <- 30
input$type = "SP"

### BLD

landcover <- raster("~/Dropbox/globalBLD2/globcover2.3/GLOBCOVER_L4_200901_200912_V2.3.tif")
names(landcover) <- "landcover"
meantemp <- raster("~/Documents/worldclim/wc2.0_bio_30s_01.tif")
names(meantemp) <- "meantemp"
precip <- raster("~/Documents/worldclim/wc2.0_bio_30s_12.tif")
names(precip) <- "precip"

load("BLDrfmodel.RData")

coordinates(input) <- ~ X + Y
input@proj4string <- landcover@crs

# Extract covariates values 
input <- extract(landcover, input, sp = T)
input <- extract(meantemp, input, sp = T)
input <- extract(precip, input, sp = T)

# filter rows where OC and covariates aren't NA
input <- as.data.frame(input[!(is.na(input$CO_0.30) | is.na(input$landcover) | 
                             is.na(input$meantemp) | is.na(input$precip)),])

names(input)[5] <- "OC"
input$BD <- NA
## workaround for problems related to factor levels in randomForest
dat$landcover <- as.numeric(as.character(dat$landcover))
cols <- c("OC", "BD", "meantemp", "precip", "landcover")
input2 <- rbind(input[,cols], dat[,cols])
input2$landcover <- factor(input2$landcover)
input2 <- input2[1:nrow(input),]


input$BD <- predict(model.rf, input2)

#### CRF

CRF1 <- raster("CRFVOL_M_sl1_1km_Argentina.tiff")
CRF2 <- raster("CRFVOL_M_sl2_1km_Argentina.tiff")
CRF3 <- raster("CRFVOL_M_sl3_1km_Argentina.tiff")
CRF4 <- raster("CRFVOL_M_sl4_1km_Argentina.tiff")

CRF <- mean(CRF1, CRF2, CRF3, CRF4)
names(CRF) <- "CRF"

coordinates(input) <- ~ X+ Y

input <- extract(CRF, input, sp = T)

input <- as.data.frame(input)

##### OCSKGM calculation #####


library(GSIF)
input$ORCDRC <- input$OC*10

input$ORCDRC[input$ORCDRC == 0] <- NA
temp <- OCSKGM(ORCDRC = input$ORCDRC, BLD = input$BD*1000,
               CRFVOL = input$CRF, HSIZE = input$bottom-input$top)
input$OCSKGM <- temp
input$meaERROR <- attr(temp,"measurementError")



## Agregar otros datos con dato de stock
input <- input[,c(1,2,3,4,13,7)]
input$type = "SP"

stocks <- read.csv("Base_Andriulo+Carina_Alvarez_CO_Stock.csv")
stocks$año.muestro <- paste0(stocks$año.muestro, "-01-01")
stocks$año.muestro <- strptime(stocks$año.muestro, "%Y-%m-%d")
stocks$COS..30..Mg.ha. <- stocks$COS..30..Mg.ha. / 10
stocks <- stocks[,c(1,4,3,5,2,6)]
stocks$type <- "TS"
names(stocks) <- names(input)


input <- rbind(input, stocks)
input$top <- 0
input$bottom <- 30

input <- input[complete.cases(input),]
#input <- input[input$OCSKGM > 0,]

#Soil profile collection


############ Prepare validation dataset
validation <- input[input$Date>'2014-01-01' & input$Date<'2016-12-31',]
input <- input[input$Date<'2014-01-01' | input$Date>'2016-12-31',]

write.csv(validation, "validation/val_141516.csv")

################

input.aqp <- input
depths(input.aqp) <- ID ~ top + bottom
input <- input[!is.na(input$X), ]
coordinates(input) <- ~ X + Y


### Descriptive analysis

hist(input$OCSKGM[input$OCSKGM< 5], breaks = 100)
summary(input$OCSKGM)
hist(log(input$OCSKGM[input$OCSKGM< 5]), breaks = 100)

## ADM limits
lim=readRDS("ARG_adm2.rds")



bubble(input, "OCSKGM", sp.layout = lim)


################################################################################
#### Covariates preparation ####

## MODIS stacks
library(raster)
# covs <- stack("stack1000.tif")
#names(covs) <- readRDS("namesStack1000.rds")

files <- list.files(path = "Layers/", pattern = "tif$", full.names = T)
covs <- stack(files)



#### Add covariates to data ####
# Convert to spatial points df and project
input@proj4string <- CRS("+init=epsg:4326")

input@data <- cbind(input@data, extract(covs, input))


summary(input@data)
input@data <- input@data[,-c(19, 32)]


write.csv(as.data.frame(input), "ARGregMatrix_covsLayers.csv")

