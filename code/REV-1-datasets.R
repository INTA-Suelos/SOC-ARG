setwd("~/Documents/SOC-ARG/")

library(raster)

dat <- read.csv("data/data.csv")
val <- read.csv("data/validation.csv")

cols <- names(val)

cols <- cols[c(1:5, 7:8)]

dat <- rbind(dat[, cols], val[, cols])

dat$Date <- as.Date(dat$Date)


nrow(dat[dat$Date > "2005-01-01",])

set.seed(123)

val <- dat[dat$Date > "2005-01-01",]
library(sp)
coordinates(val) <- ~ X + Y
plot(val)

cal <-dat[dat$Date < "2005-01-01",]
library(sp)
coordinates(cal) <- ~ X + Y
plot(cal)

pred <- dat

# 
# candidates <- dat[dat$Date > "2005-01-01",]
# 
# library(caret)
# train.ind <- createDataPartition(1:nrow(candidates), p = .5, list = FALSE)
# train <- candidates[ train.ind,]
# 
# calib <- candidates[train.ind,]
# val  <- candidates[-train.ind,]
# library(sp)
# coordinates(val) <- ~ X + Y
# plot(val)

hist(val, breaks = 24)





