setwd("~/Documents/SOC-ARG/")

library(raster)

dat <- read.csv("data/REV_alldata.csv")

dat$Date <- as.Date(dat$Date)


nrow(dat[dat$Date > "2005-01-01",])

val <- dat[dat$Date > "2005-01-01",]
library(sp)
coordinates(val) <- ~ X + Y
plot(val)

cal <-dat[dat$Date < "2005-01-01",]
library(sp)
coordinates(cal) <- ~ X + Y
plot(cal)

pred <- dat

write.csv(cal, "data/REV-cal.csv", row.names = F)
write.csv(val, "data/REV-val.csv", row.names = F)

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





