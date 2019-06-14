setwd("~/Documents/SOC-ARG/")

library(raster)

dat <- read.csv("data/data.csv")
val <- read.csv("data/validation.csv")

cols <- names(val)

cols <- cols[c(1:5, 7:8)]

dat <- rbind(dat[, cols], val[, cols])

dat <- dat[complete.cases(dat),]
dat$Date <- as.character(dat$Date)
dat$Date <- strptime(dat$Date, format='%Y-%m-%d')

dat <- dat[dat$Date > "1901-01-01",]

dat$Date[dat$Date > "2018-01-01"] <- "2017-02-22"

years <- as.POSIXlt(dat$Date)$year + 1900
hist(years,  xlab="Year", breaks =63,  main="", col= "brown", freq=TRUE, las = 2, cex.axis=0.7, lab=c(1,5,7))


################################################################################



factors <- readRDS("data/IPCC_factors.rds")


############# point data ####
dat_sp <- dat
coordinates(dat_sp) <- ~ X + Y
dat_sp@proj4string <- factors@proj4string

factors <- over(dat_sp, factors)

# load lu change according to UMSEF
nat_to_agric <- readRDS(file = "data/umsef.rds")
factors$desmonte <- nat_to_agric$ANIO_DESMO

##################################################################################### Distribuir ese 



############ Expand IPCC factor matrix #####
IPCCfactors <- list()
IPCCfactors$FI <- matrix(ncol=63, nrow=nrow(factors),
                         data=c(rep((factors$FI), 20),
                                rep((factors$FI)*0+1, 13),
                                rep((factors$FI), 30)))  ### distribute the factors between the years
colnames(IPCCfactors$FI ) <- 1955:2017

IPCCfactors$FMG <- matrix(ncol=63, nrow=nrow(factors),
                   data=c(rep((factors$FMG5587), 47),   ### acordamos considerar SD desde el 2001/2002
                          rep((factors$FMG8817), 16)))  ### distribute the factors between the years
colnames(IPCCfactors$FMG ) <- 1955:2017

IPCCfactors$FLU <- matrix(ncol=63, nrow=nrow(factors),
                          data=c(rep((factors$FLU5587), 20),
                                 rep((factors$FLU5587)*0+1, 13),
                                 rep((factors$FLU8817), 20),
                                 rep((factors$FLU8817)*0+1, 10)))  ### distribute the factors between the years
colnames(IPCCfactors$FLU ) <- 1955:2017

### Add desmonte to FLU
for(i in 1:nrow(factors)){
  if(!is.na(factors$desmonte[i])){
    IPCCfactors$FLU[i,] <- 0
    top <- ifelse(factors$desmonte[i]-1955+1+20 >63,63, factors$desmonte[i]-1955+1+20)
    IPCCfactors$FLU[i,(factors$desmonte[i]-1955+1) : top] <- 0.69 ### (0.69)/20
  }
}


### final factor
IPCCfactors$TOTAL <- IPCCfactors$FI * IPCCfactors$FMG * IPCCfactors$FLU
colnames(IPCCfactors$TOTAL ) <- 1955:2017


temporal <- function(dat, year, factor){
  result <- vector()
  for(i in 1:nrow(dat)){
    ff <-rev(factor[i, (as.character(year : years[i]))])
    if(length(ff) > 20){ff <- head(ff, 20)}  
    loss <- (dat$OCSKGM[i] - ff * dat$OCSKGM[i])/20
    result[i] <- dat$OCSKGM[i] - sum(loss)
  }
  return(result)
}

dat$OCSKGM2015 <- temporal(dat, 2015, IPCCfactors$TOTAL)
dat$OCSKGM2014 <- temporal(dat, 2014, IPCCfactors$TOTAL)
dat$OCSKGM2013 <- temporal(dat, 2013, IPCCfactors$TOTAL)
dat$OCSKGM2012 <- temporal(dat, 2012, IPCCfactors$TOTAL)
dat$OCSKGM2011 <- temporal(dat, 2011, IPCCfactors$TOTAL)
dat$OCSKGM2010 <- temporal(dat, 2010, IPCCfactors$TOTAL)
dat$OCSKGM2009 <- temporal(dat, 2009, IPCCfactors$TOTAL)
dat$OCSKGM2008 <- temporal(dat, 2008, IPCCfactors$TOTAL)
dat$OCSKGM2007 <- temporal(dat, 2007, IPCCfactors$TOTAL)
dat$OCSKGM2006 <- temporal(dat, 2006, IPCCfactors$TOTAL)
dat$OCSKGM2005 <- temporal(dat, 2005, IPCCfactors$TOTAL)


write.csv(dat, "data/REV_alldata.csv", row.names = F)

