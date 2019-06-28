### Figura 1 ###
rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
library(sf)

library(ggplot2)
library(raster)
library(rgdal)
library(tidyverse)
library(cowplot)


# arg <- getData('GADM' , country="ARG", level=0)
arg <- read_sf("data/arg.gpkg")

cal <- read.csv("data/REV-cal.csv")
val <- read.csv("data/REV-val.csv")
cal$type <- "Calibration"
val$type <- "Validation"
d <- rbind(cal, val) 

coordinates(d) <- ~ X + Y
d@proj4string <- CRS("+init=epsg:4326")
d <- st_as_sf(d)
arg <- st_as_sf(arg)

maps <- ggplot() + geom_sf(data = arg) + geom_sf(data = d, size = 0.3, aes(color = type)) +
  facet_wrap(facets = "type") + theme_grey() + labs(x = "Long", y = "Lat") + theme(legend.position="none")

d <- as_tibble(d)
d$Date <- d$Date %>% as.character() %>% strptime("%Y-%m-%d") %>% as.POSIXct()
d$Year <- as.numeric(substring(d$Date,1,4))

hist <- ggplot(data = d, aes(Year, fill = type)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = c(1955, 1960,1970, 1980,1990,2000, 2005,2010,2015)) + theme_grey() + 
  + theme(legend.position="top")

fig1 <- plot_grid(maps, hist, labels = c("a", "b"), nrow = 2, align = "v")

ggsave(filename = "fig1.png", plot = fig1, width = 5, height = 7)
