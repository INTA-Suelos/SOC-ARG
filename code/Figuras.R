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
# shape of argentina border
arg <- read_sf("data/arg.gpkg")
# calibration and validation dataset
cal <- read.csv("data/REV-cal.csv")
val <- read.csv("data/REV-val.csv")
cal$type <- "Calibration"
val$type <- "Validation"
d <- rbind(cal, val) 

coordinates(d) <- ~ X + Y
d@proj4string <- CRS("+init=epsg:4326")
d <- st_as_sf(d)
arg <- st_as_sf(arg)
# plot
maps <- ggplot() + geom_sf(data = arg) + geom_sf(data = d, size = 0.3, aes(color = type)) +
  facet_wrap(facets = "type") + theme_grey() + labs(x = "Longitude", y = "Latitude") + 
  theme(legend.position="none") +
  theme(text=element_text(size=12,  family="serif"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) + theme(panel.spacing = unit(2.5, "lines"))
# convert to tibble
d <- as_tibble(d)
d$Date <- d$Date %>% as.character() %>% strptime("%Y-%m-%d") %>% as.POSIXct()
d$Year <- as.numeric(substring(d$Date,1,4))

# how many samples in each category
d %>% group_by(type) %>% summarise(N = n())

# plot
hist <- ggplot(data = d, aes(Year, fill = type)) + geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = c(1955, 1960,1970, 1980,1990,2000, 2005,2010,2015)) + theme_grey() + 
 theme(legend.position="top") +  scale_fill_discrete("") + labs(x = "Years", y = "Count") +
  theme(text=element_text(size=12,  family="serif")) 

fig1 <- plot_grid(maps, hist, labels = c("a", "b"), nrow = 2, align = "v",
                  label_fontfamily = "serif", rel_heights = c(2, 1))

ggsave(filename = "results/fig1.png", plot = fig1, width = 5, height = 7)

###### Fig 3 ##########################
rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
library(rasterVis)
# load predicted map
soc <- raster("results/OCSKGM2015.tif")
# plot 
map <- gplot(soc, maxpixels = 1e+6) + geom_tile(aes(fill = value)) + theme_gray() +
  scale_fill_gradient(na.value = "transparent", high = "yellow",
                      name = expression(kg/m^2)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = 4326) + 
  theme(text=element_text(size=12,  family="serif"))
# save
ggsave(filename = "results/fig3.png", plot = map, width = 4, height = 7)
###############################################################
                        