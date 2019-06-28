### Figura 1 ###
rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
library(sf)

library(ggplot2)
library(raster)
library(rgdal)
library(tidyverse)
library(cowplot)
library(ggsn)


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
  theme(legend.position="none") + coord_sf(datum=st_crs(4326)) +
  theme(text=element_text(size=12,  family="serif"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  theme(panel.spacing = unit(2.5, "lines")) + 
  ggsn::scalebar(data = d, st.color = "#666666", dist = 300, 
                 st.size=3, height=0.02, transform = T,
                 model = "WGS84", dist_unit = "km", facet.var = "type",
                 border.size = 0.1, anchor = c(x=-56, y=-57)) + 
  ggsn::north(data = d, scale = 0.15, symbol = 12, 
              location = "topleft")
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
soc <- raster("results/REV-prediction.tif")
arg <- read_sf("data/arg.gpkg")
arg <- st_as_sf(arg)

xmin <- sf::st_bbox(arg)["xmin"]
xmax <- sf::st_bbox(arg)["xmax"]
ymin <- sf::st_bbox(arg)["ymin"]
ymax <- sf::st_bbox(arg)["ymax"]


# plot 
map <- gplot(soc, maxpixels = 1e+6) + geom_tile(aes(fill = value)) + theme_gray() +
    scale_fill_gradient2(low = "black",midpoint = 35, mid = "yellow",
                         high = "red", space = "Lab",
                        na.value = "transparent",
                      breaks = c(0,5,15,30,50,70),
                      name = expression(kg/m^2)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = 4326) + #coord_equal() +
  theme(text=element_text(size=12,  family="serif")) +
  ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
                 st.color = "#666666", dist = 300, 
                 st.size=3, height=0.02, transform = T,
                 model = "WGS84", dist_unit = "km", 
                 border.size = 0.1, anchor = c(x=-56, y=-57)) + 
  ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
              scale = 0.15, symbol = 12, 
              location = "topleft")
  

# save
ggsave(filename = "results/fig3.png", plot = map, width = 4, height = 7)

####### Fig 4 ###################################

soc.u <- raster("results/qrf_residuals.tif")
summary(soc.u)

map.u <- 
  gplot(soc.u, maxpixels = 1e+6) + geom_tile(aes(fill = value)) + theme_gray() +
  scale_fill_gradient2(low = "black",midpoint = 12, mid = "yellow",
                       high = "red", space = "Lab",
                       na.value = "transparent",
                       breaks = c(0,3,6,9,12),
                       name = expression(kg/m^2)) +
  xlab("Longitude") + ylab("") +
  coord_sf(crs = 4326) + #coord_equal() +
  theme(text=element_text(size=12,  family="serif")) +
  ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
                 st.color = "#666666", dist = 300, 
                 st.size=3, height=0.02, transform = T,
                 model = "WGS84", dist_unit = "km", 
                 border.size = 0.1, anchor = c(x=-56, y=-57)) + 
  ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
              scale = 0.15, symbol = 12, 
              location = "topleft")
Fig3_4 <- plot_grid(map, map.u, labels = c("a", "b"), ncol = 2, align = "h",
          label_fontfamily = "serif")

ggsave(filename = "results/fig3_4.png", plot = Fig3_4, width = 8, height = 5.76)


#### plot del error relativo (no pedido) #############################
# rel.e <- soc.u/soc
# summary(rel.e) 
# gplot(rel.e*100, maxpixels = 1e+5) + geom_tile(aes(fill = value)) + theme_gray() +
#   scale_fill_gradient2(low = "black", mid = "yellow",midpoint = 200,
#                        high = "red", space = "Lab",
#                        na.value = "transparent",
#                        breaks = c(50,100,200,500),
#                        name = "relative error (%)"
#                        ) +
#   xlab("Longitude") + ylab("") +
#   coord_sf(crs = 4326) + #coord_equal() +
#   theme(text=element_text(size=12,  family="serif")) +
#   ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
#                  st.color = "#666666", dist = 300, 
#                  st.size=3, height=0.02, transform = T,
#                  model = "WGS84", dist_unit = "km", 
#                  border.size = 0.1, anchor = c(x=-56, y=-57)) + 
#   ggsn::north(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax,
#               scale = 0.15, symbol = 12, 
#               location = "topleft")

#####  Fig 5 #################
rm(list = ls())
name <- function(x) { as.data.frame(names(x))}

v <- read_csv("results/REV-validation.csv")
z <- v %>% select(ID,observed, predH, predNH) %>% 
  mutate(id = row_number())
names(z)[3:4] <- c("Harmonized", "Not harmonized")
w <- z %>% group_by(id,observed) %>% 
  gather(Harmonized, `Not harmonized`,
         key = "variable", value = "pred")

fig5 <-  ggplot(w, aes(x = observed, y = pred)) + geom_abline(intercept = 0, color = "white") +
  geom_point(size = 0.1) + coord_fixed(ratio = 1) +#, xlim = c(0,20), ylim = c(0,20)) + 
  facet_wrap(facets = "variable") + 
  labs(x = "Observed", y = "Predicted") +
  theme(text=element_text(size=16,  family="serif"))

ggsave(filename = "results/fig5.png", plot = fig5, width = 8, height = 4)

# alternatively
fig5b <- ggplot(w, aes(x = observed, y = pred)) + geom_abline(intercept = 0, color = "white") +
    geom_bin2d(binwidth = c(0.25, 0.25),alpha = 0.6, aes(fill = variable)) +
    #geom_point(size = 1, alpha = 0.1, aes(color = variable)) + 
    coord_fixed(ratio = 1, xlim = c(0,20), ylim = c(0,20)) + 
  #facet_wrap(facets = "variable") + 
  labs(x = "Observed", y = "Predicted") + #geom_density_2d(aes(color = variable)) +
  theme(text=element_text(size=16,  family="serif"))

ggsave(filename = "results/fig5b.png", plot = fig5b, width = 8, height = 4)
