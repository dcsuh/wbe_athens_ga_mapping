library(mapview)
library(tidyverse)
library(dplyr)
library(sf)
library(plyr)
library(RColorBrewer)
library(heat.col)
library(here)

#load viral data and then clean. Here, we're summarizing the two targets into a single value, by averaging.
n1_n2 = read.csv("n1_n2_cleaned.csv")
n1_n2 = mutate(n1_n2, log_total_copies= log10(mean_total_copies))
n1_n2 = plyr::ddply(n1_n2, c("wrf", "date"), summarize, total_copies = mean(log_total_copies))
n1_n2 = n1_n2 %>% select(wrf, date, target, log_total_copies)

#now join the viral data to the catchment regions.
wrf_catchment = st_read("./wrf_catchment/wrf_catchment.shp")
n1_n2_catchment = left_join(wrf_catchment, n1_n2, by = "wrf")
n1_n2_catchment = as(n1_n2_catchment, "sf")

#Right now, we can only look at one date at a time, until we can animate/add slider/tabs.
#n1_n2_catchment = n1_n2_catchment %>% filter(date == "2020-10-20")


i <- 1
DATE <- dates[i]
plot <- n1_n2_catchment %>% filter(date == DATE) %>% mapview(., zcol = "total_copies", at = seq(10, 16, 1))
plot


#And map, using Leaflet's mapview.
mapview(n1_n2_catchment, zcol = "total_copies", at = seq(10, 16, 1))


###STOP HERE###

#Animation scripts are below from Hampton Roads. Still trouble with this.

# Animated SARS-CoV-2 total viral load for southeast VA during the first ~3 months of detection in the area

# this code creates and animates a map of total SARS-CoV-2 viral load (concentration in wastewater * flow at sample time) normalized
# by the population of WWTP's catchment.  
# the map has 3 components;
#                       1. a base map of the region - created using the maps package
#                       2. the catchment boundaries - imported shapefiles from our company's GIS dept.
#                       3. viral load data          - generated from our 9 WWTP facilities
#
# the animation is made using the gganimate package


# dependent packages

library(ggplot2)
library(ggthemes)
library(sf)
library(stringr)
library(maps)
library(mapdata)
library(wesanderson)
library(dplyr)
library(tidyr)
library(ggpubr)
library(RCurl)
library(pals)
#install.packages("gganimate")
library(gganimate)
#install.packages("tansformr")
#library(transformr) 

# get base map of region

counties <-  map_data('county')
ga_counties <-  subset(counties, region == 'georgia')


ga = st_as_sf(map('county', plot=F, fill=T )) 
ga = subset(ga, grepl('georgia', ga$ID))

# custom color scale
pal <- wes_palette("Zissou1", 100, type = "continuous")


# **** Map w/ Animation *****

ggplot()+ geom_sf(data = ga, color='grey30', fill='antiquewhite') + 
  geom_sf(data=n1_n2_catchment, aes(fill = log_total_copies), alpha=.80)


  scale_fill_gradientn(colors = pal)+
  labs(title = "SARS-CoV-2 Viral Load by TP Catchment Population", fill = expression(paste(Log[10], ' ', 'Viral Load')), 
       subtitle = 'Date: {frame_time}')+
  facet_wrap(~date)+
  theme_map()+
  theme(plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill="aliceblue", colour="black"),
        panel.border = element_rect(colour = 'black', fill=NA),
        legend.position = 'bottom',
        plot.title = element_text(size = 16))+
  guides(fill = guide_colorbar(barwidth = 12, barheight = 1))


# using animate function you can specify the transition speed in frames per second (fps)

SARS2_slower <- animate(SARS2, fps = 5)

SARS2_slower


# save to directory

anim_save('SARS2_slower.gif')

