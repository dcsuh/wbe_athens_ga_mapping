library(raster)
library(mapview)
library(tidyverse)
library(dplyr)
library(sf)
library(rgdal)
library(rgeos)
library(cleangeo)
library(here)


###CEDAR CREEK###

#We are going to going to dissolve all of the geometries in the CC file into a 
#single geometry for future use. 
#We'll do this with the "sf" functions, since these are preferred by most when 
#working with GIS data in R.

#Read in the data. You should see ~5,000 parcels with geometries and owner info. 
#We really don't need that extra information, so we will remove in a bit
cedar_creek = st_read("/Users/danielsuh/Desktop/wbe_athens_ga_mapping/CCWRF/CCWRF.shp")

#Add an additional column, speficying the WRF, so that we can join by this common variable
cedar_creek$wrf = "CC"

#Take a look at the data
cedar_creek %>%
  st_set_geometry(NULL) %>%
  glimpse()

#All of the dimensions of the Cedar Creek data are = 2
#dimensions = as.data.frame(st_dimension(cedar_creek))

#Now, use the summarize functions to group by "wrf"
cedar_creek_dissolved =
  cedar_creek %>%
  group_by(wrf) %>% 
  summarise()

#view to make sure this worked. You should see only a single polygon when you hover. This should load faster now.
mapview(cedar_creek_dissolved)

#Check the sf, and you should see only two variables: wrf, geometry
#Let's write the sf to save for later

st_write(obj = cedar_creek_dissolved, dsn = 'cc_dissolved', driver = 'ESRI Shapefile', layer = NULL)

###MIDDLE OCONEE###

#Since we're having trouble with the Middle Oconee shapefiles &polygons, let's use a different method.
#This method is described [here](https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/).

#Download Middle Oconee Shapefile, using OGR
region = readOGR(dsn = "MOWRF", layer ="MOWRF")

#Because there are warnings, use cleangeo functions
region = clgeo_Clean(region)

# Check the shapefile has loaded correctly:
# plot(region)

# We're going to dissolve all regions in to one, for the catchment area for Middle Oconee.
# For this we'll create a lookup table and merge with the spatial data, based on OBJECTID

region@data = select(region@data, c("OBJECTID"))
lu = data.frame()
lu = rbind(lu, region@data)
lu$OBJECTID = as.character(lu$OBJECTID)
#lu$NAME = as.character(lu$NAME)
lu$wrf = NA
lu$wrf = "MI"

# Merge
# We now need to merge the lookup table into our spatial object data frame.
# We should end up with one row per zone to dissolve, each with a reference for the relevant larger geography.
# I think the trick is to make sure the row names match exactly, and if you can match the polygon IDs as well with spChFIDs().
region@data$OBJECTID = as.character(region@data$OBJECTID)
region@data = full_join(region@data, lu, by = "OBJECTID")

# Ensure shapefile row.names and polygon IDs are sensible
row.names(region) = row.names(region@data)
region = spChFIDs(region, row.names(region))

# Dissolve
# I use gUnaryUnion() (and indeed I think unionSpatialPolygons() in the maptools package uses this by default).
region = gUnaryUnion(region, id = region@data$wrf)

# If you want to just plot this using base plot you can stop there.
# If you want to do anything with the data or plot using ggplot you need to recreate the data frame.

# If you want to recreate an object with a data frame make sure row names match
row.names(region) = as.character(1:length(region))

# Extract the data you want (the larger geography)
lu = unique(lu$wrf)
lu = as.data.frame(lu)
colnames(lu) = "wrf"  # your data will probably have more than 1 row!

# And add the data back in
region = SpatialPolygonsDataFrame(region, lu)

# Check it's all worked
plot(region)

#Now, let's make this into an sf for future use.
middle_oconee_dissolved = as(region, "sf")

#Make sure it looks the same and is saved as one object
mapview(middle_oconee_dissolved)

#Let's be sure to write the sf object to retrieve later if we need
st_write(obj = middle_oconee_dissolved, dsn = 'mi_dissolved', driver = 'ESRI Shapefile', layer = NULL)


###NORTH OCONEE###

#Since we're also having trouble with the North Oconee shapefiles & polygons, let's use the method.
# described [here](https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/).

#Download Middle Oconee Shapefile, using OGR
region = readOGR(dsn = "NOWRF", layer ="NOWRF")

#This step will take a few seconds. You should have a large spatial polygon as the output.

#Because there are warnings, use cleangeo functions
region = clgeo_Clean(region)

#We still have some errors, but let's see if we can see the figure properly
plot(region)
#Again, this will take some time since this file is so large & has so many seperate geometries

# We're going to dissolve all regions in to one, for the catchment area for North Oconee.
# For this we'll create a lookup table and merge with the spatial data, based on OBJECTID

region@data = select(region@data, c("OBJECTID"))
lu = data.frame()
lu = rbind(lu, region@data)
lu$OBJECTID = as.character(lu$OBJECTID)
#lu$NAME = as.character(lu$NAME)
lu$wrf = NA
lu$wrf = "NO"

# Merge
# We now need to merge the lookup table into our spatial object data frame.
# We should end up with one row per zone to dissolve, each with a reference for the relevant larger geography.
# I think the trick is to make sure the row names match exactly, and if you can match the polygon IDs as well with spChFIDs().
region@data$OBJECTID = as.character(region@data$OBJECTID)
region@data = full_join(region@data, lu, by = "OBJECTID")

# Ensure shapefile row.names and polygon IDs are sensible
row.names(region) = row.names(region@data)
region = spChFIDs(region, row.names(region))

# Dissolve
# I use gUnaryUnion() (and indeed I think unionSpatialPolygons() in the maptools package uses this by default).
region = gUnaryUnion(region, id = region@data$wrf)

# If you want to just plot this using base plot you can stop there.
# If you want to do anything with the data or plot using ggplot you need to recreate the data frame.

# If you want to recreate an object with a data frame make sure row names match
row.names(region) = as.character(1:length(region))

# Extract the data you want (the larger geography)
lu = unique(lu$wrf)
lu = as.data.frame(lu)
colnames(lu) = "wrf"  # your data will probably have more than 1 row!

# And add the data back in
region = SpatialPolygonsDataFrame(region, lu)

# Check it's all worked
plot(region)

#Now, let's make this into an sf for future use.
north_oconee_dissolved = as(region, "sf")

#Make sure it looks the same and is saved as one object
mapview(middle_oconee_dissolved) + mapview(north_oconee_dissolved, col.region = "orange") + mapview(cedar_creek_dissolved, col.region = "green")

#Let's be sure to write the sf object to retrieve later if we need
st_write(obj = north_oconee_dissolved, dsn = 'no_dissolved', driver = 'ESRI Shapefile', layer = NULL)

wrf_catchment = rbind(cedar_creek_dissolved, middle_oconee_dissolved, north_oconee_dissolved)

st_write(obj = wrf_catchment, dsn = 'wrf_catchment', driver = 'ESRI Shapefile', layer = NULL)
