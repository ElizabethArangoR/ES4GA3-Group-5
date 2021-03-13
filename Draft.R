
# Load library
library('sf')
library(tidyverse)
library(plotly)
library(cartogram)
library(gridExtra)
library(geog4ga3)
library(foreign)
library(spdep)
library(plotly)
require(rgdal)

CMBD<- read.dbf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/Toronto_Ward.dbf")


Homicide <- Homicide_Data
Variables <- Ward
Variables$SCODE_NAME <- as.factor(Variables$SCODE_NAME)

Homicide%>%
  ggplot(aes(x = Longitude, y = Latitude, colour = Homicide_Type)) +
  geom_point()


# Load shapefile

shapename <- read_sf('~/path/to/file.shp')

# Read shapefile

shape <- readOGR(dsn = ".", layer = "SHAPEFILE")

shape <- readOGR("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/data/SpatStat/Homicide Data.shp")
wards44 <- readOGR("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")

# Convert the shapefile to a datafram

wards44.sf <- st_as_sf(x=wards44, crs=4326, bbox=c("x","y"))
summary(wards44.sf)

# Merge wards shapefile to variables table

Toronto_W <- merge(x = wards44.sf, y = Variables, by = "SCODE_NAME")
summary(Toronto_W)
  
# Plot variable

p <- ggplot(data = Toronto_W, aes(x = x, y = y, color = Income)) + 
  geom_point(shape = 17, size = 5) +
  coord_fixed()
ggplotly(p)

homicides.sf <- st_as_sf(x=shape, crs=4326, coords=c("coords.x1","coords.x2"))






Wards_CT.sp <- as(wards44.sf, "Spatial")
Wards_CT.nb <- poly2nb(pl = Wards_CT.sp, queen = TRUE)
summary(Wards_CT.nb)