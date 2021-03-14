
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


# Load .csv file
Variables <- Ward
Variables$SCODE_NAME <- as.factor(Variables$SCODE_NAME)

# Load shapefile

wards44 <- read_sf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")

# Merge wards shapefile to variables table

Toronto_W <- merge(x = wards44, y = Variables, by = "SCODE_NAME")
summary(Toronto_W)

Toronto_W%>%
  mutate()

# Plot Population

ggplot(Toronto_W) + 
  geom_sf(aes(fill = cut_number(`Population (census 2016)`, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Population")

# Plot Population Density

pop_den.map <- ggplot(Toronto_W) + 
  geom_sf(aes(fill = cut_number(`Density (people/ hectare)`, 5)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(fill = "Pop Density")
pop_den.map


# Plot cartogram

CT_pop_cartogram <- cartogram_cont(Toronto_W, 
                                   weight = "`Median Male income ($)`",
                                   itermax=1)

ggplot(CT_pop_cartogram) + 
  geom_sf(aes(fill = cut_number(`Population (census 2016)`, 5)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(fill = "Population")


# Convert Toronto_W to a spatial polygons dataframe

Toronto_W.sp <- as(Toronto_W, "Spatial")
Toronto_W.nb <- poly2nb(pl = Toronto_W.sp, queen = TRUE)
summary(Toronto_W.nb)

class(Toronto_W.nb)

Toronto_W.w <- nb2listw(Toronto_W.nb)

plot(Toronto_W.sp, border = "gray")
plot(Toronto_W.nb, coordinates(Toronto_W.sp), col = "red", add = TRUE)

# Spatial Moving Averages





#homicides.sf <- st_as_sf(x=shape, crs=4326, coords=c("coords.x1","coords.x2"))
#CMBD<- read.dbf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/Toronto_Ward.dbf")
#Homicide <- Homicide_Data
#Homicide%>%
  #ggplot(aes(x = Longitude, y = Latitude, colour = Homicide_Type)) +
  #geom_point()

# Convert the shapefile to a datafram

#wards44.sf <- st_as_sf(x=wards44, crs=4326, bbox=c("x","y"))
#summary(wards44.sf)

# Read shapefile

#wards44 <- readOGR("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")