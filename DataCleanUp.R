# Load Required Library

library(sf)
library(tidyverse)
#library(plotly)
#library(cartogram)
#library(geog4ga3)
#library(spdep)
#library(plotly)


# Merge .csv files

updatedData <- merge(x = Demographic, y = CRIME, by = "SCODE_NAME")
  
# Load shapefile

wards44 <- read_sf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")

# Merge wards shapefile to updatedData

Toronto_Data <- merge(x = wards44, y = updatedData, by = "SCODE_NAME")

# Plot Violent Crime

ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(NONVIOLENTC, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Non-Violent Crime")

ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(P_NONVIOLENTC, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Percentage Non-Violent Crime")