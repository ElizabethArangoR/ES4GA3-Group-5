# Load Required Library

library(sf)
library(tidyverse)
library(spdep)
#library(plotly)
#library(cartogram)
#library(geog4ga3)

#library(plotly)


# Merge .csv files

updatedData <- merge(x = Demographic, y = CRIME, by = "SCODE_NAME")
  
# Load shapefile

wards44 <- read_sf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")

# Merge wards shapefile to updatedData

Toronto_Data <- merge(x = wards44, y = updatedData, by = "SCODE_NAME")

summary(Toronto_Data)

# Plot Violent Crime

ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(NONVIOLENTC, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Non-Violent Crime")

# Convert dataframe file to SpatialPolygonsDataFrame

Toronto_Data.sp <- as(Toronto_Data, "Spatial")
Toronto_Data.w <- nb2listw(poly2nb(pl = Toronto_Data.sp))

# Scatterplot of Non-Violent Crime

mp <- moran.plot(Toronto_Data$NONVIOLENTC, Toronto_Data.w, xlab = "Non-Violent Crime", ylab = "Lagged Non-Violent Crime")

# Regression

model1 <- lm(formula = NONVIOLENTC ~ `Population (census 2016)`, data = Toronto_Data)
summary(model1) 

ggplot(data = Toronto_Data, aes(x = `Population (census 2016)`, y = NONVIOLENTC)) + 
  geom_point() +
  geom_abline(slope = model1$coefficients[2], # Recall that `geom_abline()` draws a line with intercept and slope as defined. Here the line is drawn using the coefficients of the regression model we estimated above. 
              intercept = model1$coefficients[1], 
              color = "blue", size = 1) +
  geom_vline(xintercept = 0) + # We also add the y axis... 
  geom_hline(yintercept = 0)




