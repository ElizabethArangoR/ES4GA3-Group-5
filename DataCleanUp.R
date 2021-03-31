# Load Required Library

library('sf')
library(tidyverse)
library(plotly)
library(cartogram)
library(geog4ga3)
library(spdep)
library(plotly)

# Load .csv file 

updatedData <- ## Lucy's file + existing data 
updatedData$SCODE_NAME <- as.factor(Variables$SCODE_NAME)
  
# Load shapefile

ward44 <- read.sf("C:/Users/sooah/stats/WardTotonto.shp")

# Merge wards shapefile to updatedData

Toronto_Data <- merge(x = wards44, y = updatedData, by = "SCODE_NAME")