# loading the required packages
library(ggplot2)
library(ggmap)
library(tidyverse)


Homicide <- Homicide_Data

Homicide%>%
  ggplot(aes(x = Longitude, y = Latitude, colour = Homicide_Type)) +
  geom_point()


