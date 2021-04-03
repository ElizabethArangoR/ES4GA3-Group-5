# Load library

library(sf)
library(tidyverse)
library(spdep)
library(gridExtra)
library(ggsci)

#library(plotly)
#library(cartogram)
#library(gridExtra)
#library(geog4ga3)
#library(foreign)
#library(plotly)
#require(rgdal)


## Merge .csv files

updatedData <- merge(x = Demographic, y = CRIME, by = "SCODE_NAME")

# Load shapefile

wards44 <- read_sf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")

# Merge wards shapefile to updatedData

Toronto_Data <- merge(x = wards44, y = updatedData, by = "SCODE_NAME")

# Study area

# 44-Ward Model (214-2018) (Retrieved from: https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/ward-profiles/44-ward-model/)

ggplot() +
  geom_sf(data = Toronto_Data,
          color = "white", 
          fill = "gray") + 
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))

# General pattern of demographic variables

p1 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(NONVIOLENTC, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Non-Violent Crime")+
  theme_bw()


grid.arrange(p1, p2, p3, p4, ncol = 2)




# Crime in Toronto (Non-Violent Crime)

ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(NONVIOLENTC, 5)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd") +
  coord_sf() +
  labs(fill = "Non-Violent Crime")



summary(Toronto_Data)


Toronto_Data.sp <- as(Toronto_Data, "Spatial")
Toronto_Data.w <- nb2listw(poly2nb(pl = Toronto_Data.sp))

mp <- moran.plot(Toronto_Data$NONVIOLENTC, Toronto_Data.w, xlab = "Non-Violent Crime", ylab = "Lagged Non-Violent Crime")











# Plot Population Density

pop_den.map <- ggplot(Toronto_W) + 
  geom_sf(aes(fill = cut_number(`Density (people/ hectare)`, 5)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdPu") +
  labs(fill = "Population")
pop_den.map


# Plot cartogram

CT_UR_cartogram <- cartogram_cont(Toronto_W, 
                                   weight = "Unemployment rate (% labour force)",
                                   itermax=5)

CT_pop_cartogram <- cartogram_cont(Toronto_W, 
                                   weight = "Population (census 2016)",
                                   itermax=5)

une_rate.cartogram <- ggplot(CT_UR_cartogram) + 
  geom_sf(aes(fill = cut_number(`Unemployment rate (% labour force)`, 5)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdPu") +
  labs(fill = "Unemployment rate (%)")

pop_den.cartogram <- ggplot(CT_pop_cartogram) + 
  geom_sf(aes(fill = cut_number(`Population (census 2016)`, 5)), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdPu") +
  labs(fill = "Population")


grid.arrange(pop_den.map, pop_den.cartogram, nrow = 2)


# Convert Toronto_W to a spatial polygons dataframe

Toronto_W.sp <- as(Toronto_W, "Spatial")
Toronto_W.nb <- poly2nb(pl = Toronto_W.sp, queen = TRUE)
summary(Toronto_W.nb)

class(Toronto_W.nb)

Toronto_W.w <- nb2listw(Toronto_W.nb)

plot(Toronto_W.sp, border = "gray")
plot(Toronto_W.nb, coordinates(Toronto_W.sp), col = "red", add = TRUE)

# Spatial Moving Averages


Hamilton_CT$Null_1 <- sample(Hamilton_CT$Prop20to34)

Population.sma <- lag.listw(Toronto_W.w, Toronto_W$`Population (census 2016)`)

## Joining this new variable to both the sf and SpatialPolygonDataFrame objects:

Toronto_W$Population.sma <- Population.sma
Toronto_W.sp$Population.sma <- Population.sma

## Spatial pattern of an area variable as a smoother 

Population_s1 <- sample(Toronto_W$`Population (census 2016)`)

### Spatial moving average for this randomized variable

Population_s1.sma <- lag.listw(Toronto_W.w, Population_s1)

### Simulating new variables/null landscapes and calculating their spatial moving averages

Population_s2 <- sample(Toronto_W$`Population (census 2016)`)
Population_s2.sma <- lag.listw(Toronto_W.w, Population_s2)

Population_s3 <- sample(Toronto_W$`Population (census 2016)`)
Population_s3.sma <- lag.listw(Toronto_W.w, Population_s3)

Population_s4 <- sample(Toronto_W$`Population (census 2016)`)
Population_s4.sma <- lag.listw(Toronto_W.w, Population_s4)

Population_s5 <- sample(Toronto_W$`Population (census 2016)`)
Population_s5.sma <- lag.listw(Toronto_W.w, Population_s5)

Population_s6 <- sample(Toronto_W$`Population (census 2016)`)
Population_s6.sma <- lag.listw(Toronto_W.w, Population_s6)

Population_s7 <- sample(Toronto_W$`Population (census 2016)`)
Population_s7.sma <- lag.listw(Toronto_W.w, Population_s7)

Population_s8 <- sample(Toronto_W$`Population (census 2016)`)
Population_s8.sma <- lag.listw(Toronto_W.w, Population_s8)

### Adding the simulated null landscape to the dataframes and their spatial moving averages.

Toronto_W$Population_s1 <- Population_s1
Toronto_W$Population_s2 <- Population_s2
Toronto_W$Population_s3 <- Population_s3
Toronto_W$Population_s4 <- Population_s4
Toronto_W$Population_s5 <- Population_s5
Toronto_W$Population_s6 <- Population_s6
Toronto_W$Population_s7 <- Population_s7
Toronto_W$Population_s8 <- Population_s8

### Adding the simulated null landscape to the dataframes and their spatial moving averages.

Toronto_W$Population_s1.sma <- Population_s1.sma
Toronto_W$Population_s2.sma <- Population_s2.sma
Toronto_W$Population_s3.sma <- Population_s3.sma
Toronto_W$Population_s4.sma <- Population_s4.sma
Toronto_W$Population_s5.sma <- Population_s5.sma
Toronto_W$Population_s6.sma <- Population_s6.sma
Toronto_W$Population_s7.sma <- Population_s7.sma
Toronto_W$Population_s8.sma <- Population_s8.sma

### Selecting the empirical moving average and the 8 simulated instances of population. 

Toronto_W2 <- Toronto_W %>% # This pipe operator passes the dataframe to `select()`
  # `select()` keeps only the spatial moving averages and geometry
  select(Population.sma,
         Population_s1.sma,
         Population_s2.sma,
         Population_s3.sma,
         Population_s4.sma,
         Population_s5.sma,
         Population_s6.sma,
         Population_s7.sma,
         Population_s7.sma,
         Population_s8.sma,
         geometry) %>% # This pipe operator passes the dataframe with only the spatial moving average variables and the geometry to `gather()`
  # `gather()` places all variables with the exception of `geometry` in a single column named `DENSITY_SMA` and creates a new variable called `VAR` with the names of the original columns (i.e., POP_DENSITY.sma, POP_DENSITY_s1.sma, etc.)
  gather(VAR, DENSITY_SMA, -geometry)


# Cloropleth maps

ggplot() + 
  geom_sf(data = Toronto_W2, 
          aes(fill = DENSITY_SMA), color = NA) + 
  facet_wrap(~VAR, ncol = 3) + # We are creating multiple plots for single data frame by means of the "facet_wrap" function.
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + # Select palette for colors 
  labs(fill = "Population") + # Change the label of the legend
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) 


