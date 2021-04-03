# Load Required Library

install.packages('patchwork')
library(patchwork)
library(sf)
library(tidyverse)
library(spdep)
library(gridExtra)
library(ggsci)
#library(plotly)
library(cartogram)
library(stargazer)
# Merge .csv files

updatedData <- merge(x = Demographic, y = CRIME, by = "SCODE_NAME")

# Load shapefile

wards44 <- read_sf("C:/Users/sooah/Documents/ES4GA3-Group-5-main/WardToronto.shp")

# Merge wards shapefile to updatedData

Toronto_Data <- merge(x = wards44, y = updatedData, by = "SCODE_NAME")

Toronto_Data <- Toronto_Data%>%
  rename(POP = `Population (census 2016)`,
         DEN = `Density (people/ hectare)`,
         UNEMPLOY = `Unemployment rate (% labour force)`,
         SINPARHOU = `Single Parent households (%)`,
         HOUSINCOM = `Average Household Income ($)`,
         NOHIGHSCHO = `15+ without high school dip.`,
         MEDMALINCO = `Median Male income ($)`)

summary(Toronto_Data)

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
  geom_sf(aes(fill = cut_number(POP, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Population")+
  theme_void()

p2 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(DEN, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Density (people/hectare")+
  theme_void()

p3 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(UNEMPLOY, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Unemploymen rate (%)")+
  theme_void()

p4 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(SINPARHOU, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Single Parent Household (%)")+
  theme_void()

p5 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(HOUSINCOM, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Average Household Income ($)")+
  theme_void()

p6 <- ggplot(Toronto_Data) + 
  geom_sf(aes(fill = cut_number(MEDMALINCO, 6)), color = NA, size = 0.1) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  labs(fill = "Median Male income ($)")+
  theme_void()



grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)



p1 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = POP) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Population")+
  theme_void()

p2 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = DEN) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Density (people/hectare")+
  theme_void()

p3 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = UNEMPLOY) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Unemployment rate (%)")+
  theme_void()

p4 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = SINPARHOU) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Single Parent Household (%)")+
  theme_void()

p5 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = HOUSINCOM) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Average Household Income ($)")+
  theme_void()

p6 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = NOHIGHSCHO) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "15+ without high school diploma")+
  theme_void()


p7 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = MEDMALINCO) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Median Male income ($)")+
  theme_void()


grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 2)


# Plot Non-Violent Crime

NV <-ggplot() + 
  geom_sf(data = Toronto_Data) + 
  aes(fill = NONVIOLENTC) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Non-Violent Crime")+
  theme_void()

plot(NV)

# Cartogram Non-Violent Crime

crime_cartogram <- cartogram_cont(Toronto_Data, weight = "NONVIOLENTC")

ggplot(crime_cartogram, aes(fill = NONVIOLENTC)) +
  geom_sf(color = "white") + 
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Non-Violent Crime")+
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(caption="Source: ")



# Convert dataframe file to SpatialPolygonsDataFrame

Toronto_Data.sp <- as(Toronto_Data, "Spatial")

# Empirical Distribution of non-violent crime per ward and five simulated landscapes

Toronto_Data.w <- nb2listw(poly2nb(pl = Toronto_Data.sp))

# The function `lag.listw()` takes as argument the population density by census tracts in Hamilton, and calculates the moving average, with the "moving" part given by the local neighborhoods around each zone as defined by `Hamilton_CT.w`

Toronto_Data <- Toronto_Data %>%
  mutate(sma = lag.listw(Toronto_Data.w, NONVIOLENTC))


# Spatial Moving Average Simulation

# Null landscape/simulation #1

simulation_1 <- sample(Toronto_Data$NONVIOLENTC)
simulation_1.sma <- lag.listw(Toronto_Data.w, simulation_1)

# Null landscape/simulation #2

simulation_2 <- sample(Toronto_Data$NONVIOLENTC)
simulation_2.sma <- lag.listw(Toronto_Data.w, simulation_2)

# Null landscape/simulation #3

simulation_3 <- sample(Toronto_Data$NONVIOLENTC)
simulation_3.sma <- lag.listw(Toronto_Data.w, simulation_3)

# Null landscape/simulation #4

simulation_4 <- sample(Toronto_Data$NONVIOLENTC)
simulation_4.sma <- lag.listw(Toronto_Data.w, simulation_4)

# Null landscape/simulation #5

simulation_5 <- sample(Toronto_Data$NONVIOLENTC)
simulation_5.sma <- lag.listw(Toronto_Data.w, simulation_5)

# Adding the simulated landscapes to the `sf` dataframe.

Toronto_Data$simulation_1 <- simulation_1
Toronto_Data$simulation_2 <- simulation_2
Toronto_Data$simulation_3 <- simulation_3
Toronto_Data$simulation_4 <- simulation_4
Toronto_Data$simulation_5 <- simulation_5

# Adding the spatial moving averages of the simulated landscapes to the `sf` dataframe.

Toronto_Data$simulation_1.sma <- simulation_1.sma
Toronto_Data$simulation_2.sma <- simulation_2.sma
Toronto_Data$simulation_3.sma <- simulation_3.sma
Toronto_Data$simulation_4.sma <- simulation_4.sma
Toronto_Data$simulation_5.sma <- simulation_5.sma

Toronto_Datasm <- Toronto_Data%>% 
  transmute(observed = NONVIOLENTC, 
            simulation_1,
            simulation_2,
            simulation_3,
            simulation_4,
            simulation_5,
            geometry) %>% 
  pivot_longer(cols = -c(geometry, ends_with("sma")), names_to = "VAR", values_to = "values") %>%
  st_as_sf()

ggplot() + 
  geom_sf(data = Toronto_Datasm, 
          aes(fill = values), color = NA) + 
  facet_wrap(~VAR, ncol = 3) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Non-Violeny Crime per SMA") + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())+
  theme_void()


# Spatial 

Toronto_Datasm <- Toronto_Datasm %>% 
  data.frame(Toronto_Data %>% 
               st_drop_geometry() %>% # Drop the geometry because it is already available 
               # Select the original population density and the 5 null landscapes simulated from it.
               transmute(sma,
                         simulation_1.sma,
                         simulation_2.sma,
                         simulation_3.sma,
                         simulation_4.sma,
                         simulation_5.sma,
               ) %>% # Pass the result to `pivot_longer()`  
               pivot_longer(cols = everything(), names_to = "VAR", values_to = "SMA") %>% # Copy all density variables to a single column, and create a new variable called `VAR` with the names of the original columns 
               select(SMA))

# Scatter plot of empirical non-violent crime and its spatial moving average. Since Non-Violent Crime has a slope 
# that is much closer to the 45 degree line. This indicates that values of the variable i are not independent from their
# local means, hence, there is spatial autocorrelation.

ggplot(data = Toronto_Datasm, 
       aes(x = values, 
           y = SMA,
           color = VAR)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # Add a fitted line to the plots
  geom_smooth(formula = y ~ x,
              method = "lm") +
  coord_equal() +
  xlab("Non-Violent Crime") +
  ylab("SMA Non-Violent Crime") +
  facet_wrap(~ VAR, ncol = 3)

Toronto_Data <- Toronto_Data %>% 
  # Modify values in dataframe
  mutate(Rate_z = NONVIOLENTC - mean(NONVIOLENTC), # Substract the mean, so that the variable now is deviations from the mean 
         SMA_z = lag.listw(Toronto_Data.w, Rate_z)) 

mc <- moran.test(Toronto_Data$NONVIOLENTC, Toronto_Data.w)


# Moran's I and Moran's Scatterplot 

mp <- moran.plot(Toronto_Data$NONVIOLENTC, 
                 Toronto_Data.w, 
                 xlab = "Non-Violent Crime", 
                 ylab = "Lagged Non-Violent Crime")

Non_violent.lm <- localmoran(Toronto_Data$NONVIOLENTC, Toronto_Data.w)
summary(Non_violent.lm)


# Regression analysis of population and non-violent crime

model1 <- lm(formula = NONVIOLENTC ~ POP, data = Toronto_Data)
summary(model1) 

ggplot(data = Toronto_Data, aes(x = POP, y = NONVIOLENTC)) + 
  geom_point() +
  geom_abline(slope = model1$coefficients[2], # Recall that `geom_abline()` draws a line with intercept and slope as defined. Here the line is drawn using the coefficients of the regression model we estimated above. 
              intercept = model1$coefficients[1], 
              color = "blue", size = 1) +
  geom_vline(xintercept = 0) + # We also add the y axis... 
  geom_hline(yintercept = 0)


stargazer(model1,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on population")

r1 <- ggplot(data = Toronto_Data, 
             aes(x = POP, 
                 y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Population") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r1)

# Regression analysis of density and non-violent crime

model2 <- lm(formula = NONVIOLENTC ~ DEN, data = Toronto_Data)
summary(model2) 

ggplot(data = Toronto_Data, aes(x = DEN, y = NONVIOLENTC)) + 
  
  stargazer(model2,
            header = FALSE,
            title = "Non-Violent Crime per ward regressed on population density")

r2 <-ggplot(data = Toronto_Data, 
            aes(x = DEN, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Density (people/hectare)") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r2)

# Regression analysis of unemployment rate and non-violent crime

model3 <- lm(formula = NONVIOLENTC ~ UNEMPLOY, data = Toronto_Data)
summary(model3) 

stargazer(model3,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on unemplotment rate")

r3 <-ggplot(data = Toronto_Data, 
            aes(x = UNEMPLOY, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Unemployment Rate") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r3)

# Regression analysis of single parent household and non-violent crime

model4 <- lm(formula = NONVIOLENTC ~ SINPARHOU, data = Toronto_Data)
summary(model4) 

stargazer(model4,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on mean household income")

r4 <-ggplot(data = Toronto_Data, 
            aes(x = SINPARHOU, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Single Parent Household (%)") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r4)

# Regression analysis of household income and non-violent crime

model5 <- lm(formula = NONVIOLENTC ~ HOUSINCOM, data = Toronto_Data)
summary(model5) 

stargazer(model5,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on mean household income")

r5 <-ggplot(data = Toronto_Data, 
            aes(x = HOUSINCOM, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Average Household Income ($)") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r5)

# Regression analysis of 15+ without high school diploma and non-violent crime

model6 <- lm(formula = NONVIOLENTC ~ NOHIGHSCHO, data = Toronto_Data)
summary(model6) 

stargazer(model6,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on no high school dip")

r6 <-ggplot(data = Toronto_Data, 
            aes(x = NOHIGHSCHO, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("15+ without high school dip") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r6)

# Regression analysis of median male income and non-violent crime

model7 <- lm(formula = NONVIOLENTC ~ MEDMALINCO, data = Toronto_Data)
summary(model7) 

stargazer(model7,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on median male income ($)")

r7 <-ggplot(data = Toronto_Data, 
            aes(x = MEDMALINCO, 
                y = NONVIOLENTC))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Median Male Income ($)") +
  ylab("Non-Violent Crime")+
  theme_bw()

plot(r7)


grid.arrange(r1, r2, r3, r4, r5, r6, r7, nrow = 3, ncol = 2)

# Population ~ Non violent crimes analysis 
NV + p1 + r1 

# Pop density ~ Non violent crimes analysis 
NV + p2 + r2

# Unemployment ~ Non violent crimes analysis 
NV + p3 +r3

# Single parent household ~ Non violent crimes analysis
NV + p4 + r4

# Average household income ~ Non violent crimes analysis
NV + p5 +r5

# No high school diploma ~ Non violent crimes analysis
NV + p6 + r6

# Median Male Income ~ Non violent crimes analysis
NV + p7 +r7


