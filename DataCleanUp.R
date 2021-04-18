# Load Required Library

library(patchwork)
library(sf)
library(tidyverse)
library(spdep)
library(gridExtra)
library(ggsci)
#library(plotly)
library(cartogram)
library(stargazer)
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(geog4ga3)
library(MASS)
# Merge .csv files

updatedData <- merge(x = Demographic, y = CRIME, by = "SCODE_NAME")

# Load shapefile

wards44 <- read_sf("C:/Users/sooah/Documents/ES4GA3-Group-5-main/WardToronto.shp")
wards44 <- read_sf("C:/Users/Elizabeth/Documents/GitHub/ES4GA3-Group-5/WardToronto.shp")
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

Toronto_Data <- Toronto_Data%>%
  mutate(NVRate = (NONVIOLENTC/POP)*1000)

# Study area

# 44-Ward Model (214-2018) (Retrieved from: https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/ward-profiles/44-ward-model/)

p1 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = POP) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Population")+
  theme_void()

p2 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = DEN) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Density (people/hectare)")+
  theme_void()

p3 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = UNEMPLOY) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Unemploymente rate (%)")+
  theme_void()

p4 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = SINPARHOU) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Single Parent Household (%)")+
  theme_void()

p5 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = HOUSINCOM) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Average Household Income ($)")+
  theme_void()

p6 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = NOHIGHSCHO) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "15+ without high school diploma")+
  theme_void()

p7 <- ggplot(Toronto_Data) + 
  geom_sf() +
  aes(fill = MEDMALINCO) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(title = "Median Male income ($)")+
  theme_void()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 2)

plot_grid(p1, p2, p3, p4, p5, p6, p7, ncol = 2, align = 'v')

# Plot Non-Violent Crime

NV <-ggplot() + 
  geom_sf(data = Toronto_Data) + 
  aes(fill = NVRate) +
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Non-Violent Crime")+
  theme_void()

plot(NV)

# Cartogram Non-Violent Crime

crime_cartogram <- cartogram_cont(Toronto_Data, weight = "NONVIOLENTC")

ggplot(crime_cartogram, aes(fill = NVRate)) +
  geom_sf(color = "white") + 
  scale_fill_gradientn(colors = viridis::viridis(20))+
  labs(fill = "Non-Violent Crime")+
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(caption="Source: ")



# Convert dataframe file to SpatialPolygonsDataFrame + Creating spatial weights matrix

Toronto_Data.sp <- as(Toronto_Data, "Spatial")
Toronto_Data.nb <- poly2nb(pl = Toronto_Data.sp, queen = TRUE)

# Empirical Distribution of non-violent crime per ward and five simulated landscapes

Toronto_Data.w <- nb2listw(Toronto_Data.nb)

# The function `lag.listw()` takes as argument the population density by census tracts in Toronto, and calculates the moving average, with the "moving" part given by the local neighborhoods around each zone as defined by `Toronto_Data.w`

Toronto_Data <- Toronto_Data %>%
  mutate(sma = lag.listw(Toronto_Data.w, NVRate))

# Local Moran Maps

localmoran.map(Toronto_Data, Toronto_Data.w, "NVRate", by = "SCODE_NAME")

# Spatial Moving Average Simulation

# Null landscape/simulation #1

simulation_1 <- sample(Toronto_Data$NVRate)
simulation_1.sma <- lag.listw(Toronto_Data.w, simulation_1)

# Null landscape/simulation #2

simulation_2 <- sample(Toronto_Data$NVRate)
simulation_2.sma <- lag.listw(Toronto_Data.w, simulation_2)

# Null landscape/simulation #3

simulation_3 <- sample(Toronto_Data$NVRate)
simulation_3.sma <- lag.listw(Toronto_Data.w, simulation_3)

# Null landscape/simulation #4

simulation_4 <- sample(Toronto_Data$NVRate)
simulation_4.sma <- lag.listw(Toronto_Data.w, simulation_4)

# Null landscape/simulation #5

simulation_5 <- sample(Toronto_Data$NVRate)
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
  transmute(observed = NVRate, 
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
  mutate(Rate_z = NVRate - mean(NVRate), # Substract the mean, so that the variable now is deviations from the mean 
         SMA_z = lag.listw(Toronto_Data.w, Rate_z)) 

mc <- moran.test(Toronto_Data$NVRate, Toronto_Data.w)


# Moran's I and Moran's Scatterplot 

mp <- moran.plot(Toronto_Data$NVRate, 
                 Toronto_Data.w, 
                 xlab = "Non-Violent Crime Rate", 
                 ylab = "Lagged Non-Violent Crime Rate")

mp8 <- moran.plot(Toronto_Data$NONVIOLENTC, 
                 Toronto_Data.w, 
                 xlab = "Non-Violent Crime", 
                 ylab = "Lagged Non-Violent Crime")

mp1 <- moran.plot(Toronto_Data$POP, 
                  Toronto_Data.w,
                  xlab = "Population", 
                  ylab = "Lagged Population")

mp2 <- moran.plot(Toronto_Data$DEN, 
                  Toronto_Data.w,
                  xlab = "Density (people/hectare)", 
                  ylab = "Lagged Density (people/hectare)")

mp3 <- moran.plot(Toronto_Data$UNEMPLOY, 
                  Toronto_Data.w,
                  xlab = "Unemployment Rate", 
                  ylab = "Lagged Unemployment Rate")

mp4 <- moran.plot(Toronto_Data$SINPARHOU, 
                  Toronto_Data.w,
                  xlab = "Single Parent Household (%)", 
                  ylab = "Lagged Unemployment Rate")

mp5 <- moran.plot(Toronto_Data$HOUSINCOM, 
                  Toronto_Data.w,
                  xlab = "Average Household Income ($)", 
                  ylab = "Lagged Average Household Income ($)")

mp6 <- moran.plot(Toronto_Data$NOHIGHSCHO, 
                  Toronto_Data.w,
                  xlab = "15+ Without High School Diploma", 
                  ylab = "Lagged 15+ Without High School Diploma")

mp7 <- moran.plot(Toronto_Data$MEDMALINCO, 
                  Toronto_Data.w,
                  xlab = "Median Male Income ($)", 
                  ylab = "Lagged Median Male Income ($)")

Non_violent.lm <- localmoran(Toronto_Data$NVRate, Toronto_Data.w)
summary(Non_violent.lm)

# Regression analysis of population and non-violent crime

formula <- y ~ x

model1 <- lm(formula = NVRate ~ POP, data = Toronto_Data)
summary(model1) 

ggplot(data = Toronto_Data, aes(x = POP, y = NVRate)) + 
  geom_point() +
  geom_abline(slope = model1$coefficients[2], # Recall that `geom_abline()` draws a line with intercept and slope as defined. Here the line is drawn using the coefficients of the regression model we estimated above. 
              intercept = model1$coefficients[1], 
              color = "blue", size = 1) +
  geom_vline(xintercept = 0) + # We also add the y axis... 
  geom_hline(yintercept = 0)


stargazer(model1,
          header = FALSE,
          title = "Non-Violent Crime Rate per ward regressed on population")

r1 <- ggplot(data = Toronto_Data, 
             aes(x = POP, 
                 y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("Population") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r1)

# Regression analysis of density and non-violent crime

model2 <- lm(formula = NVRate ~ DEN, data = Toronto_Data)
summary(model2) 

ggplot(data = Toronto_Data, aes(x = DEN, y = NVRate)) + 
  
  stargazer(model2,
            header = FALSE,
            title = "Non-Violent Crime per ward regressed on population density")

r2 <-ggplot(data = Toronto_Data, 
            aes(x = DEN, 
                y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("Density (people/hectare)") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r2)

# Regression analysis of unemployment rate and non-violent crime

model3 <- lm(formula = NVRate ~ UNEMPLOY, data = Toronto_Data)
summary(model3) 

stargazer(model3,
          header = FALSE,
          title = "Non-Violent Crime Rate per ward regressed on unemplotment rate")

r3 <-ggplot(data = Toronto_Data, 
            aes(x = UNEMPLOY, 
                y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("Unemployment Rate") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r3)

# Regression analysis of single parent household and non-violent crime

model4 <- lm(formula = NVRate ~ SINPARHOU, data = Toronto_Data)
summary(model4) 

stargazer(model4,
          header = FALSE,
          title = "Non-Violent Crime rate per ward regressed on mean household income")

r4 <-ggplot(data = Toronto_Data, 
            aes(x = SINPARHOU, 
                y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("Single Parent Household (%)") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r4)

# Regression analysis of household income and non-violent crime

model5 <- lm(formula = NVRate ~ HOUSINCOM, data = Toronto_Data)
summary(model5) 

stargazer(model5,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on mean household income")

r5 <-ggplot(data = Toronto_Data, 
            aes(x = HOUSINCOM, 
                y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("Average Household Income ($)") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r5)

# Regression analysis of 15+ without high school diploma and non-violent crime

model6 <- lm(formula = NVRate ~ NOHIGHSCHO, data = Toronto_Data)
summary(model6) 

stargazer(model6,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on no high school dip")


r6 <-ggplot(data = Toronto_Data, 
            aes(x = NOHIGHSCHO, 
                y = NVRate))+
  geom_point() +
  stat_smooth(formula = formula,
              method = "lm") +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +
  xlab("15+ without high school dip") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r6)

# Regression analysis of median male income and non-violent crime

model7 <- lm(formula = NVRate ~ MEDMALINCO, data = Toronto_Data)
summary(model7) 

stargazer(model7,
          header = FALSE,
          title = "Non-Violent Crime per ward regressed on median male income ($)")

r7 <-ggplot(data = Toronto_Data, 
            aes(x = MEDMALINCO, 
                y = NVRate))+
  geom_point() +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  xlab("Median Male Income ($)") +
  ylab("Non-Violent Crime Rate")+
  theme_bw()

plot(r7)

grid.arrange(r1, r2, r3, r4, r5, r6, r7, nrow = 4, ncol = 2)

# Population ~ Non violent crimes analysis 
NV + p1 + r1 + mp1

# Pop density ~ Non violent crimes analysis 
NV + p2 + r2 + mp2

# Unemployment ~ Non violent crimes analysis 
NV + p3 +r3 + mp3

# Single parent household ~ Non violent crimes analysis
NV + p4 + r4 + mp4

# Average household income ~ Non violent crimes analysis
NV + p5 +r5 +mp5

# No high school diploma ~ Non violent crimes analysis
NV + p6 + r6 + mp6

# Median Male Income ~ Non violent crimes analysis
NV + p7 +r7 +mp7

# CORRELATION ANALYSIS

# Correlation analysis using scatter plots + cor.method = spearman

NV_POP_spear <- ggscatter(Toronto_Data, x = "POP", y = "NVRate",
                        add = "reg.line", conf.int=TRUE,
                        cor.coef=TRUE, cor.method= "spearman",
                        xlab = "Population", ylab = "Non violent crimes")

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$POP, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

<<<<<<< HEAD
plot(NV_POP_spear) #We can see that R = 0.3m and p = 0.05
=======
plot(NV_POP_spear) #We can see that R = 0.3 and p = 0.05
>>>>>>> 867d5b681d9c0789750b5590d4721fa6306b0cd4

# Correlation analysis using density and non-violent crime rate

NV_POPDEN_spear <- ggscatter(Toronto_Data, x = "DEN", y = "NVRate",
                          add = "reg.line", conf.int=TRUE,
                          cor.coef=TRUE, cor.method= "spearman",
                          xlab = "Population Density", ylab = "Non violent crimes")

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$DEN, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

plot(NV_POPDEN_spear) #We can see that R = 0.17. and p=0.27

# Correlation analysis using unemployment and non-violent crime rate

NV_UNEMP_pear <- ggscatter(Toronto_Data, x = "UNEMPLOY", y = "NVRate",
                             add = "reg.line", conf.int=TRUE,
                             cor.coef=TRUE, cor.method= "spearman",
                             xlab = "Unemployment Rate (%)", ylab = "Non violent crimes")
<<<<<<< HEAD
plot(NV_UNEMP_spear) #We can see that R = -0.018, and p = 0.91

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$UNEMPLOY, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

=======
plot(NV_UNEMP_spear)

# Correlation analysis using single parent and non-violent crime rate

NV_SINPARHOU_spear <- ggscatter(Toronto_Data, x = "SINPARHOU", y = "NVRate",
                            add = "reg.line", conf.int=TRUE,
                            cor.coef=TRUE, cor.method= "spearman",
                            xlab = "Single Parent Household (%)", ylab = "Non violent crimes")
plot(NV_SINPARHOU_spear)

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$SINPARHOU, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

# Correlation analysis using non high school and non-violent crime rate

NV_EDU_spear <- ggscatter(Toronto_Data, x = "NOHIGHSCHO", y = "NVRate",
                                add = "reg.line", conf.int=TRUE,
                                cor.coef=TRUE, cor.method= "spearman",
                                xlab = "15+ No highschool diploma", ylab = "Non violent crimes")
plot(NV_EDU_spear)

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$NOHIGHSCHO, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

# Correlation analysis using house income and non-violent crime rate

NV_AVEINC_spear <- ggscatter(Toronto_Data, x = "HOUSINCOM", y = "NVRate",
                                add = "reg.line", conf.int=TRUE,
                                cor.coef=TRUE, cor.method= "spearman",
                                xlab = "Average household income", ylab = "Non violent crimes")
plot(NV_HouseIncome_spear)

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$HOUSINCOM, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)

# Correlation analysis using medium male income and non-violent crime rate

NV_MED_spear <- ggscatter(Toronto_Data, x = "MEDMALINCO", y = "NVRate",
                                add = "reg.line", conf.int=TRUE,
                                cor.coef=TRUE, cor.method= "spearman",
                                xlab = "Medium Male Income ($)", ylab = "Non violent crimes")
plot(NV_MED_spear)

cor.test(Toronto_Data$NVRate, 
         Toronto_Data$MEDMALINCO, 
         method = "pearson", 
         conf.level = 0.95, 
         data=Toronto_Data)


grid.arrange(r1, r2, r3, r4, r5, r6, r7, nrow = 4, ncol = 2)

# MULTIPLE LINEAR REGRESSION (y = b0 + b1*x1 + b2*x2 + b3*x3)
#The “b” values are called the regression weights (or beta coefficients). 
#They measure the association between the predictor variable and the outcome.
#“b_j” can be interpreted as the average effect on y of a one unit increase in “x_j”, holding all other predictors fixed.

Multi1 <- lm(NVRate ~ DEN + UNEMPLOY + SINPARHOU + NOHIGHSCHO + HOUSINCOM + MEDMALINCO, Toronto_Data)

anova(Multi1)

#An R2 value close to 1 indicates that the model explains a large portion of the variance in the outcome variable.
summary(Multi1) # Here the adjusted R2=0.251, meaning that "25% of the variance in the measure of sales can be predicted by those variables
confint(Multi1) # Confidence intervals by interpolation in the profile traces.

# Residual Standard Error (RSE), or sigma: The RSE estimate gives a measure of error of prediction. The lower the RSE, the more accurate the model (on the data in hand).

sigma(Multi1)/mean(Toronto_Data$NVRate) # In our multiple regression model, the RSE is 1.846 corresponding to 48% error rate.

# FINDING THE BEST-FIT MODEL
# AIC: The Akaike information criterion (AIC) is an estimator of prediction error and thereby relative quality of statistical models for a given set of data.

Bestmodel <- stepAIC(Multi1, direction="both")
Bestmodel$anova # Shows final model (NVRate ~ DEN + SINPARHOU + NOHIGHSCHO) - the model that minimizes the information loss. AIC=55.01

# Conclusion: Non-violent crime rate is better explained by a multivariate model with density of population, single-parent household and Non-High School diploma as predictors.

# CHEKING HOMOSCEDASTICITY

# Plot final model information
par(mfrow=c(2,2))
plot(Bestmodel)
par(mfrow=c(1,1))


#The diagnostic plots show the unexplained variance (residuals) across the range of the observed data.

#Each plot gives a specific piece of information about the model fit, but it’s enough to know that the red line representing the mean of the residuals should be horizontal and centered on zero (or on one, in the scale-location plot), meaning that there are no large outliers that would cause bias in the model.

#The normal Q-Q plot plots a regression between the theoretical residuals of a perfectly-heteroscedastic model and the actual residuals of your model, so the closer to a slope of 1 this is the better. This Q-Q plot is very close, with only a bit of deviation.

#From these diagnostic plots we can say that the model fits the assumption of heteroscedasticity.

# Source: https://www.scribbr.com/statistics/anova-in-r/


