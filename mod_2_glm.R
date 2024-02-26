
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Statistical Modeling
# Module 1: Generalized linear models (GLMs)

# Libraries ----------------------

library(tidyverse)
library(PerformanceAnalytics)
library(lme4)
library(rphylopic)
library(MuMIn)
library(car)
library(AER)

# Data ----------------------

# Read in data for this module

cows <- read.csv('data/processed/cows.csv')

# check data structure
str(cows)


# look at full data
# View(cows) # this is commented out because rMarkdown won't run the View() function

# check out info on response variable
summary(cows$damage)


# plot the response variable
hist(cows$damage)


# Autocorrelation test --------------------

# first let's make a subset of the data that only includes explanatory variables we plan to use in models we will assign this to the environment as cows_cor

cows_cor <- 
  cows %>% 
  select(bear_abund,
         s.dist_to_forest,
         s.dist_to_town,
         openhab_10k,
         ag_10k,
         heteroag_10k,
         forest_10k,
         urban_10k,
         shdi_10k)

# check that this worked by looking at the first few rows of data
head(cows_cor)

# correlation matrix - Pearson
chart.Correlation(cows_cor, 
                  histogram = TRUE, 
                  method = "pearson")

# there are a number of non-linear relationships so we should use the Spearman method

# correlation matrix - Spearman
chart.Correlation(cows_cor, 
                  histogram = TRUE, 
                  method = "spearman")


# GLM syntax ----------------------

# glm(response variable ~ explanatory variable 1 + explanatory variable 2,    
#     data = your data,   
#     family = chosen distribution)
    

# GLM ----------------------

# run a global GLM which includes all variables not highly correlated
cows_global <- glm(damage ~ bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     openhab_10k +
                     ag_10k + 
                     shdi_10k,
                   data = cows, 
                   family = binomial)


# Test assumptions ----------------------

# Homogeneity of variance

cows <- cows %>% 
  
  # change year to a grouped variable (factor)
  mutate(year = as.factor(year))
  
  # run levene test
leveneTest(damage ~ year,
                   data = cows)


# Dispersion

# test for over dispersion
summary(cows_global)

# calculate chi square approx. for residual deviance
1629/1613 # 1.0009 

# we want a value ~1 so our data is okay
# value >1 is over dispersed
# value <1 is under dispersed

# Results ----------------------

summary(cows_global)

# calculate odds ratio for coefficients
exp(coefficients(cows_global))

# re-run scaled model

# scale variables inside the glm model to get effect sizes when looking at coefficients
cows_global <- glm(damage ~ scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     
                     # the rest of the variables were already scaled when extracted in GIS
                     openhab_10k +
                     ag_10k + 
                     shdi_10k,
                   data = cows, 
                   family = binomial)

summary(cows_global)
# calculate odds ratio for scaled coefficients
exp(coefficients(cows_global))

# inverse logit of coefficients to get probabilities 
plogis(coefficients(cows_global))

# Newdata ----------------------

# first create a new data frame that includes all variables in the model (spelled EXACTLY the same) and where one variable (the one we want to graph) has a range from the min to max value in our data and the other variables are held constant at the mean value from the data.

# let's start with bear abundance
new_data_bear <- expand.grid(bear_abund = seq(min(cows$bear_abund), 
                                              max(cows$bear_abund),
                                              by = 0.1),
                             dist_to_forest = mean(cows$dist_to_forest),
                             dist_to_town = mean(cows$dist_to_town),
                             openhab_10k = mean(cows$openhab_10k),
                             ag_10k = mean(cows$ag_10k),
                             shdi_10k = mean(cows$shdi_10k))

# look at what we created
head(new_data_bear)
# use predict function to get predicted probabilities of cow damage based on our model
new_data_bear$pred <- predict(cows_global,
                              type = 'response',
                              newdata = new_data_bear)

head(new_data_bear)
# use predict function to get predicted probabilities of cow damage based on our model
new_data_bear_pred <- predict(cows_global,
                              type = 'response',
                              se.fit = TRUE,
                              newdata = new_data_bear)

head(new_data_bear_pred)

new_data_bear_pred <- cbind(new_data_bear,
                            new_data_bear_pred) %>% 
  
  # add column for lower and upper 95% CI using manual calculation from SE
  mutate(lwr = fit - (1.96*se.fit),
         upr = fit + (1.96*se.fit))

head(new_data_bear_pred)



# generate new data with typo
new_data_bear_typo <- expand.grid(bear_abund = seq(min(cows$bear_abund), 
                                              max(cows$bear_abund),
                                              by = 0.1),
                             dist_to_forest = mean(cows$dist_to_forest),
                             dist_to_town = mean(cows$dist_to_town),
                             open_hab10k = mean(cows$openhab_10k),
                             ag_10k = mean(cows$ag_10k),
                             shdi_10k = mean(cows$shdi_10k))

# look at what we created
head(new_data_bear_typo)
# add predicted probabilities to new data with typo
new_data_bear_typo$pred <- predict(cows_global,
                              type = 'response',
                              newdata = new_data_bear_typo)
# Graphs ----------------------

# create graph with predicted prob x bear abundance
ggplot(data = new_data_bear_pred, aes(x = bear_abund, y = fit)) +
  
  # add line for predicted prob
  geom_line() +
  
  # add error bar
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.5) # changes opacity so you can see the main line

# save phylopic image for cows
cows_phylopic <- get_phylopic("cba95817-8806-49c2-932c-fb69a644c53d")

# create graph with predicted prob x bear abundance
ggplot(data = new_data_bear_pred, aes(x = bear_abund, y = fit)) +
  
  # add line for predicted prob
  geom_line() +
  
  # add error bar
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.5) + # changes opacity so you can see the main line 
  
  # add raw data points
  geom_jitter(data = cows,
             aes(x = bear_abund,
                 y = damage),
             shape = 16,
             size = 1.5,
             width = 1,
             height = 0.05,
             alpha = 0.5) + # make points opaque so can see overlapping points
  
  # change axis labels
  labs(x = 'Relative bear abundance',
       y = 'Predicted probability of bear preadtion') +
  
  # expand y axis
  coord_cartesian(ylim = c(0, 1)) +
  
  # add ticks for x axis at 10
  scale_x_continuous(breaks = seq(0, 80, 10),
                     
                     # expand removes spacing between plot and border
                     expand = c(0,0)) +
  
  # remove grey background and add border
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.title = element_text(size = 12)) +
  
  # add cow image
  add_phylopic(cows_phylopic, 
               alpha = 1,
               ysize = 5,
               x = 73,
               y = 0.98)

# Model selection ----------------------

# models for model selection

# we already ran cows_global but here is code again
cows_global <- glm(damage ~ scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     openhab_10k +
                     ag_10k + 
                     shdi_10k,
                   data = cows, 
                   family = binomial)

# null
cows_null <- glm(damage ~ 1,
                 data = cows,
                 family = binomial)

# bears and broad landscape model
cows_1 <- glm(damage ~ scale(bear_abund) +
                openhab_10k +
                ag_10k +
                shdi_10k,
              data = cows, 
              family = binomial)

# bears + proximity of grazing cattle to bear habitat
cows_2 <- glm(damage ~ scale(bear_abund) + 
                scale(dist_to_forest) +
                openhab_10k,
              data = cows, 
              family = binomial)

# bears + proximity of grazing cattle to human protection
cows_3 <- glm(damage ~ scale(bear_abund) + 
                scale(dist_to_town),
              data = cows, 
              family = binomial)

# bears + proximity to bears and humans and habitat needs for cows and bears
cows_4 <- glm(damage ~ scale(bear_abund) + 
                scale(dist_to_town) +
                scale(dist_to_forest) +
                openhab_10k +
                ag_10k,
              data = cows, 
              family = binomial)


model.sel(cows_global,
          cows_null,
          cows_1,
          cows_2,
          cows_3,
          cows_4)
