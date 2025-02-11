
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Statistical Modeling
# Module 2: Generalized linear models (GLMs)

# Libraries ----------------------

library(tidyverse) # for data cleaning and tidying operations
library(PerformanceAnalytics) # for generating correlation plots
library(lme4) # fitting glms
library(rphylopic) # adding silhouettes to graphs
library(MuMIn) # model selection
library(car)
library(AER) # testing for dispersion w/ poisson distribution
library(broom)
library(pROC) # calculating area under the curve
library(ResourceSelection) # Package with function to test dispersion for 
library(ggeffects) # getting model predictions in tibble format for plotting
library(MASS)


# Data ----------------------


# INSERT YOUR CODE HERE ----------------------
# Read in data for this module

cows <- read_csv('data/raw/cows.csv',
                 
                 col_types = cols(Damage = col_factor(),
                                  Year = col_factor(),
                                  Month = col_factor(),
                                  Targetspp = col_factor(),
                                  Hunting = col_factor(),
                                  .default = col_number())) %>% 
  
  rename_with(tolower)



# look at full data


# check out info on response variable
str(cows)



# multicolinearity test --------------------

# first let's make a subset of the data that only includes explanatory variables we plan to use in models we will assign this to the environment as cows_cor

# INSERT YOUR CODE HERE --------------------


cows_cor <- cows %>% 
  
  dplyr::select(bear_abund,
         altitude,
         human_population,
         dist_to_forest,
         dist_to_town,
         shannondivindex,
         prop_ag,
         prop_gras,
         prop_open,
         prop_forest)


# check that this worked by looking at the first few rows of data
head(cows_cor)

# correlation matrix - Pearson-
chart.Correlation(cows_cor, 
                  histogram = TRUE, 
                  method = "pearson")

# there are a number of non-linear relationships so we should use the Spearman method

# correlation matrix - Spearman
chart.Correlation(cows_cor, 
                  histogram = TRUE, 
                  method = "spearman")

# histograms of explanatory variables --------------------

hist(cows$altitude)
hist(cows$human_population)
hist(cows$bear_abund)
hist(cows$dist_to_forest)
hist(cows$dist_to_town)
hist(cows$shannondivindex)
hist(cows$prop_forest)
hist(cows$prop_ag)
hist(cows$prop_open)

# or using purrr and the cows.cor data
cows_cor %>% 
  
  # use imap which will retain both the data (x) and the variable names (y)
  imap(~.x %>% 
        
         # use the hist function on the data from previous pipe
        hist(.,
             
             # set the main title to y (each variable)
             main = .y))


# GLM syntax ----------------------

# glm(response variable ~ explanatory variable 1 + explanatory variable 2,    
#     data = your data,   
#     family = chosen distribution)
    

# GLM ----------------------

# run a global GLM which includes all variables not highly correlated
cows_global <- glm(damage ~ altitude +
                     bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex,
                   data = cows, 
                   family = binomial)


summary(cows_global)

# VIF ----------------------

# report VIF
vif(cows_global)

# additional plot of each variable
# calculate vif
vif(cows_global) %>%
  
  # Converts the named vector returned by vif() into a tidy tibble
  enframe(name = 'Predictor', 
          value = 'VIF') %>%
  
  # plot with ggplot
  ggplot(aes(x = reorder(Predictor, VIF), # reorders from smallest VIF to largest (not sure I want like this)
             y = VIF)) +
  
  # plot as bars
  geom_bar(stat = 'identity', fill = 'skyblue') +
  
  # add labels
  labs(x = 'Predictor',
       y = 'VIF') +
  
  # set theme
  theme_classic()



# Dispersion ----------------------

# test for over dispersion
summary(cows_global)

# calculate chi square approx. for residual deviance
1624/1631 # 0.99

# we want a value ~1 so our data is okay
# value >1 is over dispersed
# value <1 is under dispersed

hoslem.test(cows$damage, fitted(cows_global))
dispersiontest(cows_global)

plot(cows_global)


# Results ----------------------

summary(cows_global)

# calculate odds ratio for coefficients
exp(coefficients(cows_global))


# Scale variables ----------------------

# re-run scaled model

# scale variables inside the glm model to get effect sizes when looking at coefficients
cows_global <- glm(damage ~ scale(altitude) +
                     scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     scale(prop_ag) +
                     scale(prop_open) +
                     scale(shannondivindex),
                   data = cows, 
                   family = binomial)

summary(cows_global)


# Odds ----------------------

# calculate odds ratio for scaled coefficients
exp(coefficients(cows_global))


model_odds <- 
tidy(cows_global,
     exponentiate = TRUE,
     confint.int = TRUE) %>% 
  
  # bind the estimates with the confidence intervals from the model
  cbind(exp(confint(cows_global))) %>% 
  
  # change format to a tibble so works nicely with ggplot
  as.tibble() %>% 
  
  # rename the autoformatted columns to names that R doesn't hate (e.g. lower and upper not using special characters or spaces)
  rename(lower = '2.5 %',
         upper = '97.5 %') %>% 
  
  # remove the intercept term because we don't care to plot the odds for this
  filter(term != '(Intercept)')

# view
model_odds

# plot odds

# specify data and mapping asesthetics
ggplot(data = model_odds,
       aes(x = term,
           y = estimate)) +
  
  # add points for the odss
  geom_point() +
  
  # add errorbars for the confidence intervals
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                linewidth = 0.5,
                width = 0.4) +
  
  geom_hline(yintercept = 1,
             alpha = 0.5) +
  
  # rename the x axis labels
  scale_x_discrete(labels = c('Altitude',
                              'Bear abundacne',
                              'Distance to forest',
                              'Distance to town',
                              'Agirculture',
                              'Open Habitat',
                              'Shannon Diversity Index')) +
  
  # rename y axis title
  ylab('Odds ratio') +
  
  # flip x and y axis 
      coord_flip() +

  # specify theme
  theme_bw() +
  
  # specify theme elements
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank())

# inverse logit of coefficients to get probabilities 
plogis(coefficients(cows_global))


# Newdata ----------------------

# first create a new data frame that includes all variables in the model (spelled EXACTLY the same) and where one variable (the one we want to graph) has a range from the min to max value in our data and the other variables are held constant at the mean value from the data.

# let's start with bear abundance
new_data_bear <- expand.grid(bear_abund = seq(min(cows$bear_abund), 
                                              max(cows$bear_abund),
                                              by = 0.1),
                             altitude = mean(cows$altitude),
                             dist_to_forest = mean(cows$dist_to_forest),
                             dist_to_town = mean(cows$dist_to_town),
                              prop_ag = mean(cows$prop_ag),
                            prop_open = mean(cows$prop_open),
                             shannondivindex = mean(cows$shannondivindex))

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
                                  altitude = mean(cows$altitude),
                                  dist_to_forest = mean(cows$dist_to_forest),
                                  dist_to_town = mean(cows$dist_to_town),
                                  prop_ag = mean(cows$prop_ag),
                                  prop_ope = mean(cows$prop_open),
                                  shannondivindex = mean(cows$shannondivindex))

# look at what we created
head(new_data_bear_typo)
# add predicted probabilities to new data with typo
new_data_bear_typo$pred <- predict(cows_global,
                              type = 'response',
                              newdata = new_data_bear_typo)




# tidyverse way to do all the code above :)

new_data_bear_tidy <- ggpredict(cows_global,
                                terms = 'bear_abund')

head(new_data_bear_tidy)

# Graphs ----------------------

# create graph with predicted prob x bear abundance
ggplot(data = new_data_bear_tidy, 
       aes(x = x, 
           y = predicted)) +
  
  # add line for predicted prob
  geom_line() +
  
  # add error bar
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
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
               ysize = 0.1,
               x = 73,
               y = 0.98)

# Model selection ----------------------

# models for model selection

# we already ran cows_global but here is code again
cows_global <- glm(damage ~ scale(altitude) +
                     scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     scale(prop_ag) +
                     scale(prop_open) +
                     scale(shannondivindex),
                   data = cows, 
                   family = binomial)

# null
cows_null <- glm(damage ~ 1,
                 data = cows,
                 family = binomial)

# cows bears
cows_bears <- glm(damage ~  scale(bear_abund),
                   data = cows, 
                   family = binomial)

# broad landscape model
cows_landscape <- glm(damage ~ scale(altitude) +
                     scale(prop_ag) +
                     scale(prop_open),
                   data = cows, 
                   family = binomial)

# bears + proximity of grazing cattle to bear habitat
cows_bear_prox <- glm(damage ~ scale(bear_abund) + 
                scale(dist_to_forest),
              data = cows, 
              family = binomial)

# bears + proximity of grazing cattle to human protection
cows_human_prox <- glm(damage ~ scale(bear_abund) + 
                scale(dist_to_town),
              data = cows, 
              family = binomial)

# bears + proximity to bears and humans and habitat needs for cows and bears
cows_alt <- glm(damage ~ scale(altitude),
                   data = cows, 
                   family = binomial)


# Model selection ---------------------------------------------------------


# compare models with model.sel
model.sel(cows_global,
          cows_null,
          cows_bear_prox,
          cows_bears,
          cows_alt,
          cows_human_prox)

# plot chosen model
plot(cows_global)


# AOC ---------------------------------------------------------------------



c.roccurve <- roc(cows$damage, 
                  predict(cows_global, type = 'response'))

auc(c.roccurve)

plot.roc(c.roccurve, main="Area Under the Curve for best model")

summary(cows_global)

# 1 - (Residual Deviance/Null Deviance)

1 - (1624/1843.9) # you want a high r square so this is not a very good fitting model

# or from the model directly
with(summary(cows_global), 1 - deviance/null.deviance)

# YOUR CODE HERE ----------------------




# view summary of model







# define the reference level of hunting

cows_global_hunt <- glm(damage ~ scale(altitude) +
                     scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     scale(prop_ag) +
                     scale(prop_open) +
                     scale(shannondivindex) +
                      relevel(hunting, ref = '0'),
                   data = cows, 
                   family = binomial)

# print summary
summary(cows_global_hunt)

# YOUR CODE HERE ----------------------

# add year to global model




# summary



# YOUR CODE HERE ----------------------

# add year to global model with ref level as 2008


#summary



# Ordinal  ----------------------------------------------------------------



# re read in data and set year to ordered factor
cows <- read_csv('data/raw/cows.csv',
                 
                 col_types = cols(Damage = col_factor(),
                                  Year = col_factor(levels = c('2008',
                                                               '2009',
                                                               '2010',
                                                               '2011',
                                                               '2012',
                                                               '2013',
                                                               '2014',
                                                               '2015',
                                                               '2016'), ordered = TRUE),
                                  Month = col_factor(),
                                  Targetspp = col_factor(),
                                  Hunting = col_factor(),
                                  .default = col_number())) %>% 
  
  rename_with(tolower)

# check data structure
str(cows)


# add year to global model after set as an ordinal variable
cows_global_yr_o <- glm(damage ~ scale(altitude) +
                     scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     scale(prop_ag) +
                     scale(prop_open) +
                     scale(shannondivindex) +
                      year,
                   data = cows, 
                   family = binomial)

summary(cows_global_yr_o)



# apply successive differences coding to compare year-to-year changes
contrasts(cows$year) <- contr.sdif(nlevels(cows$year))

# add year to global model after set as an ordinal variable
cows_global_yr_o <- glm(damage ~ scale(altitude) +
                     scale(bear_abund) + 
                     scale(dist_to_forest) + 
                     scale(dist_to_town) +
                     scale(prop_ag) +
                     scale(prop_open) +
                     scale(shannondivindex) +
                      year,
                   data = cows, 
                   family = binomial)

# model summary
summary(cows_global_yr_o)


# Modeling interactions ----------------------

# model without interactions
cows_hunt_town <- glm(damage ~ scale(dist_to_town) +
                   hunting,
                 data = cows, 
                   family = binomial)

# model with interactions
 cows_hunt_town_i <- glm(damage ~ scale(dist_to_town)*hunting,
                 data = cows, 
                   family = binomial)

summary(cows_hunt_town_i)

# model selection
model.sel(cows_hunt_town,
          cows_hunt_town_i)
