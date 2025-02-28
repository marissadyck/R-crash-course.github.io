
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada  

#  Statistical Modeling
# Module 3: GLMMs


library(tidyverse)
library(lme4) # fitting glmss
library(MuMIn) # model selection
# Data ----------------------

# Read in data for this module

cows <- read_csv('data/raw/cows.csv',
                 
                 col_types = cols(Damage = col_factor(),
                                  Year = col_factor(),
                                  Month = col_factor(),
                                  Targetspp = col_factor(),
                                  Hunting = col_factor(),
                                  .default = col_number())) %>% 
  
  rename_with(tolower)

# check data structure
str(cows)
# GLMM ----------------------

# run a global GLM which includes all variables not highly correlated including a random intercept
cows_global_ri <- glmer(damage ~ altitude +
                    bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex +
                     
                     # random intercept for year
                   (1|year),
                   
                   data = cows, 
                   family = binomial
                   )

summary(cows_global_ri)
# generate new data or overwrite earlier data
cows_scaled <- cows %>% 
  
  mutate_if(is.numeric,
            scale)

head(cows_scaled)
# run a global GLM which includes all variables not highly correlated including a random intercept
cows_global_ri <- glmer(damage ~ altitude +
                    bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex +
                     
                     # random intercept for year
                   (1|year),
                   
                   data = cows_scaled, 
                   family = binomial
                   )

summary(cows_global_ri)

r.squaredGLMM(cows_global_ri)
# run a global GLM which includes all variables not highly correlated including a random intercept
cows_global_rs <- glmer(damage ~ 
                    bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex +
                     
                     # random slope each year (0 indicates no random intercept)
                   (0 + altitude | year),
                   
                   data = cows_scaled, 
                   family = binomial
                   )

summary(cows_global_rs)
# run a global GLM which includes all variables not highly correlated including a random intercept
cows_global_r <- glmer(damage ~ 
                    bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex +
                     
                     # random slope each year (1 indicates a random intercept)
                   (1 + altitude | year),
                   
                   data = cows_scaled, 
                   family = binomial
                   )

summary(cows_global_r)

# run glm model

cows_global <- glm(damage ~ 
                    bear_abund + 
                     dist_to_forest + 
                     dist_to_town +
                     prop_ag +
                     prop_open +
                     shannondivindex,
                   data = cows_scaled, 
                   family = binomial
                   )

# perform model selection
model.sel(cows_global,
        cows_global_r,
        cows_global_ri,
        cows_global_rs)
