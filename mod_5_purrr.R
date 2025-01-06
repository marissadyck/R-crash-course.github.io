
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Module 5: The wonders of Purrr

library(tidyverse)

bobcat_collection_data <- read_csv('data/raw/bobcat_collection_data.csv')

bobcat_necropsy_only_data <- read_csv('data/raw/bobcat_necropsy_only_data.csv')

bobcat_age_data <- read_csv('data/raw/bobcat_age_data.csv')

# assign objects as list provide the names and file path of each data frame

bobcat_data <- list('data/raw/bobcat_collection_data.csv',
                    'data/raw/bobcat_necropsy_only_data.csv',
                    'data/raw/bobcat_age_data.csv') %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    ~read_csv(.x)
  )

# look at the list structure
str(bobcat_data)

# assign object name to environment and provide file path for the data
bobcat_data <- file.path('data/raw', 
  
  # provide the names of each data frame
  c('bobcat_collection_data.csv',
    'bobcat_necropsy_only_data.csv',
    'bobcat_age_data.csv')) %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    ~read_csv(.x))

str(bobcat_data)
# assign object name to environment and provide list of data file names

bobcat_data <- list('bobcat_collection_data.csv',
                    'bobcat_necropsy_only_data.csv',
                    'bobcat_age_data.csv') %>% 
  
  # provide file path for the data
  file.path('data/raw', .) %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    ~read_csv(.x))

str(bobcat_data)
# first see what the list elements are named
names(bobcat_data)


# assign object name to environment and provide file path for the data
bobcat_data <- file.path('data/raw', 
  
  # provide the names of each data frame
  c('bobcat_collection_data.csv',
    'bobcat_necropsy_only_data.csv',
    'bobcat_age_data.csv')) %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    ~read_csv(.x)) %>% 
  
  # assign names to list objects
purrr::set_names('bobcat_collection',
                   'bobcat_necropsy',
                   'bobcat_age')

str(bobcat_data)
# look at the structure of the necropsy data
str(bobcat_data$necropsy)

bobcat_collection_data <- read_csv('data/raw/bobcat_collection_data.csv') %>% 
  
  # set names to lowercase
  set_names(
    names(.) %>% 
      tolower())

head(bobcat_collection_data)

bobcat_necropsy_only_data <- read_csv('data/raw/bobcat_necropsy_only_data.csv')%>% 
  
  # set names to lowercase
  set_names(
    names(.) %>% 
      tolower())

head(bobcat_necropsy_only_data)

bobcat_age_data <- read_csv('data/raw/bobcat_age_data.csv')%>% 
  
  # set names to lowercase
  set_names(
    names(.) %>% 
      tolower())

head(bobcat_age_data)
# assign object name to environment and provide file path for the data
bobcat_data <- file.path('data/raw', 
                         
                         # provide the names of each data frame
                         c('bobcat_collection_data.csv',
                           'bobcat_necropsy_only_data.csv',
                           'bobcat_age_data.csv')) %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    
    # reference list elements with ~
    ~.x %>% 
      
      # read in list elements   
      read_csv() %>% 
      
      # set names to lower case
      set_names(
        names(.) %>% 
          tolower())
  ) %>% 
  
  # assign names to list objects
purrr::set_names('bobcat_collection',
                   'bobcat_necropsy',
                   'bobcat_age')

head(bobcat_data$bobcat_collection)
# assign object name to environment and provide file path for the data
bobcat_data <- file.path('data/raw', 
                         
                         # provide the names of each data frame
                         c('bobcat_collection_data.csv',
                           'bobcat_necropsy_only_data.csv',
                           'bobcat_age_data.csv')) %>% 
  
  # use purrr::map to read in all data at once
  purrr::map(
    
    # reference list elements with ~
    ~.x %>% 
      
      # read in list elements   
      read_csv() %>% 
      
      # set names to lower case
      set_names(
        names(.) %>% 
          tolower()) %>% 
      
      # change bobcats id# to better name
      rename(.,
             'bobcat_id' = 'bobcat_id#') # new name = old name
  ) %>% 
  
  # assign names to list objects
  purrr::set_names('bobcat_collection',
                   'bobcat_necropsy',
                   'bobcat_age')

head(bobcat_data$collection)

# save each data set as a csv
write_csv(bobcat_data$bobcat_collection,
          'data/processed/bobcat_collection.csv')

write_csv(bobcat_data$bobcat_age,
          'data/processed/bobcat_age.csv')

write_csv(bobcat_data$bobcat_necropsy,
          'data/processed/bobcat_necropsy.csv')
# save each data set as a csv
purrr::imap(
  bobcat_data,
  ~write_csv(.x,
             file = paste0("data/processed/",
                           .y,
                           '.csv')))
