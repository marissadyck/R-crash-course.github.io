
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Module 4: data manipulation

library(tidyverse)

  # import data FILL IN YOUR CODE HERE ----------------------

# read in altered turtles data




# check internal structure



# code for altering turtle data
turtles.df <- read_delim('data/raw/turtle_data.txt',
                         delim = '\t') %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  
      tolower()) %>% 
  
  # rename columns to shorter names
  rename(tag = tag_number,
         c_length = carapace_length,
         h_width = head_width) %>% 
  
  # change sex to factor variable
  mutate(sex = as.factor(sex))


# save to data/processed folder as 'turtles_tidy.csv'
write_csv(turtles.df,
          'data/processed/turtles_tidy.csv')


# Sorting/ordering data --------------------

#  Sorting using arrange

# To sort a data frame by one vector (variable), you can use arrange()
turtles_tidy %>% 
  arrange(c_length)

# or if we want in descending order...
turtles_tidy %>% 
  arrange(desc(c_length))

# Sorting by 2 columns - use a comma to separate columns
turtles_tidy %>% 
  arrange(sex, weight)

# Practice FILL IN YOUR CODE HERE ----------------------

# practice- arrange diamonds data by highest price to lowest price



# order columns alphabetically with select
turtles_tidy %>% 
  select(order(colnames(.)))   


# ordering columns with relocate

# example with turtles_tidy data
turtles_tidy %>% 
  
  # move 'weight' before 'c_length'
  relocate(weight, 
           .before = c_length)
  
# alternatively we could specify to have weight AFTER 'sex'
turtles_tidy %>% 
  
  # relocate 'weight' after 'sex'
  relocate(weight,
           .after = sex)

# if we don't specify a .before or .after argument it will move the selected column to the front of the data
turtles_tidy %>% 
  relocate(weight)

# ordering data

# reordering multiple columns by name
turtles_tidy %>% 
  
  # move 'weight', 'c_length', and 'h_width' before 'tag' and 'sex'
  relocate(c(weight, c_length, h_width))
  
# alternatively we could specify which variables to relocate by their type
turtles_tidy %>% 
  
  # move all numeric variables to the front
  relocate(where(is.numeric))

# or
turtles_tidy %>% 
  
  # move all numeric variables after 'tag'
  relocate(where(is.numeric),
                 .after = tag)
# practice with diamonds data

# reorder columns so price is first
diamonds %>% 
  
  # move price to front
  relocate(price) # most parsimonious way

# or
diamonds %>% 
  
  # move price to front
  relocate(price,
           .before = carat)


turtles_tidy %>% 
  
  # move factors to front
  relocate(where(is.factor))


# check structure of turtles_tidy
str(turtles_tidy)


# Data manipulation --------------------

# FILL IN YOUR CODE HERE --------------------

# first read in data for this section




# check internal structure


 

  # Replace ---------------------

# fix issue with multiple entries for female
turtles_tidy <-  turtles_tidy %>% 
  mutate(sex = replace(sex, 
                       sex == "fem",
                       "female"))

# print
turtles_tidy

levels(turtles_tidy$sex)

# FILL IN YOUR CODE HERE --------------------

# change 'sex' back to a character then try the data manipulation again to see if anything is different. We can do this all in one pipe!
turtles_tidy <- turtles_tidy %>% 
  
  # sex to character
  mutate(sex = as.character(sex),
         
         # change fem to female with replace
         replace(sex,
                 sex == 'fem',
                 'female'),
         
         # sex to factor
         sex = as.factor(sex))

head(turtles_tidy)

levels(turtles_tidy$sex)



  # Recode  ----------------------

# read in turtles data again and set 'sex' to factor to overwrite the changes we made with the last code chunk, and in the same code chunk we will 'recode' the 'sex' column

# read in altered turtles data
turtles_tidy <- read_csv('data/processed/turtles_tidy.csv') %>% 
  
  # change sex to a factor
  mutate(sex = as.factor(sex),
         sex = recode(sex, 
                       fem = 'female'))

# check data
str(turtles_tidy)

levels(turtles_tidy$sex)

  # import data FILL IN YOUR CODE HERE ----------------------

# read in altered turtles data



head(turtles_tidy)
     
str(turtles_tidy)
 # mutate data FILL IN YOUR CODE HERE ----------------------








# col_types example -------------------------------------------------------


turtles_tidy <- read_csv('data/processed/turtles_tidy.csv',
                         
                          # set the column types to read in correctly
                         col_types = cols(tag = col_factor(),
                                          sex = col_factor(),
                                          c_length = col_number(),
                                          h_width = col_number(),
                                          weight = col_number()))

turtles_tidy <- read_csv('data/processed/turtles_tidy.csv',
                         
                          # set the column types to read in correctly
                         col_types = cols(tag = col_factor(),
                                          sex = col_factor(),
                                          .default = col_number()))



  # If else ----------------------

# first learn morn about the diamonds data set



# change color to two categories based on worst color diamonds (J)
diamonds %>% 
  mutate(color = if_else(color == 'J', # if color is J 
                         'worst', # TRUE = worst
                         'not_worst')) # FALSE = not_worst



# create new column with two categories based on worst color diamonds (J)
diamonds %>% 
  mutate(color_quality = if_else(color == 'J', # if color is J 
                         'worst', # TRUE = worst
                         'not_worst')) # FALSE = not_worst


  # Case when ----------------------

# create new column with two categories based on worst color diamonds (J)
diamonds %>% 
  mutate(clarity = case_when(clarity == 'l1' ~ 8,
                             clarity == 'SI2' ~ 7,
                             clarity == 'SI1' ~ 6,
                             clarity == 'VS2' ~ 5,
                             clarity == 'VS1' ~ 4,
                             clarity == 'VVS2' ~ 3,
                             clarity == 'VVS1' ~ 2,
                             clarity == 'IF' ~ 1))


# group the scores for clarity using case_when
diamonds %>% 
  mutate(clarity = case_when(clarity == 'l1' ~ 'worst',
                             clarity == 'IF' ~ 'best',
                             .default = 'neutral'))


# group the scores for clarity using case_when
diamonds %>% 
  mutate(clarity = case_when(clarity == 'l1' ~ 'poor',
                             clarity == 'SI2' ~ 'poor',
                             clarity == 'WS1' ~ 'good',
                             clarity == 'IF' ~ 'good',
                             .default = 'moderate'))

# using a vector with case_when

# define the  entries we want to be 'poor' quality
poor_quality <- c('l1', 'SI2')

# define the entries we want to be 'good' quality
good_quality <- c('WS1', 'IF')

# group the scores for clarity using case_when and %in%
diamonds %>% 
  mutate(clarity = case_when(clarity %in% poor_quality ~ 'poor',
                             clarity %in% good_quality ~ 'good',
                             .default = 'moderate'))

# using other boolean operations with mutate()

# look at range for price 
summary(diamonds$price) # let's use the mean and 1st and 3rd quartiles to set our levels

# change price to factor/categorical 
diamonds %>%
mutate(price = case_when(price < 950 ~ 'low',
                         price > 950 & price < 5324 ~ 'moderate',
                         price > 5324 ~ 'high'))

# or using the default argument
diamonds %>%
mutate(price = case_when(price < 950 ~ 'low',
                         price > 5324 ~ 'high',
                         .default = 'moderate'))

  # Dealing with NAs ----------------------

  # Check for NAs ----------------------

# read data

# NOTE: you need to specify that this is a tab-delimited file. It is especially important to specify the delimiter for data files with missing data. If you specify the header and what the text is delimited by correctly, it will read missing data as NA. Otherwise it will fail to read data in properly.

missing.df <- read_delim('data/raw/data_missing.txt',
                         delim = "\t")  %>% 
  
  # set names to lowercase
   set_names(
    names(.) %>%  # the period here is a placeholder for the data it tells R to use the element before the last pipe (e.g., turtles.df)
      tolower()) 

# Missing data are read as an NA
missing.df

# You can summarize your data and tell you how many NA's per col
summary(missing.df)

# ?is.na   (Boolean test!)
is.na(missing.df)

complete.cases(missing.df)   # Boolean: for each row, tests if there are no NA values


  # Remove NAs ----------------------

# Base R function that omits (removes) rows with missing data
na.omit(missing.df) # note this removes the entire row, so only do this if you don't want to use any data from an observation with NAs

# we can also use this with %>% to remove rows with NA
missing.df %>% 
  
  # remove rows with NA
  na.omit()


# print missing.df data to compare with data after we remove NAs
missing.df

# use drop_na to remove only rows missing data for 'import'
missing.df %>% 
  
  # remove NAs
  drop_na(import)

# drop NAs for import and export columns
missing.df %>% 
  
  # remove NAs
  drop_na(c(import,
            export))
  # Replace NAs ----------------------

# specifying columns

# first check which columns have NA values
is.na(missing.df) # import and export are the only columns with NAs

# replace NA in import with zeros using mutate() and replace_na()
missing.df %>% 
  
  # replace NA with 0
  mutate(import = replace_na(import, 0),
         export = replace_na(export, 0)) # provide the variable/column name and then what you want the NAs replaced with (e.g. 0)

# or using tidyverse trickery (less code repetition)
missing.df %>% 
  
  # replace NA w/ 0
  mutate(across(
    where(is.numeric),
    ~ replace_na(., 0)))

# for entire data set

# replace NA in missing.df with zeros using replace()
missing.df %>% 
  
  # replace NW w/ 0
  replace(is.na(.), 0)

# Replace all missing values in the data frame with the mean for the column

# check what the mean is for import and export columns
summary(missing.df)

missing.df %>% 
  
  # replace NA with mean value for each column
  mutate(export = replace_na(export,
                             mean(export,
                                  na.rm = T)),
         import = replace_na(import, 
                             mean(import, 
                                  na.rm = T)))

# or using tidyverse trickery (less code repetition)
missing.df %>% 
  
  # replace NA with mean value for each column
  mutate(across(
    where(is.numeric),
    ~ replace_na(.,
                 mean(.,
                      na.rm=T))))

  # Replace values with NA ----------------------

 # FILL IN YOUR CODE HERE ----------------------








# replace 'na' in age using replace function


# replace na in age using recode



# change multiple entries to NA

# make a vector with a string of values that are meant to be NAs in data
na_string <- c('na', 'X')

# use mutate with %in% argument to replace all values in the string above with NA
bobcats %>% 
  mutate(age = replace(age,
                       age %in% na_string,
                       NA))
  
 # Pivot functions ----------------------

# use data from tidyr package
relig_income

relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")

  # Joining data----------------------

# create some fake data first 
turtles_env <- tibble(
  
  # create variable 'tag' ranging from 1 - 105
  tag = as.factor(1:105),
  
  # create variable site with 3 sites that repeat for 35 indv each
  site = rep(c('A', 'B', 'C'), 
             each = 35),
  
  # create a variable for average summer temperature in Celsius
  avg_summer_temp = runif(105,
                          min = 15,
                          max = 30),
  
  # create a variable for average summer precipitation in mm
  avg_summer_precip = runif(105,
                            min = 0,
                            max = 110)
  
)

turtles_env

  # Left join----------------------

turtles_full <- turtles_tidy %>% 
  
  # left join with turtles_env
  left_join(turtles_env,
            by = 'tag')

turtles_full

  # Right join ----------------------

turtles_tidy %>% 
  
  # right join
  right_join(turtles_env,
             by = 'tag') %>% 
  
  # reorder by tag number so we can see what happened
  arrange(desc(tag))



  # Inner join ----------------------

# run inner join without altering data to see what happen

turtles_tidy %>% 
  
  inner_join(turtles_env,
             by = 'tag')

# if we remove some observations (say we didn't collect environmental data for the first 10 captures) then we can see the diff

turtles_env.sub <- turtles_env %>% 
  
  # filter to only tags greater than 10
  filter(tag > 10)

head(turtles_env.sub)

# now join with turtles_tidy

turtles_tidy %>% 
  
  inner_join(turtles_env.sub,
             by = 'tag')

# notice fewer rows

  # Full join ----------------------

turtles_tidy %>% 
  full_join(turtles_env,
            by = 'tag')

  # Mismatched keys----------------------

# rename tag in the turtles_env dataset 
turtles_env <- turtles_env %>% 
  rename(tag_number = tag)

names(turtles_env)

# now join with mismatched names

turtles_tidy %>% 
  left_join(turtles_env,
            join_by('tag' == 'tag_number'))
# check which tags are in turtles tidy that aren't in turtles env
setdiff(levels(turtles_tidy$tag),
        levels(turtles_env$tag))

# and reversed
setdiff(levels(turtles_env$tag),
        levels(turtles_tidy$tag))
