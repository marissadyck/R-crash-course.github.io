
#  R Crash Course, Day 1 Module 2
 # Casa Tisaru Lepsa, Romania 

#  Working with data

library(tidyverse)


# Working directories --------------------

# Find the directory you're working in 
getwd()          # note: the results from running this command on my machine will differ from yours!  


#  Import/Export data files into R with base R----------------------

# read.table to import textfile (if no delim specified, it will try to guess!)
data.txt.df <- read.table("data/data.txt")  



# Look at data by printing it in the console
# You can also look at data by using the View() function
# Or you can click on the object (data.txt.df) in your environment to view it
data.txt.df


# read.csv to import textfile with columns separated by commas
data.df <- read.csv("data/data.csv")

#print data
data.df

# Remove redundant objects from memory
rm(data.txt.df)


  # Using Rs built-in data----------------------

# Look at all built-in data files
data()   


# read built-in data on car road tests performed by Motor Trend
data(mtcars) 

# inspect the first few lines
head(mtcars)   

# learn more about this built-in data set
# ?mtcars        


  # Basic data checking ----------------------

# ?str: displays the internal structure of the data object
str(mtcars)
str(data.df)



# get summary of the variables (columns) of a data object
summary(data.df)


# get summary info for a specific variable
summary(data.df$Import)


# Exporting data (save to hard drive as data file)

# ?write.csv: writes a CSV file to the working directory

# to save an entire data frame to your hard drive you would use the format below
# write.csv(data name in R, file = 'data name to save as.csv')

# however if we did this for data.df we would just be making a copy, instead we can export a subset of the data we just ran in
write.csv(data.df[, c("Country","Product")], # this tells R to save all rows for the 'Country' and 'Product' columns
          file = "data_export.csv")   


# Working with data in R ------------------------

  # Assigning objects ----------------------

# <- assignment operator
# =  alternative assignment operator *avoid using this to assign objects to the environment

a <- 3     # assign the value "3" to the object named "a"
a = 3      # assign the value "3" to the object named "a" (alternative)
a == 3     # answer the question: "does the object "a" equal "3"? 


  # Boolean operations ----------------------

# Basic operators

# <    less than
# >    greater than
# <=   less than or equal to
# >=   greater than or equal to
# ==   equal to
# !=   not equal to
# %in% matches one of a specified group of possibilities

# Combining multiple conditions

# &    must meet both conditions (AND operator)
# |    must meet one of two conditions (OR operator)

Y <- 4
Z <- 6

Y == Z  #I am asking if Y is equal to Z, and it will return FALSE
Y < Z

!(Y < Z)  # the exclamation point reverses any boolean object (read "NOT")

# Wrong!
data.df[, 2] = 74     # sets entire second column equal to 74! OOPS WE GOOFED UP!!!

data.df <- read.csv("data/data.csv")  ## correct our mistake in the previous line (revert to the original data)!

# Right
data.df[, 2] == 74    # tests each element of column to see whether it is equal to 74

data.df[, 2] < 74 | data.df[, 2] == 91   # use the logical OR operator


  # Subsetting data base R----------------------

# select those rows of data for which second column is less than 74
data.df[data.df[, "Import"] < 74, ]  

# select rows of data for which second column is greater than 25 AND less than 75

data.df[data.df[, 'Import'] > 25 & 
          data.df[, 'Import'] <75, ]

  # Summary statistics base R ----------------------

mean(data.df$Import)
sd(data.df$Import)


  # Packages----------------------

# Install packages 

# install.packages("tidyr")


# Load packages to library 

library(tidyr)


  # Package datasets ----------------------

ggplot2::diamonds   # note the use of the package name followed by two colons- this is a way to make sure you are using a function (or data set or other object) from a particular package... (sometimes several packages have functions with the same name...)



  # The wonders of tidyverse ----------------------

  # Read in tidy data ----------------------

tidy_data.txt <- read_delim('data/data.txt')

# print data
tidy_data.txt


# check the object type of data

class(tidy_data.txt)


tidy_data.csv <- read_csv('data/data.csv')

# print the first few rows of data
head(tidy_data.csv)


# in the console type
# rm(tidy_data.txt)
# rm(data.df)

  # Export tidy data ----------------------

# ?write_csv

# write_csv(data, 'data_name_to_save.csv')

  # The pipe operator ----------------------

# start by assigning a value to x
x <- 3

# call the variable x followed by %>% ('then')
x %>% 
  
  # calculate the log of x, 'then'
  log() %>% 
  
  # round the log of x to 2 digits
  round(digits = 2) 


# Data exploration with tidyverse ----------------------

# read turtle_data.txt in to R using readr
turtles.df <- read_delim('data/turtle_data.txt',
                         delim = '\t') # specify tab-delimited file



# Renaming columns ----------------------

# check names of turtles data before changing
names(turtles.df)

turtles.df %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  # the period here is a placeholder for the data it tells R to use the element before the last pipe (e.g., turtles.df)
      tolower()) 

# check the names of turtles data
names(turtles.df)

rm(turtles.df)


# read turtle_data.txt in to R using readr
turtles.df <- read_delim('data/turtle_data.txt',
                         delim = '\t') %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  
      tolower()) 

summary(turtles.df)

# extract the column names for turtle data
names(turtles.df)

# to change the column names, use the "names()" function
names(turtles.df)  <- c("tag", "sex","c_length", "h_width", "weight")

# let's check
head(turtles.df)

# read turtle_data.txt in to R using readr
turtles.df <- read_delim('data/turtle_data.txt',
                         delim = '\t') %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  
      tolower()) %>% 
  
  # rename columns to shorter names
  rename(tag = tag_number,
         c_length = carapace_length,
         h_width = head_width)

summary(turtles.df)

# Summary stats the tidyverse way ----------------------

# calculate the mean and sd for three columns in the turtles data using base R

# carapace length
mean(turtles.df$c_length)
sd (turtles.df$c_length)

# head width
mean(turtles.df$h_width)
sd(turtles.df$h_width)

# weight
mean(turtles.df$weight)
sd(turtles.df$weight)

# calculate the mean and sd for three columns in the turtles data using dplyr

turtles.df %>% 
  
  # use summarise function to get mean and sd for multiple columns
  summarise(cl_mean = mean(c_length),
            cl_sd = sd(c_length),
            hw_mean = mean(h_width),
            hw_sd = sd(h_width),
            w_mean = mean(weight),
            w_sd = sd(weight)
  )


turtles.df %>% 
summarise_all(.funs = list(mean, sd))


turtles.df %>% 
summarise_if(is.numeric,
             .funs = list(mean, sd))


# ?summarise_if()

by_species <- iris %>%
  group_by(Species)

by_species %>%
  summarise(across(everything(), list(min = min, max = max)))

turtles.df %>% 
summarise_if(is.numeric,
             .funs = list(mean = mean, 
                          sd = sd))
# Change variable type ----------------------

# Change sex to factor
turtles.df %>% 
  mutate(sex = as.factor(sex)) # we use the same name for our 'new' column so that it modifies the old column instead of creating a new one

# check structure
str(turtles.df) # sex is still a character, why?

# Change sex to factor as save changes to data
turtles.df <-  turtles.df %>% 
  mutate(sex = as.factor(sex))

# check structure
str(turtles.df) 

# read turtle_data.txt in to R using readr
turtles.df <- read_delim('data/turtle_data.txt',
                         delim = '\t') %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  
      tolower()) %>% 
  
  # rename columns to shorter names
  rename(tag = tag_number,
         c_length = carapace_length,
         h_width = head_width) %>% 
  
  # change sex to factor
  mutate(sex = as.factor(sex))



# read turtle_data.txt in to R using readr
turtles.df %>% 
  mutate_if(is.numeric, as.character)

  # Subsetting data with tidyr ----------------------

  # Filter ----------------------

# subset turtles data
turtles.df %>% 
  
  # return data for males only
  filter(sex == 'male')

turtles.df %>% 
  
  # return data for males only that are larger than 5 (grams?)
  filter(sex == 'male' &
           weight > 5)

  # Select ----------------------

turtles.df %>% 
  
  # return data for tag and weight only
  select(tag, weight)
turtles.df %>%
  
  # return data for males only with weight greater than 5
  filter(sex == 'male' &
           weight > 5) %>% 

# return data for tag and weight only
  select(tag, weight)
turtles_males_lg <- turtles.df %>% 
    # return data for males only with weight greater than 5
  filter(sex == 'male' &
           weight > 5) %>% 

# return data for tag and weight only
  select(tag, weight)
  # Group by ----------------------

turtles.df %>% 
  
  # group by sex
  group_by(sex) %>% 
  
  summarise(mean = mean(weight))


write_csv(turtles.df,
          'data/turtles_tidy.csv')


bear_data <- read_csv('data/bear_2008_2016.csv') %>% 
  
  # set column names to lowercase
  set_names(
    names(.) %>%  
      tolower()) %>% 
  
  # rename columns to include units
  rename(m_to_town = dist_to_town,
         m_to_forest = dist_to_forest)
  

head(bear_data) 

str(bear_data)

# change targetspp to factor dplyr
bear_data <- bear_data %>% 
  mutate(targetspp = as.factor(targetspp))

levels(bear_data$targetspp)

bear_sheep_data <- bear_data %>% 
  
  # return only rows for sheep
  filter(targetspp == 'ovine') %>% 
  
  # select specified columns
  select(damage:month, bear_abund:altitude) # most parsimonious way to do it but you could have also done


# select(damage, year, month, bear_abund, landcover_code, altitude)

# or 

# select(1:3, 7:9)

head(bear_sheep_data)

bear_sheep_data %>% 
  summarise(mean_alt = mean(altitude),
            sd_alt = sd(altitude),
            se_alt = sd_alt/sqrt(length(altitude))) # you could also look up the length for altitude in the console and type the number here directly but this is more flexible and readable


