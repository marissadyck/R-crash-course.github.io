
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Data Visualization
# Module 1: Data Visualization in Base R

library(tidyverse)

  # Data ----------------------

# look at description of built in dataset
?trees      

# Show the structure of the trees dataframe
str(trees)   

 # Show the first few observations of the trees dataframe
head(trees)  


  # Basic plotting ----------------------

plot(x = trees$Girth, y = trees$Volume)    # use R's built-in "trees" dataset: ?trees

# or

plot(trees$Girth, trees$Volume) 

# line graph
plot(trees$Girth, trees$Volume,
     type = "l")   # lines
# ?base::plot

  # Viewing multiple plots ----------------------

?par

par()   # view the default graphical parameters (can be kind of overwhelming!)

# change plotting window

# specify graphical parameters
par(mfrow=c(2,2)) 

# create plots to fill plotting window
plot(trees$Girth, 
     trees$Volume)             # point

plot(trees$Girth, 
     trees$Volume, 
     type="l")   # lines

plot(trees$Girth, 
     trees$Volume,
     type="b")   # both

plot(trees$Girth, 
     trees$Volume, 
     type="o")   # both with connected lines

# layout

# Use "layout" to define a 2 row x 2 column matrix with elements 1, 2, 3, and 4.
# This divides the image into four sections and then fills these with the plot function
layout(matrix(1:4, nrow=2, ncol=2))

# create plots to fill plotting window
plot(trees$Girth, 
     trees$Volume)             # point

plot(trees$Girth, 
     trees$Volume, 
     type="l")   # lines

plot(trees$Girth, 
     trees$Volume, 
     type="b")   # both

plot(trees$Girth, 
     trees$Volume, 
     type="o")   # both with connected lines


plot(trees$Girth, 
     trees$Volume)             ## The plot is still in 4 parts

graphics.off()                                  ## now the plot is reset!



  # Plot symbols ----------------------

# Use layout to define a 3 row x 1 column matrix with elements 1, 2, and 3.
# This divides the image into three sections and then fills these with the plot function
layout(matrix(1:3, 
              nrow = 3, 
              ncol = 1))

# pch: 'plotting character' changes the type of point that is used (default is an open circle)!
plot(trees$Girth, 
     trees$Volume, 
     pch = 19)     # filled point

plot(trees$Girth, 
     trees$Volume, 
     pch = 2)      # open triangle

plot(trees$Girth, 
     trees$Volume, 
     pch = 11)     # star


  # Histograms ----------------------

layout(matrix(1:2, 1, 2))

## y-axis is in counts by default (total observations in each "bin")
hist(iris$Sepal.Length, 
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length")

## change y-axis to proportions of the entire dataset using freq=FALSE
hist(iris$Sepal.Length, 
     freq = FALSE, 
     main = "Histogram of Sepal Length", 
     xlab = "Sepal Length")


  # Plotting matrices ----------------------

plot(iris)


# quick invesrtigation of relationship between variables 

pairs(iris)

# create correlation matrix for numeric variables in the iris data
chart.Correlation(iris[,1:4], 
                  histogram = TRUE, 
                  method = "pearson")
