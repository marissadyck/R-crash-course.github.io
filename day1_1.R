
#  R Crash Course, Day 1 Module 1   
#      Casa Tisaru Lepsa, Romania             

#  Getting started with R: the basics 

# Using the help file ----------------------

?seq
  
# example of order of operations using seq()
seq(from = 1,
    to = 10,
    by = 1)

# the below is read the same as above
seq(1, 10, 1)

# or
seq(to = 10,
    from = 1,
    by = 1)

# but not the same as
# seq(10, 1, 1) # try this, what happens?

# Functions ------------------- 

# sum
sum(1, 2, 3, 10)    # returns: 15

# combine
c(1, 2, 3, 10)   # combine four numbers into a single data object (a vector!)

# floor
floor(67.8)  # removes the decimal component of a number

# round
round(67.8)  # rounds a number

round    # oops, forgot the parentheses!


?round        




#  Create R Objects ------------------------

  # Scalars----------------------

scalar1 <- 'this is a scalar'

scalar2 <- 104

  # Vectors ----------------------

# create a vector of numbers 1 to 4
vector1 <- c(1, 2, 3, 4)  

# print vector1
vector1

# create a vector of numbers 1 to 4 using ':'
vector2 <- 1:4  

# print vector2
vector2

# create a vector of numbers 1 to 4 using seq()
vector3 <- seq(1, 4, by = 1)

# print vector 3
vector3

# create a vector of numbers 1 to 4 using 'c'
vector4 <- c(1, 4)

# print vector 4
vector4

# make a vector of 1, 2, and 3
d1 <- 1:3  

d1
          
# add 3 to all elements of the vector d1
d2 <- d1 + 3  

d2

# elemntwise addition
d3 <- d1 + d2

d3

# check the number of elements in a vector
length(d1)           

# sum all the elements in a vector
sum(d3)              

# extract the second element in a vector
d2[2]                 


  # Matrices ----------------------

# create a matrix by binding vectors, with vector d1 as column 1 and d2 as column 2
mymat <- cbind(d1, d2)

mymat

# confirm that the new object "mymat" is a matrix using the 'class()' function
class(mymat)   

 # create matrix another way (matrix constructor)
mymat <- matrix(
  c(1,2,3,4,5,6),
  nrow = 3,
  ncol = 2)  

mymat


# math with matrices

mymat + 2

sum(mymat)

# extract matrix elements

# extract the element in the 3rd row, 2nd column
mymat[3, 2]  

mymat[, 1]     # extract the entire first column

# Syntax for using []
# X[a,b]       access row a, column b element of matrix/data frame X
# X[,b]        access all rows of column b of matrix/data frame X
# X[a,]        access row a of matrix/data frame X


  # lists ----------------------

# create an empty list
mylist <- list()

# add elements to the empty list
mylist[[1]] <- c(1, 2, 3)     # note the double brackets- this is one way to reference list elements. 

mylist[[2]] <- c('piatra','craiului')

mylist[[3]] <- matrix(1:6,
                      nrow = 2)

# print mylist
mylist

# do stuff with lists

# extract the second list element
mylist[[2]]    

# extract the first row, second column from the matrix that is embedded as the third element in this list !
mylist[[3]][1,2]      


  # Data frames ----------------------

 # create a data frame with two columns. Each column is a vector of length 3
df1 <- data.frame(col1 = c(1,2,3),
                  col2 = c("A","A","B")) 

df1

# extract the first element in the second column
df1[1, 2]    

# extract the second column by name (alternatively, df1[["col2"]])
df1$col2    


# Making up data ----------------------------------

# Generating vector sequences  

# sequential vector from 1 to 10
1:10            

# sequence of length 5 between 0 and 1 
seq(0, 1, length = 5)         


# Repeating vector sequences 

# repeat 0 three times
rep(0, 
    times = 3) 

# repeat the vector 1:3 twice
rep(1:3, 
    times = 2) 

# repeat each element of 1:3 two times
rep(1:3, 
    each = 2)              


# Random numbers 

# 10 samples from std. normal
rnorm(10)                    

# 10 samples from Normal(-2,4^2)
rnorm(10, 
      mean= -2, 
      sd = 4)      

# 5 samples from Binom(3,0.5)
rbinom(5,
       size = 3,
       prob = .5)  

# 5 samples from Binom(3,0.1)
rbinom(5, 3, .1)            

# 10 standard uniform random numbers
runif(10)  

  # 10 uniform random numbers from [-1,1]
runif(10,
      min = -1,
      max = 1)      


# Make up an entire fake data frame!

my.data <- tibble(
  Obs.Id = 1:100,
  Treatment = rep(c("A","B","C","D","E"),
                  each = 20),
  Block = rep(1:20,
              times = 5),
  Germination = rpois(100,
                      lambda = rep(c(1,5,4,7,1),
                                   each = 20)),   # random poisson variable
  AvgHeight = rnorm(100,
                    mean = rep(c(10,30,31,25,35,7),
                               each = 20))
)

my.data

# Use the "summary()" function to summarize each column in the data frame.
summary(my.data)   

# extract rows 21 to 30 and store as a new data frame
mydf = my.data[21:30, ]  

mydf

# access a column of the data frame by name
mydf$Treatment 


# Useful data exploration/checking tools in R --------------------

# Obtain length (# elements) of vector d2
length(d2)  

# Obtain dimensions of matrix or array
dim(mymat)

# summarize columns in a data frame. 
summary(my.data)

# look at the "internals" of an object (useful for making sense of complex objects!)
str(my.data) 

# get names of variables in a data frame (or names of elements in a named vector)
names(my.data) 

# get number of rows/observations in a data frame
nrow(my.data) 

# get number of columns/variables in a data frame
ncol(my.data)   
     


# answer 1 (the most parsimonious)
myvec <- 1:10

# answer 2
myvec <- c(1, 10)

# answer 3 (time consuming)
myvec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# answer 4 (unecessarily complicated for this exercise but works)
myvec <- seq(1, 10, by = 1)

rbind(
c(1,4),
c(2,5),
c(3,6)
)


names(mydf)

mydf$AvgHeight

mydf[, 5]


as.matrix(mydf[, 3:5])


#create an empty list
mylist <- list()

# add a vector of 1 to 3 to the list
mylist[[1]] <- 1:3

# add a matrix to the list
mylist[[2]] <- matrix(1:6,
                      nrow = 3,
                      ncol = 2)

# add a data frame to the list
mylist[[3]] <- data.frame(x = c(1, 2,3 ),
                          y = c(TRUE, FALSE, TRUE),
                          z = c("a", "a", "b"))

# print the list
mylist


# call mylist then reference the position of the data frame in the list [[3]], then the column in the data frame [[1]], and finally the observations within that columns c(2, 3)
mylist[[3]][[1]][c(2, 3)]

