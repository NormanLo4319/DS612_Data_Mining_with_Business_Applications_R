#--------------------------------------#
#----------Introductory to R:----------#
#---------------(Part I)---------------#
#--------------------------------------#

# Find out where is your working directory 'wd'
getwd()

# Setting the working directory for your project or work
setwd("Your Directory Path!")

# Updating the current libraries or packages in R environment
update.packages()

# Create a new R script file from the current working directory
file.edit('Practice.R')

# Clear all the environment variables
rm(list = ls())

# Basic knowledge for Vectors and Matrices
# Using function c() to create a vector of elements or a list and store in a variable x
x <- c(2,7,5)

# There is a sequence function seq(). 
# If you want to find out what it is and how to use it, you can query by using the '?'
?seq

# Create a sequence from 4 to 10 and incremented by 1, then store it into a variable y
y <- seq(from =4 , to = 10, by = 1)
y

# Create a sequence starting at 4 and incremented by 5 with 3 steps, then store it into a variable y
y <- seq(from =4 , length = 3, by=5)
y

# Note that the second time we store the sequence into variable y, it actually replaced the previous sequence.

# Basic Mathematical Operation in R

# Addition or subtraction
x+y
x-y

# Multiplication or Division
x*y
x/y

# Polynomial or Power
x^y

# Basic Data Index
# Extracting data from a vector or list by calling the data index
# Calling the second element in vector x
x[2]

# Calling the second to the third elements in vector x
x[2:3]

# Calling all the elements in vector x, but dropping the second element
x[-2]

# Calling all the elements in vector x, but dropping the first two elements
x[-c(1:2)]

# Basic Matrices
# Creating a 3 by 4 matrix with sequence from 1 to 12
# Keep in mind, the demansion of matrix in R is always defined by number of row (i) and column (j)
z <- matrix(seq(1:12),3,4)
z

# Calling the element at i=2 and j=2 position
z[2,2]

# Calling all the elements, but dropping the first row
z[-1,]

# Calling all the elements, but dropping the second column
z[,-2]

# Calling all the elements, but dropping the second and third columns
z[,-c(2:3)]

# Check the size of the matrix z
dim(z)

# Check the lenght of matrix z
length(z)

# List all the variables in the global environment in this project
ls()

# remove variable (vector) x from the global environment
rm(x)

# Random Numbers
# R has basic function to create random numbers
# Using runif() to creat a vector x with 50 random numbers (default from 0 - 1)
x <- runif(50)
x

# Using rnorm() to create a vector z with 50 random z-scores (standardized scores in normal distribtion)
z <- rnorm(50)
z

# You can check the rnorm function by using the help function '?'
?rnorm

# Using rnorm() to create a vector t with 50 random t-scores (standardized scores with mean=5 and std=10)
t <- rnorm(50,mean = 5, sd = 10)
t

# Plotting the data point in R can use the simple plot function plot()
plot(x,z)

# You can also check the use of plot() by help function
?plot

# You can also give the graph labels on x and y axes
plot(x, z, xlab = "X-Axis", ylab = "Y-Axis")

# Using the histogram function hist() to display the distribution of z scores
hist(z)

# Using the histogram function hist() to display the distribution of t scores
hist(t)

# Creating a vector NN with 1,000,000 randomized standardized scores
NN <- rnorm(1000000)

# Plot the large sample size randomized standardized scores
hist(NN)
# It is worth to note that as sample size increase, the shape of the distribution tend to form a normal distribution

# We can also print the mean score, standard deviation, and variance of vector t 
mean(t)
sd(t)
var(t)


# Reading Data from CSV file
# Note that when loading the data set into R, you have to realize the current working directory
# The na.string parameter is for R to replace the value specified "?" with "NA"
AutoData <- read.csv("Data/Textbook_Data/Auto.csv",header = TRUE, na.string="?")

# If there is NA elements in the table, then drop the entire row from the table
AutoData <- na.omit(AutoData)
# As you can see the total observation dropped from 397 to 392

# R has a summary function that helps getting the descriptive statistic of the data frame
summary(AutoData)

# We can also use the plot function to plot any two columns (variables) from the data frame
# Note that we are use the "$" to specify the columne name in the data frame
plot(AutoData$mpg,AutoData$weight)

# Instead of calling the columns name with "$", we can also attache the data frame on the search path
attach(AutoData)

# After the data frame is attached, we can use the columne names directly without specifying with the "$"
plot(mpg,weight)
plot(origin,mpg)

# We can also call out a specific column from the data frame and save it as "factor" (catagories)
# The purpose of doing this is because we can treat the column as catagory instead of numeric.
origin <- as.factor(origin)

# As you can see, the plot is not defined as box plot because "origin" is not treated as catagorical data 
plot(origin,mpg)

# R has a pairs(), which returns a plot matrix consisting of scatter plots for each variable-combination of a data frame.
pairs(AutoData)

# We can also specify the columns in the data frame for the plot matrix
pairs(~mpg+cylinders+acceleration+weight)
