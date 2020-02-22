#---------------------------------------#
#---------------Chapter 3---------------#
#--------Linear Regression Model--------#
#---------------------------------------#

# Linear Regression
# We have learned some of the basic use of R.
# However, there are many individual contributors in the world who wrote thousands of functions and made it free for people to use.
# To avoid writing our own function for the specific study or analysis, researchers or scientists usually will use the existing packages written by those individual contributors.
# In this exercise, we are going to use use the package named "ISLR" and the libraries "MASS" and "ISLR" for our analysis.

# First of all, if you have not install the package "ISLR" on your machine yet, you will need to run the following command.
install.packages("ISLR")

# Once the package is installed, you can get access to the libraries within the package
library(MASS)
library(ISLR)

# Within the MASS library, there is a data set named "Boston".
# We will be using this data set for our exercise today.
# Boston is the source of Data
?Boston

# Before our analysis, we would like to visualize our data
# For instance, we can plot two columns in the data set, medv (median value of owner-occupied homes)
# and lstat (lower status of the population(%))
plot(medv~lstat,Boston)

# Simple Linear Regression Line
# If we believe that there is a linear relationship between medv and lstat, we can build a simiple regression model with lm()
?lm

# The specification of our model is medv = beta0 + beta1*(lstat) and we are going to save the model into fit1
fit1 <- lm(medv~lstat,data = Boston)

# Once the model is saved into a variable, we can then print the summary of it.
summary(fit1)

# We also plot the regression line and visualize the fitted line
# The abline function will take two values a = intercept of a straight line and b = slope of a stright line
abline(fit1,col="red")

# We can use the names() to check the attribute in the fitted model
names(fit1)

# We can check the confidence interval for our coefficients by using the confint()
confint(fit1)

# Setting the 99% confidence interval
x <- confint(fit1,level=0.99)
x

# If any question about the function, always make use of the help function
?confint
?predict

# Use the predict function to make prediction of medv
predict(fit1,data.frame(lstat = c(5,10,15)),interval = "confidence",level = .99)

# We can check if adding a new variable (age) to the model specification would help explaining medv
fit2 <- lm(medv~lstat+age,data= Boston)
summary(fit2)

# We can also add all the variables from the data set into our model by use "."
fit3 <- lm(medv~.,data = Boston)
summary(fit3)

# If we want to drop some variables from the data set, we can use the negative sign "-"
fit4 <- lm(medv~.-age-indus,data = Boston)
summary(fit4)

# Non-linear Transformations or Interation effects
# Let's add lstat*age interation effect
fit5 <- lm(medv~lstat*age,data = Boston)
# If you want to keep the interation effect only - you need to use lstat:age
summary(fit5)

# You can also add a squared term to the model
fit6 <- lm(medv~lstat+I(lstat^2),data = Boston)
summary(fit6)

# The reason we are using the I() is because we need the function to specify the formula (lstat^2)
# When we need to specify the mathematic operator in our model, we need to use I() to interpret within the model.
?I

# Creating a polynomial specification, we can use the function poly()
fit7 <- lm(medv~poly(lstat,5),data = Boston) 
summary(fit7)

# How to deal with Qualitative Predictors - Dummy Variables
# In this exercise, we are using the data set "Carseats" from the ISLR library
?Carseats

# Use the names() function to print the column names in the data frame
names(Carseats)

# Use the summary() function to print the discriptive static summary
summary(Carseats)

# We can also use the lm() function to run the linear model with catagorical varaibles
fit8 <- lm(Sales~.+Income:Advertising+Age:Price,data = Carseats)
summary(fit1)

