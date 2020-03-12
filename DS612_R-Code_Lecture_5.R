#---------------------------------------#
#---------------Chapter 5---------------#
#----------Resampling Methods-----------#
#---------------------------------------#

# For Cross Validation, we need to import ISLM and Bootstrap
require(ISLR)
require(boot)

# Plot the variables mpg and horsepower from Auto data set
plot(mpg~horsepower,data=Auto)


# Validation Set Approach:
# First create an index for the subset data from the original dataset.
# For instance, we can create a subset of 300 random sample for the dataset.
set.seed(1)
train <- sample(dim(Auto)[1], 300)

# Fit the model with the training set
fit1 <- lm(mpg~horsepower, data=Auto, subset=train)

# Calcuate the Mean Square Error for the training set (Train Error)
mean((Auto$mpg-predict(fit1, Auto))[train]^2)

# Calculate the Mean Square Error for the testing set (Test Error)
mean((Auto$mpg-predict(fit1, Auto))[-train]^2)

# As observed the training error is much smaller than the testing error, which is 
# in line with our prediction.

# We can continue to the do the same thing for quadratic and cubic regressions.
fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((Auto$mpg-predict(fit2, Auto))[train]^2)
mean((Auto$mpg-predict(fit2, Auto))[-train]^2)

fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((Auto$mpg-predict(fit3, Auto))[train]^2)
mean((Auto$mpg-predict(fit3, Auto))[-train]^2)


# Leave-One-Out Cross Validation (LOOCV):

# We are going to use the cv.glm() function for cross validation
?cv.glm

# We first fit the data into a simple linear model using the glm() function
glm.fit <- glm(mpg~horsepower, data=Auto)

# We can then use the cross validation function to run glm model for (n - 1) times
# pretty slow (doesnt use formula (5.2) on page 180)
cv.glm(Auto,glm.fit)$delta 

#Delta is the cross-validated prediction error. It gives two numbers. The first 
#Number is the raw Leave-one-Out or Lieu Cross-Validation Error, and the second one is
#the bias correction has to due to the fact that the data set that we train it on 
#is slightly smaller that the one that we actually would like to get the error for, which
#is the full data set of size n.

?lm.influence
##Lets write a simple function to use formula (5.2)
loocv = function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# Now we use the loocv() function to perform the cross validation
loocv(glm.fit)

# Using a for loop to check the the k-fold cross validation comparison
# Try to use 10 cross validation outcomes
cv.error <- rep(0,10)
# Degree of polynomial from 1 to 10
degree <- 1:10
for(d in degree){
  glm.fit <- glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d] <- loocv(glm.fit)
}

# Plot the cv error and compare different degrees for cross validation
plot(degree,cv.error,type="b")


# Now, let's try to use the 10-fold cross validation
# Try to use 5 cross validation outcomes with degree of polynomial from 1 to 10
cv.error10 <- rep(0,5)
for(d in degree){
  glm.fit <- glm(mpg~poly(horsepower, d), data=Auto)
  cv.error10[d] <- cv.glm(Auto,glm.fit, K=10)$delta[1]
}

# Plot the line that represents the degree and cv error
lines(degree, cv.error10, type="b", col="red")

# Bootstrap
# Minimum Risk Investment - Section 5.2

# We are going to use 'Portfolio' data set
?Portfolio

# What is the standard error of alpha?
# We are going to create an alpha function for the analysis
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

# Use the alpha.fn() function to calculate the standard error for alpha
alpha.fn(Portfolio,1:100)

# Set a random seed number
set.seed(1)

# Use the alpha.fn() function again on the 100 sample in the data set
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

# Use boot() function for repeated measures by 1000 times
boot.out <- boot(Portfolio,alpha.fn,R=1000)
boot.out

# Plotting the bootstrap output
plot(boot.out)


# Estimateing the Accuracy of a Linear Regression Model
# Create a boostrap function to pass in the data set and index 
boot.fn <- function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}

# Using the bootstrap function to estimate the coefficiencts by defining the subset of data.
# For example, we can define to use all 392 observation from the data.
boot.fn(Auto, 1:392)

# Compare it to the coefficienct with the regular lm() function without specifying subset.
coef(lm(mpg~horsepower, data=Auto))

# Now, let's try to set up a random seed and generate a random sample with replacement.
set.seed(1)
boot_s1 <- sample(392, 392, replace=TRUE)
boot.fn(Auto, boot_s1)

boot_s2 <- sample(392, 392, replace=TRUE)
boot.fn(Auto, boot_s2)
