#---------------------------------------#
#---------------Chapter 5---------------#
#----------Resampling Methods-----------#
#---------------------------------------#

# For Cross Validation, we need to import ISLM and Bootstrap
require(ISLR)
require(boot)

# We are going to use the cv.glm() function for cross validation
?cv.glm

# Plot the variables mpg and horsepower from Auto data set
plot(mpg~horsepower,data=Auto)

# Leave-One-Out Cross Validation (LOOCV)
# We first fit the data into a simple linear model using the glm() function
glm.fit <- glm(mpg~horsepower, data=Auto)

# We can then use the cross validation function to run glm model for (n - 1) times
# pretty slow (doesnt use formula (5.2) on page 180)
cv.glm(Auto,glm.fit)$delta 

#Delta is the cross-validated prediction error. It gives two numbers. The first 
#Number is the raw Leave-one-Our or Lieu Cross-Validation Error, and the seond one is
#the bias correction has to due to the fact that the data set that we train it on 
#on is slightly smaller that the one that we actually would like to get the error for, which
#is the full data set of size n.


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

?with