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
cv.error <- rep(0,10)
degree <- 1:10
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}

# Plot the results and compare different degree for cross validation
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

## Bootstrap
## Minimum risk investment - Section 5.2

## What is the standard error of alpha?
?Portfolio

alpha.fn=function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

?with