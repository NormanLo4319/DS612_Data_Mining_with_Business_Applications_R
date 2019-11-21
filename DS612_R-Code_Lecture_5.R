require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto) #Fits a linear model
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)

#Delta is the cross-validated prediction error. It gives two numbers. The first 
#Number is the raw Leave-one-Our or Lieu Cross-Validation Error, and the seond one is
#the bias correction has to due to the fact that the data set that we train it on 
#on is slightly smaller that the one that we actually would like to get the error for, which
#is the full data set of size n.


##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

cv.error=rep(0,10)
degree=1:10
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
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