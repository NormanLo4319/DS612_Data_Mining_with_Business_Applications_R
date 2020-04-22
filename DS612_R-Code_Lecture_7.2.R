#---------------------------------------#
#---------------Chapter 7---------------#
#--------Moving Beyond Linearity--------#
#---------------------------------------#

rm(list=ls(all=TRUE))

#### Splines ####
library(splines)
library(ISLR)

# The bs() function generates the entire matrix of basis functions for splines
# with the specified set of knots. By default the cubic splines are produced.
attach(Wage)

# We speified knots at ages 25, 40, and 60, which produces a spline with 6 basis functions
# The bs() function, by default, genearte a cubic splines model
fit.splines = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
summary(fit.splines)

# Create an age grid for prediction
agelims = range(age)
age.grid=seq(agelims[1],to=agelims[2])

# Predict wage based on the age grid
pred.Splines = predict(fit.splines, newdata=list(age=age.grid), se=TRUE)
names(pred.Splines)

# Plot the prediction and confidence intervals
plot(age,wage,col="gray")
se.bands = cbind(pred.Splines$fit+2*pred.Splines$se.fit, pred.Splines$fit-2*pred.Splines$se.fit)
lines(age.grid,pred.Splines$fit, lwd=2, col="blue") #lwd is linewidth
matlines(age.grid,se.bands, col="blue", lty=2)  #lty shows type of line lty = 2 is used for broken lines

# Note that the regular spline regression usually genearate a wider confident intervals on the two extreme regions.  
# A Natrual Spline regression can reduce the standard errors on thesee regions.

## We can change degrees of freedom and use Natural Spline with the ns() function
# In this example we are using df=4 to define 3 internal knots for the model.
# By default, ns() function will pick 25th, 50th, and 75th percetiles of age, 
# which are 33.8, 42, and 51 as the internal knots.
fit.splines2 = lm(wage~ns(age,df=4),data=Wage)
summary(fit.splines2)

# Predict wage based on the age grid
pred.Splines2 = predict(fit.splines2, newdata=list(age=age.grid), se=TRUE)

# Plot the prediction and confidence intervals
lines(age.grid,pred.Splines2$fit, col="red", lwd=2)
se.bands2 = cbind(pred.Splines2$fit+2*pred.Splines2$se.fit, pred.Splines2$fit-2*pred.Splines2$se.fit)
matlines(age.grid,se.bands2, col="red", lty=2)

# As suggested by the graph, the natural spline provides a more promising prediction with 
# narrower confident intervals on both ends of age range.


#### Smoothing Splines ####

# Fitting the smooth spline by using the smooth.spline() function 
# We use 16 degree of freedom to determines the value of lamda
fit.SS = smooth.spline(age, wage, df=16)
# We then select the smoothness level by cross-validation with cv=TRUE
fit.SS2 = smooth.spline(age, wage, cv=TRUE)
names(fit.SS2)

# The CV selected degree of freedom = 6.794596
fit.SS2$df

# Plot the two fitted model for comparison
plot(age,wage,col="gray")
lines(fit.SS, col="red", lwd=2)
lines(fit.SS2, col="blue", lwd=2)


#### Local Regression Lines #### loess()
# span = the neighborhood it should consider for analysis
fit.LRL = loess(wage~age, span=0.2, data=Wage)
Predict.LRL = predict(fit.LRL, newdata=data.frame(age=age.grid)) #we shall use data.frame instead of list
fit.LRL2 = loess(wage~age, span=0.5, data=Wage)
Predict.LRL2 = predict(fit.LRL2, newdata=data.frame(age=age.grid)) 

plot(age, wage, col="gray")
lines(age.grid,Predict.LRL, col="blue", lwd=2)
lines(age.grid,Predict.LRL2, col="red", lwd=2)


### GAMS ####
install.packages("gam")
library(gam)

# The convenience for fitting a GAM model is that we can mix with different non-linear models
# s() is smoothing spline in GAMs library
# ns() is natural spline in GAMs library
# lo() is local regression in GAMs libarary
gam1 = gam(wage~ns(year,4)+s(age,5)+education, data=Wage)
summary(gam1)

# Plot the prediction
plot(gam1, se=TRUE, col="blue")  # not an impressive representation"
par(mfrow=c(1,3)) # Show 3 outcomes 1 for each variable  year, age, education
plot.gam(gam1,se=TRUE, col="red")


### ANOVA IN GAM ###
gam.V1 = gam(wage~s(age,5)+education, data=Wage)
gam.V2 = gam(wage~year+s(age,5)+education, data=Wage)
gam.V3 = gam(wage~ns(year,4)+s(age,5)+education, data=Wage)

anova(gam.V1, gam.V2, gam.V3, test="F")
# There is a complelling evidence that the second model which uses a linear model for year
# is better than the other two 

# Make predictions with the best model
PredictGam = predict(gam.V2, newdata=Wage)
PredictGam


##### Adding Local regression fits by lo() models
gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.5)+education, data=Wage)
summary(gam.lo)

##### Finally that is the time for logistic regression in GAM #####
## You can use I() and don't forget to set family to binomial
gam.lr = gam(I(wage>250)~year+s(age,df=5)+education, family=binomial)
summary(gam.lr)
plot(gam.lr, se=TRUE, col="green")
