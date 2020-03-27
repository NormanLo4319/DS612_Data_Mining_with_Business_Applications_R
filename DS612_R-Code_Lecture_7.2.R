#---------------------------------------#
#---------------Chapter 7---------------#
#--------Moving Beyond Linearity--------#
#---------------------------------------#

#### Splines ####
library(splines)
library(ISLR)

#The bs() function generates the entire matrix of baseis functions for splines
#with the specified set of knots. By default the cubic splines are produced
attach(Wage)

fit.splines = lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
summary(fit.splines)

agelims = range(age)
age.grid=seq(agelims[1],to=agelims[2])

pred.Splines = predict(fit.splines, newdata=list(age=age.grid), se=TRUE)
names(pred.Splines)
plot(age,wage,col="gray")
se.bands = cbind(pred.Splines$fit+2*pred.Splines$se.fit,preds$fit-2*pred.Splines$se.fit)
lines(age.grid,pred.Splines$fit, lwd=2, col="blue") #lwd is linewidth
matlines(age.grid,se.bands, col="blue", lty=2)  #lty shows type of line lty = 2 is used for broken lines

## We can change degrees of freedom and use Natural Smoothing
fit.splines2 = lm(wage~ns(age,df=4),data=Wage)
pred.Splines2 = predict(fit.splines2, newdata=list(age=age.grid), se=TRUE)
lines(age.grid,pred.Splines2$fit, col="red", lwd=2)


#### Smoothing Splines ####
plot(age,wage,col="gray")
fit.SS = smooth.spline(age, wage, df=16)
fit.SS2 = smooth.spline(age, wage, cv=TRUE)
names(fit.SS2)
fit.SS2$df
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
#s() is smoothing spline in GAMs libarary
gam1 = gam(wage~ns(year,4)+s(age,5)+education, data=Wage)
plot(gam1, se=TRUE, col="blue")  #not an impressive representation"
par(mfrow=c(1,3)) #Show 3 outcomes 1 for each variable  year, age, education
plot.gam(gam1,se=TRUE, col="red")
summary(gam1)


### ANOVA IN GAM ###
gam.V1 = gam(wage~s(age,5)+education, data=Wage)
gam.V2 = gam(wage~year+s(age,5)+education, data=Wage)
gam.V3 = gam(wage~ns(year,4)+s(age,5)+education, data=Wage)

anova(gam.V1, gam.V2, gam.V3, test="F")
# There is a complelling evidence that the second model which uses a linear model for year
#is better than the other two 

PredictGam = predict(gam.V2, newdata=Wage)


##### Adding Local regression fits by lo() models
gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.5)+education, data=Wage)


##### Finally that is the time for logistic regression in GAM #####
## You can use I() and don't forget to set family to binomial
gam.lr = gam(I(wage>250)~year+s(age,df=5)+education, family=binomial)
plot(gam.lr, se=TRUE, col="green")