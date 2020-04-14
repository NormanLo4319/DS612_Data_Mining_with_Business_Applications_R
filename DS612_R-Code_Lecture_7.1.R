#---------------------------------------#
#---------------Chapter 7---------------#
#--------Moving Beyond Linearity--------#
#---------------------------------------#

# Loading the required libraries for the lab
require(ISLR)
attach(Wage)

?Wage

# Polynomial Regression and Step Functions

# In this example, we are tyring to predict wages using age from the dataset
# First fit the model using the basic command for polynomial regression
fit.Poly.Basic <- lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fit.Poly.Basic)

# We can also use the poly() function to fit the same model
fit.Poly <- lm(wage~poly(age,4, raw=TRUE),data=Wage)
summary(fit.Poly)

# Note that if we do not define raw=TRUE, the model will still have the same fitted values
# However, the coefficients for each term cannot be interpreted as usual
# In the orthogonal polynomial form, only the p-value can be interpreted
fit.Poly.Orth <- lm(wage~poly(age,4), data=Wage)
summary(fit.Poly.Orth)

# Let's create a grid of values for age for prediction
agelims <- range(age)
agelims
age.grid <- seq(agelims[1], to=agelims[2])
age.grid
preds <- predict(fit.Poly,newdata=list(age=age.grid), se=TRUE)
names(preds)
preds
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)


# Plot the data dn add the fit from the degree-4 polynomial
plot(age, wage, col="darkgray")
lines(age.grid, preds$fit, lwd=2, col="blue") #lwd is linewidth
matlines(age.grid, se.bands, col="red", lty=2)  #lty shows type of line lty = 2 is used for broken lines


#### Using Anova in Nested sequence of Models ####
fita <- lm(wage~education, data=Wage)
fitb <- lm(wage~education + age, data=Wage)
fitc <- lm(wage~education + poly(age,2), data=Wage)
fitd <- lm(wage~education + poly(age,3), data=Wage)
fite <- lm(wage~education + poly(age,4), data=Wage)

# The p-value indicates the model with the prior model
# The p-vlaue is significant means the prior model is not sufficient
# or the current model is better then the prior model
anova(fita,fitb,fitc,fitd,fite)


#### Logistic Regression ####
# Fitting the logistic regression model on binary response, wage,
# using wrapper I() to create binary response, over 250 (True) and below or equal 250 (False)
# on age with polynomial degree 3
fit.Logistics <- glm(I(wage>250)~poly(age,3), data=Wage, family=binomial)
summary(fit.Logistics)

# Predict the response by the age grid created earlier
preds.Logistics <- predict(fit.Logistics, newdata=list(age=age.grid), se=TRUE)
names(preds.Logistics)

# Create the confidence intervals for the prediction
se.bands <- preds.Logistics$fit+cbind(fit=0, lower=-2*preds.Logistics$se.fit, upper=2*preds.Logistics$se.fit)
se.bands[1:5,]

# Convert logistics to probabilities
prob.bands <- exp(se.bands)/(1+exp(se.bands))
prob.bands[1:5,]

# Plot the predictions with confidence intervals
matplot(age.grid, prob.bands, col="Green", lwd=c(2,1,1), lty=c(1,2,2), type="l")

range(wage)
plot(wage)

#### Fitting step functions ####
# In order to fit a step function, we need to use the cut() function
?cut
table(cut(age,4))
fit.Step <- lm(wage~cut(age,4), data=Wage)
summary(fit.Step)

table(cut(age, c(17.9, 35, 50, 65, 80.1)))
