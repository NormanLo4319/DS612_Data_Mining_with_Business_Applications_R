# Practice with food_env data set
Credit <- read.csv("Data/Textbook_Data/Credit.csv", header = TRUE, na.string="?")
names(Credit)
attach(Credit)

# Pairs Plot
pairs(Credit)

# Plot the data between Balance and Age
plot(Age, Balance)

# The plot suggests the two variables are not highly correlated.
# However, we do not want to check every plots to see the pattern of the data.

?cor

# Create a for loop to get all the correlation between Balance and other variables.
corr <- c()

for (i in 2:12){
  result <- tryCatch(cor(Balance, Credit[i]), error=function(e)
    paste("NA"))
  print(result)
  corr <- append(corr, result)
}

# Results: Limit and Rating are highly correlated with Balance.
corr

# Note that the cor() function cannot return result for categorical variables, such as Gender, Student, etc.

# Let's try the forward selection method with the highest correlated variable Rating
fit1 <- lm(Balance~Rating, data=Credit)
summary(fit1)

# Let's try to add the second highest correlated variable Limit
fit2 <- lm(Balance~Rating+Limit, data=Credit)
summary(fit2)

# As we observed from the result, adding Limit will significantly increase the p-value of Rating and R^2 did not improve.
# This is a classic example of multicollinearity.  The easy want to solve it is to drop the one that is not statistically significant "Limit"
# There is another alternative way to deal with this problem, such as creating a new variable with Rating and Limit combined.

# Now, let's try to add the third highest correclated variable Income
fit3 <- lm(Balance~Rating+Income, data=Credit)
summary(fit3)

# Adding Income to our model improve the R^2 significantly and all coefficients are highly significant.

# Now, let's try adding Cards to the model
fit4 <- lm(Balance~Rating+Income+Cards, data=Credit)
summary(fit4)

# Cards is not statistically significant and the other two variables have extremely low correlation with Balance, so we can stop here.

# What about the qualitative predictors?

# Let's try adding gender to the model.
fit5 <- lm(Balance~Rating+Income+Gender, data=Credit)
summary(fit5)

# Not statistically significant

# Let's try adding Student to the model.
fit6 <- lm(Balance~Rating+Income+Student, data=Credit)
summary(fit6)

# Impressive result, we have both the R^2 improved significantly and Student coefficient is statistically significant.
# However, the R^2 also propose that is a overfitting problem for this model specification.

# Let's try Married to the model
fit7 <- lm(Balance~Rating+Income+Student+Married, data=Credit)
summary(fit7)

# Not statistically significant

# Let's try adding Ethnicity to the model.
fit8 <- lm(Balance~Rating+Income+Student+Ethnicity, data=Credit)
summary(fit8)

# Final Result: The variables we are going to use will be Rating, Income, and Student

fit6 <- lm(Balance~Rating+Income+Student, data=Credit)

# Check again the data between Balance and Rating
plot(Rating, Balance)

# Plotting the regression line
abline(fit6, lwd=3, col="blue")

# plot(Income, Balance)
# plot(Student, Balance)

# Divides the plotting region into a 2x2 grid of panels.
par(mfrow=c(2,2))

# Plot the residual fitted plots
plot(fit6)

# We can also use the residuals() function to compute the residuals from the fitted model.
plot(predict(fit6), residuals(fit6))
plot(predict(fit6), rstudent(fit6))

# We can also compute the Leverage statistics for any number of predictors using the hatvalues() function
plot(hatvalues(fit6))

# We can also find out the largest leverage statistic observation in the model.
which.max(hatvalues(fit6))

# One of the common test for multicollinear for multivariable linear regression is VIF
library(car)
vif(fit6)

# The VIF test will test for multicollinearity for each varible in the model.
# Consider the test value is over 10 is a sign of significant multicollinearity issue.  
# In our model, all the variables have a score close to 1 or moderate level, so multicollinearity is not the major concern here.

# At this point, we can try to make a prediction of Balance based on our fitted model.
predict(fit6, interval="confidence", level=0.99)

