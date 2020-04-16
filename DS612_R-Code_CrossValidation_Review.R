# Cross-Validation Review

# Objective: Cross-Validation is mainly used validate the testing MSE for model selection.

# Validation Set Approach
require(ISLR)
require(boot)

attach(Auto)
?Auto
pairs(Auto)

# Splitting data to 300 training observations and 92 testing observations
set.seed(1)
train <- sample(dim(Auto)[1], 300)
train
test <- -train
trainData <- Auto[train,]
testData  <- Auto[test,]
trainOutcome <- Auto$mpg[train]
testOutcome <- Auto$mpg[test]

fit1 <- lm(mpg~horsepower, data=Auto, subset=train)
summary(fit1)

fit2 <- lm(mpg~horsepower+weight, data=trainData)
summary(fit2)

fit3 <- lm(mpg~horsepower+weight+displacement, data=Auto[train,])
summary(fit3)

# Use Validation Set Approach to evaluate the predictive performance from each model

# Model 1 MSEs
# Training MSE
fit1_trainMSE <- mean((Auto$mpg-predict(fit1, Auto))[train]^2)
fit1_trainMSE
# Testing MSE
fit1_testMSE <- mean((Auto$mpg-predict(fit1, Auto))[-train]^2)
fit1_testMSE

# Model 2 MSEs
# Training MSE
fit2_trainPred <- predict(fit2, newdata=trainData)
fit2_trainMSE <- mean((fit2_trainPred - trainOutcome )^2)
fit2_trainMSE
# Testing MSE
fit2_testPred <- predict(fit2, newdata=testData)
fit2_testMSE <- mean((fit2_testPred - testOutcome )^2)
fit2_testMSE

# Model 3 MSEs
# Training MSE
fit3_trainPred <- predict(fit3, Auto)[train]
fit3_trainMSE <- mean((fit3_trainPred - trainOutcome)^2)
fit3_trainMSE
# Test MSE
fit3_testPred <- predict(fit3, Auto)[-train]
fit3_testMSE <- mean((fit3_testPred - testOutcome)^2)
fit3_testMSE

# Create data frame to show result
models <- c("fit1", "fit2", "fit3")
x <- c(1, 2, 3)
trainMSEs <- c(fit1_trainMSE, fit2_trainMSE, fit3_trainMSE)
testMSEs <- c(fit1_testMSE, fit2_testMSE, fit3_testMSE)
results <- data.frame(models, trainMSEs, testMSEs)
results

# Plot the training and testing MSEs 
plot(x, testMSEs, type="b", col="blue")
lines(x, trainMSEs, type="b", col="red")
legend(2.6, 24, legend=c("testMSE", "trainMSE"),
       col=c("blue", "red"),
       lty=1)

# Using a bar chart to demonstrate the training and test MSEs 
# barplot(c(trainMSEs, testMSEs),
#        legend=models, col=c("green", "blue", "red"))

# Conclusion: More complex model is not going to improve predictive performance.
# Given the situation, fit2 model is confirmed by Validation Set Approach, which
# is the best model for prediction out of the three.


# Cross-Validation Approach

# LOOCV Approach
fit1 <- glm(mpg~horsepower, data=Auto)
summary(fit1)

fit2 <- glm(mpg~horsepower+weight, data=Auto)
summary(fit2)

fit3 <- glm(mpg~horsepower+weight+displacement, data=Auto)
summary(fit3)

# Calculate the LOOCV MSE for each model
# Model 1 LOOCV MSE
fit1_loocv <- cv.glm(Auto, fit1)$delta[1]
fit1_loocv

# Model 2 LOOCV MSE
loocv = function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
fit2_loocv <- loocv(fit2)
fit2_loocv

# Model 3 LOOCV MSE
fit3_loocv <- loocv(fit3)
fit3_loocv

loocv_MSEs <- c(fit1_loocv, fit2_loocv, fit3_loocv)
loocv_results <- data.frame(models, loocv_MSEs)
loocv_results

# Adding the LOOCV MSEs to the previous graph
lines(x, loocv_MSEs, type="b", col="green")
legend(2.5, 24, legend=c("testMSE", "trainMSE", "LOOCVMSE"),
       col=c("blue", "red", "green"),
       lty=1)


# 5-Folds & 10-Folds Cross-Validation
# 5-Fold Cross-Validation
# Model 1 MSE
fit1_5fold <- cv.glm(Auto, fit1, K=5)$delta[1]
fit1_5fold

# Model 1 MSE
fit2_5fold <- cv.glm(Auto, fit2, K=5)$delta[1]
fit2_5fold

# Model 1 MSE
fit3_5fold <- cv.glm(Auto, fit3, K=5)$delta[1]
fit3_5fold


# 10-Fold Cross-Validation
# Model 1 MSE
fit1_10fold <- cv.glm(Auto, fit1, K=5)$delta[1]
fit1_10fold

# Model 1 MSE
fit2_10fold <- cv.glm(Auto, fit2, K=5)$delta[1]
fit2_10fold

# Model 1 MSE
fit3_10fold <- cv.glm(Auto, fit3, K=5)$delta[1]
fit3_10fold

fold5_MSEs <- c(fit1_5fold, fit2_5fold, fit3_5fold)
fold10_MSEs <- c(fit1_10fold, fit2_10fold, fit3_10fold)

# Store results into a data frame
fold_results <- data.frame(models, fold5_MSEs, fold10_MSEs)
fold_results

# Plot the results to the same graph
lines(x, fold5_MSEs, type="b", col="purple")
lines(x, fold10_MSEs, type="b", col="brown")
legend(2.5, 24, legend=c("testMSE", "trainMSE", "LOOCVMSE", "5-Fold MSE", "10-Fold MSE"),
       col=c("red", "blue", "green", "purple", 'brown'),
       lty=1)

# Conclusion: We can now confirm by four validation approaches (validation set, 
# LOOCV, 5-fold CV, and 10-fold CV) that model 2 is the best model for prediction.