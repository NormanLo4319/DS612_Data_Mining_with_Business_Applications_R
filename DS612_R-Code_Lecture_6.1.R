#---------------------------------------#
#---------------Chapter 8---------------#
#-----------Tree-Based Methods----------#
#---------------------------------------#

# In this section, we are going to use the tree-based methods for regresssion and classification.
# We first need to install the 'randomForest' and 'ISLR' packages
install.packages("randomForest")
install.packages("ISLR")
install.packages("tree")

# Import the libraries for analysis
library(randomForest)
library(ISLR)
library(tree)
library(MASS)

# We are going to use the Boston data set from MASS library
?Boston

# Splitting data into training and testing sets (50/50)
set.seed(1)
NumberofObservations = dim(Boston)[1]
SplitofTrainTest = 0.5 #let's split the data 50/50
train = sample(1:NumberofObservations,NumberofObservations*SplitofTrainTest)
test = -train
trainingData = Boston[train,]
testingData  = Boston[test,]
Testing_outcome = Boston$medv[test]
?randomForest

#####################
###### Bagging ######
#####################

# Bagging is simply a special case of random forest with m = p (All predictors are used to grow trees)
bag.boston = randomForest(medv~., data=trainingData, mtry=13, importance=TRUE)
# mtry is the nymber of variables randomly sampled as candidates at each slpit. 
# The defult is sqrt(p) in classification and p/3 in regression
# The defult number of trees is 500
bag.boston
names(bag.boston)
summary(bag.boston)

# Using the importance() function to check the importance of each variable
importance(bag.boston)
# Two measures are used to evaluate the importance of a variable,
# First, we can check the mean decrease of accuracy in prediction on the out of bag samples 
# when the variable is excluded from the model. (%IncMSE)
# Secondly, checking the total decrease in node impurity that result from splits over that variable
# Note: the node impurity is measured by the training RSS and for classsification tree by the deviance

# We can plot these importance measures with the varImpPlot() function
varImpPlot(bag.boston)

# Make prediction with the trained model by passing in the testing dataset
predict.bag = predict(bag.boston, newdata=testingData)

# Plot the prediction and testing outcome
plot(predict.bag,Testing_outcome)
abline(0,1)

# Calculate the MSE for the bagging model
MSE.bag = mean((predict.bag - Testing_outcome )^2)
MSE.bag

# What if we choose less than 500 trees?

bag.boston = randomForest(medv~., data=trainingData, mtry=13, importance=TRUE, ntree=25)
predict.bag = predict(bag.boston, newdata=testingData)
MSE.bag.25Trees = mean((predict.bag - Testing_outcome )^2)
MSE.bag.25Trees #Our error increased


#######################
#### random Forest ####
#######################

# In random Forest we only need to change the mtry so that we get the minimum MSE, 
# so, let's try different size of ntry - from 1 to 13

# Using a for loop to run different size of mtry (number of predictors to be consider for each split of the tree)
MSE.Rf=rep(0,13)
for(d in 1:13){
  rf.boston = randomForest(medv~., data=trainingData, mtry=d, importance=TRUE)
  predict.rf = predict(rf.boston,newdata = testingData)
  MSE.Rf[d] = mean((predict.rf- Testing_outcome )^2)
}
MTRY = c(1:13)

# Plot the MSE for each size of ntry
plot(MTRY,MSE.Rf,type="b",col="red")
min(MSE.Rf)

# mtry=4 created the minimum error - if you repeat this over and over you may get another miminizers such as 4 or 5
rf.boston = randomForest(medv~., data=trainingData, mtry=4, importance=TRUE)

# Getting the prediction from the best model
predict.rf = predict(rf.boston, newdata=testingData)

# Calculate the MSE from the best model
MSE.Rf = mean((predict.rf- Testing_outcome )^2)
MSE.Rf

# Check the importance measures
importance(rf.boston)

# Plot the importance measures
varImpPlot(rf.boston)

########################
######  Boosting #######
########################

install.packages("gbm")
library(gbm)
set.seed(1)
?gbm

# If you are running regression problems then use distribution = "gaussian". If you are working on 
# binary classfiication problems, then use distribution = "bernoulli"
# The default value of Lambda is 0.001
boost.boston = gbm(medv~., data=trainingData, distribution="gaussian", n.trees=5000, interaction.depth=4)
boost.boston
summary(boost.boston)
summary(Boston$medv)

# Parial dependence plots for rm and lstat
# These plots illustrate the marginal effect of the selected variables on the response (medv) after 
# integerating out the other variables. as we expect medv is increasing with rm and decreasing with lstat
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

# Get the prediction from the boosting model
predict.boost = predict(boost.boston, newdata=testingData, n.trees=5000)

# Calculate the MSE of the boosting model
mean((predict.boost-Testing_outcome )^2)
?gbm

# Let's Change Lambda
Lambda = c(.00001,0.0001,0.001,.01,0.1,.15,.2)
Counter = 1
MSE.Boost = rep(0,7)

# Using a for loop to check different values for Lamda
for(d in Lambda){
  boost.boston = gbm(medv~., data=trainingData, distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=d)
  predict.boost = predict(boost.boston, newdata=testingData, n.trees=5000)
  MSE.Boost[Counter] = mean((predict.boost- Testing_outcome )^2)
  Counter = Counter + 1
}

# The miminum happened at Lambda = 0.01
plot(Lambda,MSE.Boost,type="b",col="red") 

# 17.79736 less than random forest
min(MSE.Boost) 

# Now let's fix Lambda and change size of the tree
TreeSize = c(50,100,200,400,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
Counter = 1
MSE.Boost.Tree = rep(0,15)

# Create a for loop to check different tree size with the best Lamda selected (shrinkage=0.01)
for(d in TreeSize){
  boost.boston = gbm(medv~., data=trainingData, distribution="gaussian", n.trees=d, interaction.depth=4, shrinkage=0.01)
  predict.boost = predict(boost.boston, newdata=testingData, n.trees=d)
  MSE.Boost.Tree[Counter] = mean((predict.boost- Testing_outcome )^2)
  Counter = Counter + 1
}

# The tree size reaches miminum at 4000
plot(TreeSize,MSE.Boost.Tree,type="b",col="red")

# it seems like 4000 was a very good choice
min(MSE.Boost.Tree) 

MSE.Boost.Tree

#Another parameter you can play with is interation.depth


