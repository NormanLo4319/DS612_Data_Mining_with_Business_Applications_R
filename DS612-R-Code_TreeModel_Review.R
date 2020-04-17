# Decision Tree Review

# Objective: Practice using tree model for prediction
rm(list=ls(all=TRUE))
library(randomForest)
library(ISLR)
library(MASS)
library(tree)
library(gbm)

?Boston
attach(Boston)
head(Boston)

# Create a binary variable for high and low medv
range(medv)
homePrice <- ifelse(medv<=24, "Low", "High")
boston <- data.frame(Boston, homePrice)
boston <- boston[,-14]
head(boston) 

# Spliting the data into training and testing set, 
set.seed(1) 
n = nrow(boston)
n # that is 506
train = sample(1:n, 300)
test = -train
trainData = boston[train,]
testData  = boston[test,]
testOutcome = boston$homePrice[test]

# Fitting a Classification Tree Model
treeFit <- tree(homePrice~., data=trainData)
summary(treeFit)

# Plot the tree diagram
plot(treeFit)
text(treeFit, pretty=0)

# See each of the split from the tree model
treeFit

# Check the test error rate
treePred <- predict(treeFit, testData, type="class")
table(treePred,testOutcome)
mean(treePred != testOutcome)  # 0.199029


# Fitting a Prunning Tree Model
# Using 5-fold and 10-fold cross validation to find the best size of tree

# 5-fold Cross Validation
cv5_Tree <- cv.tree(treeFit, FUN=prune.misclass, K=5)
cv5_Tree

# Plot the result
plot(cv5_Tree$size, cv5_Tree$dev, type="b", col="blue")

# 10-fold Cross Validation
cv10_Tree <- cv.tree(treeFit, FUN=prune.misclass, K=10)
cv10_Tree

# Plot the result
lines(cv10_Tree$size, cv10_Tree$dev, type="b", col="red")
legend(12, 80, legend=c("5-Fold CV", "10-Fold CV"),
       col=c("blue", "red"), lty=1)

# Both 5-fold and 10-fold CVs confirmed the best size of the tree is 4

# Fitting the best prunning tree
prunedFit = prune.misclass(treeFit, best=4)
prunedFit

# Plot the Pruned Tree
plot(prunedFit)
text(prunedFit, pretty=0)

# Check the test error rate for the pruned tree
prunedPred <- predict(prunedFit, testData, type="class")
table(prunedPred,testOutcome)
mean(prunedPred != testOutcome)  # 0.1359


# Bagging 
# Again, bagging is just a special case of random forest, which uses all the avaialbe
# predictors for tree splitting from the bootstrap data sets
bagTreeFit <- randomForest(homePrice~., data=trainData, mtry=13, importance=TRUE, ntree=500)
summary(bagTreeFit)

# Check the most important predictor from the data
importance(bagTreeFit)

# Plot the importance
varImpPlot(bagTreeFit)

# Check the testing error rate
bagTreePred <- predict(bagTreeFit, newdata=testData, type="class")
table(bagTreePred,testOutcome)
mean(bagTreePred != testOutcome)  # 0.1165

# Random Forest
# Now, let's see what is a good number of predictors to use for spliting the tree
rfError = rep(0,13)
for(d in 1:13){
  rfTreeFit = randomForest(homePrice~., data=trainData, mtry=d, importance=TRUE, ntree=1000)
  rfTreePred = predict(rfTreeFit, newdata=testData, type='class')
  rfError[d] = mean(rfTreePred != testOutcome)
}
MTRY = c(1:13)

# Plot the MSE for each size of ntry
plot(MTRY, rfError, type="b", col="blue")
data.frame(MTRY, rfError)

# Use maximum 3 predictors for spliting trees from bootstrap data set
rfTreeFit = randomForest(homePrice~., data=trainData, mtry=3, importance=TRUE, ntree=500)
summary(rfTreeFit)

# Check the most important predictor from the data
importance(rfTreeFit)

# Plot the importance
varImpPlot(rfTreeFit)

# Check the testing error rate
rfTreePred <- predict(rfTreeFit, newdata=testData, type="class")
table(rfTreePred,testOutcome)
mean(rfTreePred != testOutcome)  # 0.0873786


# Boosting
# Building a forest of tree with "stumps", where depth=1.
# Learning rate (shrinkage) = 0.01 and build with 5000 trees
trainData$price <- ifelse(trainData$homePrice == "High", 1, 0)
testData$price <- ifelse(testData$homePrice == "High", 1, 0)

boostTreeFit <- gbm(price~.-homePrice, data=trainData, distribution="bernoulli",
                    n.trees=5000, interaction.depth=1, shrinkage=0.01)
summary(boostTreeFit)

# Check the testing error rate
testOutcome2 = testData$price
boostTreePred <- predict(boostTreeFit, newdata=testData, n.trees=5000)
boostTreePred
boostTreePred <- ifelse(boostTreePred > 0, 1, 0)
table(boostTreePred,testOutcome2)
mean(boostTreePred != testOutcome2)  #0.0873786
