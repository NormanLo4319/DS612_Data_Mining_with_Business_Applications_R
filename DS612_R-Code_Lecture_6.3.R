#---------------------------------------#
#---------------Chapter 8---------------#
#-----------Tree-Based Methods----------#
#---------------------------------------#

# Regression Tree Models

# Remove or clear all memories in the global environment
rm(list=ls(all=TRUE))

# book's Libraray
library(ISLR) 
# If you cannot find it in R use this package
install.packages("tree")
# This libraray is essential to fit decision trees
library(tree) 
# The dataset is from ISLR Library
attach(Carseats) 

# let's explore the dataset
head(Carseats)
# 400 observations 11 variables
dim(Carseats) 
summary(ShelveLoc)

# We want to predict Sales based on other variables 
# Now is the time to split our data to training and test data
# Let's randomly divide our data into two groups - test and training

# you can set.seed to any number
set.seed(2) 
NumberofObservations = nrow(Carseats)
NumberofObservations # that is 400
SplitofTrainTest = 0.5 # let's split the data 50/50
train = sample(1:NumberofObservations,NumberofObservations*SplitofTrainTest)
test = -train
trainingData = Carseats[train,]
testingData  = Carseats[test,]
Testing_outcome = Sales[test] # The is the our test outcomes


# Now it is the TREE TIME :)
# You need to use training data
Tree_Model = tree(Sales~.,trainingData)
# note there is no text on the tree
plot(Tree_Model)
# Use pretty=0 only when you have categorical variables
# (ShelveLoc is a categorical variables)
text(Tree_Model, pretty=0) 

# Let's check the model based on the test data
Tree_pred = predict(Tree_Model,testingData) 
names(Tree_pred)
# Let's compute the error
mean((Tree_pred - Testing_outcome)^2) 
#4.844991 You may find another number based on your random seed


##### PRUNE THE TREE

# May be we can do better with pruning
# you can set it to any number!
set.seed(3) 
# Five fold cross-validation
cv_tree = cv.tree(Tree_Model,K=5)
# Size is the size of the tree, Dev is cross-validation Error
names(cv_tree)
# Our min happened at size=12, you may get a different answer based on your random seeds
plot(cv_tree$size,cv_tree$dev,type="b")


# Now let's prune our tree - that is the gardening time!
pruned_Model = prune.tree(Tree_Model, best=12) #We pruned our model based on best size we found
# in cross-validation - remember it was 12 (you might have found another number)
plot(pruned_Model)
text(pruned_Model,pretty=0)

Tree_pred_new = predict(pruned_Model,testingData)
mean((Tree_pred_new - Testing_outcome)^2) #error decreased to 4.667

