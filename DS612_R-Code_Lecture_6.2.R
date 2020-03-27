#---------------------------------------#
#---------------Chapter 8---------------#
#-----------Tree-Based Methods----------#
#---------------------------------------#

# Fitting Classifiction Tree Models

# If you cannot find the "tree" library in R use this package
install.packages("tree")
# book's Libraray
library(ISLR) 
# This libraray is essential to fit decision trees
library(tree) 

# The dataset is from ISLR Library
attach(Carseats) 

# let's explore the dataset
head(Carseats)
dim(Carseats)
# 400 observations 11 variables
# We can only use binary response outcomes, so let's create a
# Dummy variable based on sales

# First let's look up the range for Sales
range(Sales)
# It is from 0 to 16.27
# so, let's split them in half and call high sales for
# Sales more than 8 
High = ifelse(Sales >= 8, "Yes", "No")
Carseats = data.frame(Carseats, High)
dim(Carseats)
# Variable High was added to the data

# Let's get rid of Sales data - Since we already have High
Carseats = Carseats[,-1]
head(Carseats) 
# We got rid of Sales data

# Now is the time to split our data to training and test data
# Let's randomly divide our data into two groups - test and training

#you can set.seed to any number
set.seed(2) 
NumberofObservations = nrow(Carseats)
NumberofObservations # that is 400
SplitofTrainTest = 0.5 # let's split the data 50/50
train = sample(1:NumberofObservations,NumberofObservations*SplitofTrainTest)
test = -train
trainingData = Carseats[train,]
testingData  = Carseats[test,]
Testing_outcome = High[test] # The is the our test outcomes


# Now it is the TREE TIME :)
# You need to use training data
Tree_Model = tree(High~.,trainingData)
plot(Tree_Model) # note there is no text on the tree
text(Tree_Model, pretty = 0) # Use pretty = 0 only when you have categorical variables

#Let's check the model based on the test data
Tree_pred = predict(Tree_Model,testingData,type="class") #Since our predictions are on categorical variables we used type = "class"

# Let's compute the error
mean(Tree_pred != Testing_outcome) #0.23

##### PRUNE THE TREE

# May be we can do better with pruning
# again, you can set it to any number!
set.seed(3) 

# Since we dealt with classification we neede ot set FUN to prune.misclass 
cv_tree = cv.tree(Tree_Model, FUN=prune.misclass) 
# Size is the size of the tree, Dev is cross-validation error rate
names(cv_tree) 
plot(cv_tree$size,cv_tree$dev,type="b")
# Our min happened at size = 9, you may get a different answer based on your random seeds

# Now let's prune our tree - that is the gardening time!
# We pruned our model based on best size we found
pruned_Model = prune.misclass(Tree_Model, best=9) 
# in cross-validation - remember it was 9 (you might have found another number)
plot(pruned_Model)
text(pruned_Model)

Tree_pred_new = predict(pruned_Model,testingData,type="class")
mean(Tree_pred_new != Testing_outcome) 
# error decreased to 22.5% Slightly better than what we had earlier