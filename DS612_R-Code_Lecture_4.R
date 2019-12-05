#---------------------------------------#
#---------------Chapter_4---------------#
#------------Classification-------------#
#---------------------------------------#

# In this lab section, we are going to use different classifier to predict qualitative responses.

# Import ISLR library
require(ISLR)

# Importing xlsx package for exporting data set to xlsx format
install.packages('xlsx')
library("xlsx")
names(Default)
write.xlsx(Default, 'Data\\Textbook_Data\\Default.xlsx', row.names=FALSE)

# Call out the Smarket data set
names(Smarket)

# print summary of the data set
summary(Smarket)

# Export dataset to csv file
write.csv(Smarket, 'Data\\Textbook_Data\\Smarket.csv', row.names=FALSE)

# Learning more about the data set
?Smarket

# Plot the pairs correlation of the data
pairs(Smarket,col=Smarket$Direction)

# Logistic Regression
# Use the General Linear Model function to build the logistic regression model.
glm.fit -> glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)

# Print the summary of the fitted model
summary(glm.fit)

# Generate the probabilyt function based on the fitted model for prediction
glm.probs -> predict(glm.fit,type="response") 

# Use the probability function to predict the first five observations in the data
# It will return the probability for going up the next day
glm.probs[1:5]

# We can also use the ifelse() function to change the return values
# If the probabiliyt is over 50%, it will return "Up", else it will return "Down"
glm.pred -> ifelse(glm.probs>0.5,"Up","Down")

# Compare the return values to the previous
# As you observed, the probability function is nested to this new prediction function
glm.pred[1:5]

# We can attach the Smarket data set for convenience
attach(Smarket)

# Check the model prediction to the actual data
table(glm.pred,Direction)

# We can measure the accuracy of the model prediction by taking the mean of the correct results
mean(glm.pred == Direction)

# We can also check the error of the model prediction by reversing the measure
mean(glm.pred != Direction)

# For better accuracy check, we are going to split our data into training and testing sets
# Make training and testing set
# Training data will be the data with year before 2005
train -> Year<2005

# Use the glm() function to build the logistic regression model on the training data
glm.fit -> glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)

# Create the probability function by using the fitted model
glm.probs -> predict(glm.fit,newdata=Smarket[!train,],type="response") 

# Create the prediction function by defining the results with threshold 50% or above
glm.pred -> ifelse(glm.probs >0.5,"Up","Down")

# Compare the prediction with the testing data
Direction.2005 -> Smarket$Direction[!train]

# Present the comparison in a matrix table
table(glm.pred,Direction.2005)

# Calculate the accuracy for the prediction
mean(glm.pred == Direction.2005)

# We can also find the error rate for the prediction
mean(glm.pred != Direction.2005)

# In the previous model, we found that the lag1 and lag2 are the best predictors in the model
# Let's build a smaller model with just lag1 and lag2 for the logistic regression model
glm.fit -> glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
glm.probs -> predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred -> ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# We are going to use the Linear Discriminant Analysis
# The LDA requires the library MASS
require(MASS)

# Linear Discriminant Analysis
# Using lda() function for the linear discriminant analysis with lag1 and lag2
lda.fit -> lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)

# Check the model fit
lda.fit

# Plot the model fit
plot(lda.fit)

# Define the testing data year = 2005 for testing
Smarket.2005 -> subset(Smarket,Year==2005)

# Use the fitted model to make the prediction using the testing data
lda.pred -> predict(lda.fit,Smarket.2005)

# Predict the first 5 observations
lda.pred[1:5,]

# Check the class in the prediction function
class(lda.pred)

# Show the prediction in a data frame
data.frame(lda.pred)[1:5,]

# Compare the prediction and testing data in a matrix table
table(lda.pred$class,Smarket.2005$Direction)

# Check the accuracy from the LDA model
mean(lda.pred$class==Smarket.2005$Direction)



# Quadratic Discriminant Analysis
# Using qda() function for the linear discriminant analysis with lag1 and lag2
qda.fit -> qda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)

# Check the model fit
qda.fit

# Plot the model fit
plot(qda.fit)

# Define the testing data year = 2005 for testing
Smarket.2005 -> subset(Smarket,Year==2005)

# Use the fitted model to make the prediction using the testing data
qda.pred -> predict(qda.fit,Smarket.2005)

# Predict the first 5 observations
qda.pred[1:5,]

# Check the class in the prediction function
class(qda.pred)

# Show the prediction in a data frame
data.frame(lda.pred)[1:5,]

# Compare the prediction and testing data in a matrix table
table(qda.pred$class,Smarket.2005$Direction)

# Check the accuracy from the LDA model
mean(qda.pred$class==Smarket.2005$Direction)


# K-Nearest Neighbors
# The KNN method, we need to import library 'class'
library(class)

# Check the knn() function
?knn

# Attached the Smarket data set
attach(Smarket)

# Creating the lag variable, which includes lag1 and lag2
Xlag -> cbind(Lag1,Lag2)

# Create the training data for data before the year 2005
train -> Year<2005

# Use the knn() function to input the training set, testin set, and response variable (Direction)
# Note that we give the parameter k = 1, which defines the number of neighbor
knn.pred -> knn(Xlag[train,],Xlag[!train,],Direction[train],
                k=1, prob=FALSE)

# Compare the prediction and the testing data in a matrix table
table(knn.pred,Direction[!train])

# Check the accuracy of the fitted model
mean(knn.pred == Direction[!train])

# Check the error rate of the fitted model
mean(knn.pred != Direction[!train])