rm(list = ls())

#Acknowledgement: This function is written based on the
#work submitted by Dj Yauger on Oct 20th 2014


TestTrainSplit = function(Data,ProportionTrain)
{
  n=dim(Data[1])
  TrainingSet = sample(1:n[1],ProportionTrain*n[1],replace = FALSE, prob = NULL)
  TeseSet = (1:n[1])[-TrainingSet]
  TrainingData = Auto[TrainingSet,]
  TestData = Auto[TeseSet,]
  list(TestData = TestData,TrainingData = TrainingData)
}


require(ISLR)
require(boot)
#An Example using Auto Data - It is splited into 90% Training and 10% Test Data
#You can change 90% to any number at your will. The only thing you need to do is
# to change 0.9 to the number you wish
x = TestTrainSplit(Auto,0.9)
#The TestTrainSplit function is called. The first argument of the function is
#the original Data Set - Auto in this case - and the 2nd component is your desired training proportion.


TrainingSample = x$TrainingData
#you must put $ in front of x - the variable you saved the outputs of your functions 
#and call the TrainingData 
TestSample = x$TestData
#you must put $ in front of x - the variable you saved the outputs of your functions 
#and call the TestData

#You can now use the Training and Test Sample.
