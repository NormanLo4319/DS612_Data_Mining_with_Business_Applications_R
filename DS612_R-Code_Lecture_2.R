# In R, we can create and save our own function to use in the global environment
# For example we can create a TestFunction() that take two numbers and return sum and product of the two numbers.
TestFunction = function(number1,number2){
  SumResult = number1+number2
  ProductResult = number1 * number2
  c(SumResult,ProductResult)
}

# Create a variable x to save the result with TestFunction() that takes inputs 2 and 4.
x = TestFunction(2,4)

# You can check the result by printing the varaible x
x

# You can also extract the result of each element stored in variable x by its index
x[1]
x[2]

# Note that once the function is defined in the global environment, it can be reused as many time as you want until it is cleared from the globa enivronment.
a = TestFunction(10, 20)

# Let's practice creating a function that take two vectors a and b.
# The DistanceTest() function will be use to calculate the square-root of the sum of squared difference
DistanceTest = function(a,b){
  Dist = sqrt(sum((a-b)^2))
  Dist
}

# Create vector x1 and x2
x1 = c(1,2,3,4)
x2 = c(0,0,1,2)

# Using the DistanceTest function to calculate the result
DistanceTest(x1,x2)

# We can manually check the answer by writing the whole equation (hard-coding)
D= sqrt((1-0)^2+(2-0)^2+(3-1)^2+(4-2)^2)
D


# Write a function that has a vector as its input and returns the mean and Standard Deviation
?sd
?mean

FunctionMeanSD = function(a){
  a_mean = mean(a)
  a_sd = sd(a)
  c(a_mean,a_sd)
}

# The function will take a vector and calculate and return its mean and standard deviation.
FunctionMeanSD(x1)
