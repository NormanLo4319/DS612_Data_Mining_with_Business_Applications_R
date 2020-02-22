# Euclidean Distance in R
# In this exercise, we are trying to use R to understand the logic of Euclidean Distance


# Let's use an example to demonstrate the distance between two points
x <- c(1, 5)

# The distance between the two points 1 and 5 in a one dimension line is the difference of the two numbers
# We can calculate the distance by subtracting the values
5-1

# We can also use the distance function in R to get the answer
dist(x)

# Suppose we have two points in a two dimension plot
p1 <- c(1, 3)
p2 <- c(5, 8)

# In this case, we will need to get the distance between two coordinates
# By basic geometry, we know the distance between the two points can be calculated by using the following equation.
# distance = sqrt((x2-x1)**2 + (y2-y1)**2))
# Note: p1 has the x value equals to 1 and y value equals to 3
# We can now use the equation to calculate the distance.
sqrt((p2[1]-p1[1])**2 + (p2[2]-p1[2])**2)

# We can also utilize the distance function for a two dimension case.
dist(rbind(p1, p2))

# Finally, we can try to think about the distance between two points in a 3 dimension plate
p1 <- c(1, 3, 10)
p2 <- c(3, 2 ,13)

# We can use the following equation for a 3 dimension case
# distance = sqrt((x2-x1)**2 + (y2-y1)**2 - (z2-z1)**2)
# This time, we are going to generate a function for this calculation
euclidean <- function(a, b) {
  distance <- sqrt((a[1]-b[1])**2 + (a[2]-b[2])**2 + (a[3]-b[3])**2)
  return(distance)
}

# Plug in the two coordinates to the function
euclidean(p1,p2)

# Again, we can utilize the distance function in R for this calculation
dist(rbind(p1, p2))
