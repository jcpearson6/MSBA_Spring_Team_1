## Problem 4-15

# Part A: 
#  Write a function, Power(), that prints out the result of raising 2
# to the 3rd power. In other words, your function should compute
# 23 and print out the results.
Power <- function(){
  2^3
}
Power()

# Part B: 
# Create a new function, Power2(), that allows you to pass any
# two numbers, x and a, and prints out the value of x^a.
Power2 <- function(x,a){
  x^a
}
Power2(3,8)

# Part C:
# Using the Power2() function that you just wrote, compute 10^3,
# 8^17, and 131^3.
Power2(10,3)
Power2(8,17)
Power2(131,3)

# Part D:
# Now create a new function, Power3(), that actually returns the
# result x^a as an R object, rather than simply printing it to the
# screen.
Power3 <- function(x,a){
  result <- x^a
  return(result)
}
Power3(2,2)

# Part E:
# Now using the Power3() function, create a plot of f(x) = x2.
# The x-axis should display a range of integers from 1 to 10, and
# the y-axis should display x2. Label the axes appropriately, and
# use an appropriate title for the figure. 
plot(Power3(1:10,2), xlab="Base Value", ylab="Squared Value", main = "Plot of Squared Values")

# Part F:
# Create a function, PlotPower(), that allows you to create a plot
# of x against x^a for a fixed a and for a range of values of x.
PlotPower <- function(x,a){
  plot(x^a, xlab="X", main="Plot of X vs X^a")
}
PlotPower(1:10,3)






