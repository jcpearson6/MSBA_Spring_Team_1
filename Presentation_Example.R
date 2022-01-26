## Problem 4-15

# Part A: Create Power Function
Power <- function(){
  2^3
}
Power()

# Part B: Power2()
Power2 <- function(x,a){
  x^a
}
Power2(3,8)

# Part C:
Power2(10,3)
Power2(8,17)
Power2(131,3)

# Part D: 
Power3 <- function(x,a){
  result <- x^a
  return(result)
}
Power3(2,2)

# Part E: 
plot(Power3(1:10,2))

# Part F:
PlotPower <- function(x,a){
  plot(x^a)
}
PlotPower(1:10,3)
