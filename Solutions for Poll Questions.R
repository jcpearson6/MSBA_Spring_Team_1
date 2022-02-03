Test_function <- function(add,mult,div) {((add + add)*mult)/div}

Test_function(2,3,6)
Test_function(2, div = 6, mult = 3)
Test_function(div=6, 3,add=2)
Test_function(div = 6, add=2, mult=3)




#Question #5 solution 
array_count9 <- function(nums){
  count = 0
  for (i in nums){
   x = ifelse(i==9,1,0)
   count = count + x
  }
  return(count)
}
array_count9(c(1, 9, 9, 9, 9))
array_count9(c(1, 9, 9))
array_count9(c(1, 9, 9, 3, 9))




#Question #6 solution 
array123 <- function(nums){
  for (i in 1:(length(nums)-2)){
    if (nums[i]==1 && nums[i+1]==2 && nums[i+2]==3){
      return(TRUE)
   }
  }
  return(FALSE)
}

array123(c(1, 1, 2, 3, 1))
array123(c(1, 1, 2, 4, 1))
array123(c(1, 1, 2, 1, 2, 3))
array123(c(1, 1, 2, 2, 2, 3))
