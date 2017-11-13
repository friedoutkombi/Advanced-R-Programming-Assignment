# The purpose of this file is to outline the functions used in Part 1 of the
# Functional & Object Oriented Programming Assignment for the Coursera - Advanced
# R Programming course.

# Descr: The objective of Part 1 is to write a function that computes the factorial of an
# integer greater than or equal to 0. Recall that the factorial of a number n is 
# n * (n-1) * (n - 2) * … * 1. The factorial of 0 is defined to be 1.

#-----------------------LOAD LIBRARIES---------------------------#
library(purrr)
library(microbenchmark)

#-----------------------FACTORIAL_LOOP---------------------------#
# A version that computes the factorial of an integer using looping (such as a for loop)

factorial_loop<-function(n) {
  # Error proof the function
  if(n <= 0 | n%%1 != 0 )  stop('n must be integer greater than 0') 
  # Set starting value for product
  product=1
  # Generate factorial
  for (i in seq(n)) {
    product=product*i
  }
  # Return factorial
  return(product)
}

#-----------------------FACTORIAL_REDUCE-------------------------#
# a version that computes the factorial using the reduce() function in the purrr package. 
# Alternatively, you can use the Reduce() function in the base package.
# Please note when unit testing these in development could only get n<=13 before integer overflows
# were recorded as errors.
factorial_reduce<-function(n) {
  # Error proof the function
  if(n <= 0 | n%%1 != 0 )  stop('n must be integer greater than 0') 
  # Generate factorial
  product<-reduce(1:n, `*`)  
  # Return factorial
  return(product)
}

#-----------------------FACTORIAL_FUNC---------------------------#
# A version that uses recursion to compute the factorial
factorial_func <- function(n) {
  # As recrusvie set for n when equal to 0
  if (n ==0) return(1)
  # Set recursion
  n * factorial_func(n - 1)
}

#-----------------------FACTORIAL_MEM----------------------------#
# A version that uses memoization to compute the factorial.

# First create empty memory table
mem_table<-c(rep(NA,1000))

factorial_mem<-function(n) {
  # As recrusvie set for n when equal to 0
  if (n ==0) return(1)
  if (!is.na(mem_table)[n])
    return(mem_table[n])
  mem_table[n]<<-n*factorial_mem(n-1)
  mem_table[n]
}



#------------------------MICROBENCHMARK--------------------------#
# Use microbenchmark to time operation of these functions and provide a
# summary of their performance. Make sure to show a range of inputs in 
# order to demonstrate the timing of each function for larger inputs

# Set output to text file with title
sink("factorial_output.txt")
cat("microbenchmark tests for various factorial finding functions\n")

# Calculate and compare perforamnce of individual input values
microbenchmark_results <- map(c(1,5,10,20,30,50,100), ~ microbenchmark(
  factorial_loop(.),
  factorial_reduce(.),
  factorial_func(.),
  factorial_mem(.)
))

# Set names so we know each record belongs to which factorial number
names(microbenchmark_results) <- as.character(c(1,5,10,20,30,50,100))
microbenchmark_results

# Close output
sink()

#---------------------------COMMENTS-----------------------------#
# Interesting to note that the use of memoisation ends up being very quick in the long run!