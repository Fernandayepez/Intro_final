# test_functions.R
library(testthat)
source("Final.R")  

# Write a test for the function
test_that("calculate_sum_squared_differences returns the expected result for a vector", {
  # Input vector
  input_vector <- 1:10
  
  # Expected result (you need to calculate or obtain this)
  expected_result <- 0.1090909
    
    # Actual result
    actual_result <- calculate_sum_squared_differences(input_vector)
  
  # Check if the actual result is equal to the expected result
  expect_equal(actual_result, expected_result)
})

