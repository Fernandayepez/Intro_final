# test_functions.R
library(testthat)
source("Final.R")  

# Write a test for the function
test_that("calculate_sum_squared_differences returns the expected result for a vector", {
  # Input vector
  input_vector <- 1:10
  
  # Expected result 
  expected_result <- 0.1090909
    
  # Actual result
  actual_result <- calculate_sum_squared_differences(input_vector)
  
  # Check if the actual result is equal to the expected result
  expect_equal(actual_result, expected_result)
})

test_that(" create_adjancency_matrix returns an expected matrix given an edge matrix", {
 library(ape)
  #set up needed 
  original_tree <- read.tree(text = "((A, B),  (C,D));")
  edges <- original_tree$edge
  
  #expected result
  expected_result <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0), nrow = 7,
  byrow = TRUE)
  
  #Actual result
  actual_result <-create_adjacency_matrix(edges)
  
  #Check if the same 
  expect_equal(actual_result, expected_result)
  
  })

test_that("get_edge_matrix gives us the expected matrix given an adj", {
  
  #Input 
  adj_matrix <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 0, 0, 0, 0, 0,
                         0, 0, 1, 2, 0, 0, 0), nrow = 7, byrow = TRUE)
  
  #expected result
  expected_matrix <- matrix(c(5, 6, 6, 1, 6, 2, 5, 7, 7, 3, 7, 4), nrow = 6, byrow = TRUE)
  print(expected_matrix)
  print(class(expected_matrix))
  
  #Actual result
  actual_result <-get_edge_matrix(adj_matrix)
  print(actual_result)
  print(class(actual_result))
  
  #Check if the same 
  expect_equivalent(actual_result, expected_matrix)
  
})
