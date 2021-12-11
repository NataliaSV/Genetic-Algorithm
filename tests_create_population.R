# load the source code of the functions to be tested
source("initialization.R")

# tests for the create_population function
test_that("create_population works with integer type inputs: ", {
  chromosome_length <- 10.5
  population_size <- 4
  
  expect_error(create_population(chromosome_length, population_size),
               'chromosome_length must be a positive integer value')
  
  chromosome_length <- 10
  population_size <- 4.4
  
  expect_error(create_population(chromosome_length, population_size),
               'population_size must be a positive integer value')
  
  chromosome_length <- 0
  population_size <- 0
  
  expect_error(create_population(chromosome_length, population_size),
               'chromosome_length and population_size must be positive integer values')
  
})