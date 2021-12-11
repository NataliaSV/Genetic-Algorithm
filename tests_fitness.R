# load the source code of the functions to be tested
source("initialization.R")

# tests for the fitness function
test_that("fitness works with formula and dataframe type inputs: ", {
  formula <- "y ~ x "
  data <- matrix(runif(100), ncol=10, nrow = 10)
  
  expect_error(fitness_function(formula, data),
               'the input is not an actual formula')
  
  formula <- as.formula("y ~ x")
  data <- as.data.frame(NULL)
  
  expect_error(fitness_function(formula, data),
               'data cannot be empty')
  
})