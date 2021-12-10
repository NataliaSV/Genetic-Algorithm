# load the source code of the functions to be tested
source("Functions.R")

# tests for the set formulas function
test_that("set formulas works with character type inputs: ", {
  active_genes <- c("var_1","var_2")
  name_y <- c(1)
  
  expect_error(set_formulas(active_genes, name_y),
               'the name of the target variable is not a string')
  
  active_genes <- c(1,2)
  name_y <- c("y")
  
  expect_error(set_formulas(active_genes, name_y),
               'the vector with the active genes must be character type')
  
  active_genes <- c(1,2)
  name_y <- c()
  
  expect_error(set_formulas(active_genes, name_y),
               'the name of the target variable is not a string')
  
  active_genes <- c()
  name_y <- c("y")
  
  expect_error(set_formulas(active_genes, name_y),
               'the vector with the active genes must be character type')
  
  
})