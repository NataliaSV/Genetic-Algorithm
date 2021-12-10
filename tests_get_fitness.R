# load the source code of the functions to be tested
source("Functions.R")

# tests for the get_fitness function
test_that("get_fitness works with a target variable with name of string type and other non empty inputs: ", {
  
  library(extraDistr)
  data <-  matrix(runif(100), ncol=10, nrow = 10)
  names(data) <- c("var1","var2","var3","var4","var5","var6","var7","var8","var9","var10")
  name_y <- 1
  generation <- matrix(rbern(90), ncol=9, nrow = 10)
  
  expect_error(get_fitness(data, name_y, generation),
               "the name of the target variable is not a string")
  
  data <-  as.data.frame(NULL)
  name_y <- "var8"
  
  expect_error(get_fitness(data, name_y, generation),
               'data cannot be empty')
  
  generation <- as.data.frame(NULL)
  data <-  matrix(runif(100), ncol=10, nrow = 10)
  
  expect_error(get_fitness(data, name_y, generation),
               'generation cannot be empty')
  
})