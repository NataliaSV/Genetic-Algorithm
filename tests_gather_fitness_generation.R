# load the source code of the functions to be tested
source("initialization.R")
library(extraDistr)

# tests for the gather_fitness_generation function
test_that("gather_fitness_generation works with non empty, numeric inputs: ", {
  
  generation <- as.data.frame(NULL)
  fitness_scores <- runif(10)
  
  expect_error(gather_fitness_generation(generation,fitness_scores),
               'generation cannot be empty')
  
  generation <- as.data.frame(matrix(rbern(90), ncol=9, nrow = 10))
  fitness_scores <- c(NULL)
  
  expect_error(gather_fitness_generation(generation,fitness_scores),
               'fitness_scores cannot be empty')
  
  fitness_scores <- c("a","b")
  
  expect_error(gather_fitness_generation(generation,fitness_scores),
               "fitness_scores need to be numeric")
  
})