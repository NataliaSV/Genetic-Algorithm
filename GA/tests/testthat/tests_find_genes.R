test_that("find_genes works with vector type inputs: ", {
  chromosome <- c()
  variable_names <- c(0,1,2)
  
  expect_error(find_genes(chromosome,variable_names),
               'chromosome or variable_names is empty')
  
  chromosome <- c(0,1,2)
  variable_names <- c(NA)
  
  expect_error(find_genes(chromosome,variable_names),
               'chromosome and variable_names cannot contain NAs')
  
})
