test_that("set formulas works with character type inputs: ", {
  active_genes <- c("var_1","var_2")
  name_y <- c(1)
  
  expect_error(set_formulas(active_genes, name_y),
               'the name of the target variable is not a string')
  
  active_genes <- c(1,2)
  name_y <- c()
  
  expect_error(set_formulas(active_genes, name_y),
               'the name of the target variable is not a string')
})
