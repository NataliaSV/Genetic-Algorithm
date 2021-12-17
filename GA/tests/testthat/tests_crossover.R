test_that("the output is a matrix and is the same size as the parent matrices
          with values from either parent", {
  test_mat_A <- matrix(rep(0,100),10,10)
  test_mat_B <- matrix(rep(1,100),10,10)
  test_child <- crossover(test_mat_A, test_mat_B, num_split = 3)
  
  expect_is(test_child, 'matrix')
  expect_true(nrow(test_child)==nrow(test_mat_A))
  expect_true(ncol(test_child)==ncol(test_mat_A))
})
