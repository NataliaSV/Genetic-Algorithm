test_that("the output matrix is formatted correctly and errors are thrown when given wrong input", {
  n <- 100
  test_matrix <- matrix(sample(0:1, n * n, replace = TRUE), n, n)
  fitness <- runif(n)

  result_matrix <- rank_selection(cbind(test_matrix, fitness))
  
  expect_is(result_matrix, 'matrix')
  expect_equal(ncol(result_matrix), ncol(test_matrix))
  expect_equal(nrow(result_matrix), nrow(test_matrix))
  expect_error(tournament_selection(fitness))
  expect_error(tournament_selection(1))
})
