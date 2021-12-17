test_mat <- matrix(sample(c(0,1), 200, replace = T),10,20)
mutated_mat <- mutate(test_mat)
test_mat - mutated_mat

test_that("the output is a matrix of the same size as the input and is binary", {
  expect_is(mutated_mat, 'matrix')
  expect_true(nrow(mutated_mat)==nrow(test_mat))
  expect_true(ncol(mutated_mat)==ncol(test_mat))
})
