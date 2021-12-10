### STAT243 Group Project
### Mutation
### Jonathan Ling

library(testthat)

mutate <- function(chromosomes, mutateProbability = 0.01) {
  # Inputs: 
  #   chromosomes: matrix of chromosomes
  #   mutateProbability: the probability for each gene to mutate. Default
  #                       is 1%.
  # Output:
  #   Matrix of mutated chromosomes
  
  # Mutate by subtracting 1 from the element and taking the absolute value
  #   Changes 0 to 1, changes 1 to 0
  # Generate mutation matrix from Bernoulli(mutateProbablity) distribution
  mutatedChromosomes <- abs(chromosomes - 
                            matrix(rbinom(nrow(chromosomes) * ncol(chromosomes),
                                            1, mutateProbability), 
                            nrow = nrow(chromosomes), ncol = ncol(chromosomes))
                            )
  return(mutatedChromosomes)
}



### Test Case
test_mat <- matrix(sample(c(0,1), 200, replace = T),10,20)
mutated_mat <- mutate(test_mat)
test_mat - mutated_mat

test_that("the output is a matrix of the same size as the input and is binary",
          {
            expect_is(mutated_mat, 'matrix')
            expect_true(nrow(mutated_mat)==nrow(test_mat))
            expect_true(ncol(mutated_mat)==ncol(test_mat))
            expect_true(mutated_mat == 1 || mutated_mat == 0)
            
          })
