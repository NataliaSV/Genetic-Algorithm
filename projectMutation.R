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

