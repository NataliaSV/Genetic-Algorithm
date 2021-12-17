# Inputs: 
#   chromosomes: matrix of chromosomes
#   mutate_probability: the probability for each gene to mutate. Default
#                       is 0.01.
# Output:
#   Matrix of mutated chromosomes

mutate <- function(chromosomes, mutate_probability = 0.01) {
  # Mutate by subtracting 1 from the element and taking the absolute value
  #   Changes 0 to 1, changes 1 to 0
  # Generate mutation matrix from Bernoulli(mutate_probablity) distribution
  mutation_matrix <- matrix(
    rbinom(nrow(chromosomes) * ncol(chromosomes), 1, mutate_probability), 
    nrow = nrow(chromosomes), 
    ncol = ncol(chromosomes)
  )
                            
  mutated_chromosomes <- abs(chromosomes - mutation_matrix)
  
  return(mutated_chromosomes)
}

