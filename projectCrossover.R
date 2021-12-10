### STAT243 Group Project
### Crossover
### Jonathan Ling

library(testthat)

### Default crossover function
# Arguments: 
#   parents_A, parents_B: Two parent matrices
#   num_split: the number of locations to split each parent by when breeding.
#     Default is 1.
#     For example, if num_split=2, the parents may be split at indices 2 and 5.
#     Then, the child might inherit the first 2 genes from parent B, the next 3 
#     genes from parent A, and the last 5 from B.
# Returns: full child matrix

crossover <- function(parentsA, parentsB, num_split = 1) {
  
  # Create vector of where to split each chromosome
  # Random selection
  split_location <- matrix(rep(0, num_split * nrow(parentsA)), 
                          nrow = nrow(parentsA), ncol = num_split)
  for (i in 1:nrow(parentsA)) {
    split_location[i,] <- sort(sample(1:(ncol(parentsA) - 1),
                                size = num_split, replace = F))
  }
  split_location <- cbind(rep(0,nrow(parentsA)), split_location, 
                         rep(ncol(parentsA), nrow(parentsA)))
  
  ### Combine genes based on split location from parents A and B
  # Pick genes from alternating parents
  select_genes <- function(index, split_indices, A, B, random_remainder = 1) {
    if (index %% 2 == random_remainder) {
      selected_gene <- A[(split_indices[index] + 1):(split_indices[index+1])]
    }
    else {
      selected_gene <- B[(split_indices[index] + 1):(split_indices[index+1])]
    }
  }
  
  # Function to make a single child from two parents
  breed_single <- function(A, B, split_indices) {
    # Randomize which parent is selected from first when performing crossover
    remainder <- sample(c(0,1), 1)
    child <- unlist(sapply(1:(length(split_indices) - 1), select_genes, 
                    split_indices, A, B, remainder))
    return(child)
  }
  
  # Apply breed_single to every pair of parents
  child_matrix <- matrix(rep(0, nrow(parentsA) * ncol(parentsA)),
                        nrow(parentsA), ncol(parentsA))
  for (i in 1:nrow(parentsA)) {
    child_matrix[i,] <- breed_single(parentsA[i,], parentsB[i,], split_location[i,])
  }
  
  return(child_matrix)
}

