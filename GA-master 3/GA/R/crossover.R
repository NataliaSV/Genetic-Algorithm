# Create vector of where to split each chromosome
generate_split_locations <- function(parents_A, parents_B, num_split = 1) {
  
  # Random selection
  split_location <- matrix(rep(0, num_split * nrow(parents_A)), 
                           nrow = nrow(parents_A), ncol = num_split)
  for (i in 1:nrow(parents_A)) {
    split_location[i,] <- sort(sample(1:(ncol(parents_A) - 1),
                               size = num_split, replace = F))
  }
  split_location <- cbind(rep(0,nrow(parents_A)), split_location, 
                          rep(ncol(parents_A), nrow(parents_A)))
  return(split_location)
}

### Selects genes based on split location from parents A and B
# Pick genes from alternating parents
select_genes <- function(index, split_indices, A, B, random_remainder = 1) {
  if (index %% 2 == random_remainder) {
    selected_gene <- A[(split_indices[index] + 1):(split_indices[index+1])]
  }
  else {
    selected_gene <- B[(split_indices[index] + 1):(split_indices[index+1])]
  }
  
  return(selected_gene)
}

# Function to make a single child from two parents
breed_single <- function(A, B, split_indices) {
  # Randomize which parent is selected from first when performing crossover
  remainder <- sample(c(0,1), 1)
  child <- unlist(sapply(1:(length(split_indices) - 1), select_genes, 
                         split_indices, A, B, remainder))
  return(child)
}

### Default crossover function
# Arguments: 
#   parents_A, parents_B: Two parent matrices
#   num_split: the number of locations to split each parent by when breeding.
#     Default is 1.
#     For example, if the chromosome length is 10 and num_split=2, the parents 
#     may be split at indices 2 and 5.
#     Then, the child might inherit the first 2 genes from parent B, the next 3 
#     genes from parent A, and the last 5 from B.
# Returns: full child matrix

crossover <- function(parents_A, parents_B, num_split = 1) {
  
  split_location <- generate_split_locations(parents_A = parents_A,
                                             parents_B = parents_B,
                                             num_split = num_split)
  
  # Apply breed_single to every pair of parents
  child_matrix <- matrix(rep(0, nrow(parents_A) * ncol(parents_A)),
                         nrow(parents_A), ncol(parents_A))
  for (i in 1:nrow(parents_A)) {
    child_matrix[i,] <- breed_single(parents_A[i,], parents_B[i,], split_location[i,])
  }
  
  return(child_matrix)
}