### STAT243 Group Project
### Parent Selection
### Hans Bak
m <- 50
n <- 100
test_matrix <- round(matrix(runif(m*n), n, m))
fitness <- c(runif(n))

get_selection_propability <- function(rank, population_size) {
  return((2*rank)/(population_size*(population_size+1)))
}

rank_selection <- function(population) {
  sorted_population <- population[order(population[,ncol(population)], decreasing=TRUE), ]
  probability_vec <- sapply(1:nrow(population), function(rank){
    get_selection_propability(rank, nrow(population))
  })
  sorted_population[sample(1:nrow(population), size = nrow(population), prob = probability_vec),]
}

### INPUT:
# population: a matrix containing the chromosomes 
# fitness: a vector of the corresponding fitness of the chromosomes
# num_partitions: number of partitions in the matrix. Defaults to 
#                 the number of chromosomes divided by 3
### OUTPUT:
# A matrix with containing the best chromosomes from each partition.
# The number of rows is equal to the number of partitions. Adds the 
# fitness to the matrix as the last column (at column index: ncol(population)+1)
tournament_selection <- function(population, num_partitions=floor(nrow(population)/3)) {
  columns <- ncol(population) - 1
  rows <- nrow(population)
  
  #Adds fitness as last column to population matrix
  # combined_matrix <- cbind(population, fitness)
  combined_matrix <- shuffle_matrix(population)
  
  
  partitions <- matrix_partition(combined_matrix, num_partitions)
  selectedParents <- t(sapply(partitions, function(x) {
    x[which.min(x[,columns + 1]),]
  }))
  
  return(selectedParents[,1:(columns)])
}


matrix_partition <- function(matrix, num_partitions) {
  elements_per_partition <- floor(nrow(matrix)/num_partitions)
  lapply(1:num_partitions, function(partition_i) { 
    start_element <- ((partition_i-1)*elements_per_partition)+1
    end_element <- (partition_i)*elements_per_partition
    matrix(matrix[start_element:end_element,], nrow=elements_per_partition, ncol=ncol(matrix))
  })
}

shuffle_matrix <- function(matrix) {
  random <- sample(nrow(matrix))
  matrix[random,]
}


rank_selection(cbind(test_matrix, fitness))