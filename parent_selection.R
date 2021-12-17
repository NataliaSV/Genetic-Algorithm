### STAT243 Group Project
### Parent Selection
### Hans Bak Nielsen
library(assertthat)
m <- 50
n <- 100
test_matrix <- round(matrix(runif(m*n), n, m))
fitness <- c(runif(n))

get_selection_probability <- function(rank, population_size) {
  return((2*rank)/(population_size*(population_size+1)))
}

rank_selection <- function(population_with_fitness, minimize = TRUE) {
  assert_that(population_with_fitness[,1:ncol(population_with_fitness)-1] == 0 ||
              population_with_fitness[,1:ncol(population_with_fitness)-1] == 1 )

  sorted_population_with_fitness <- population_with_fitness[order(population_with_fitness[,ncol(population_with_fitness)], decreasing=!minimize), ]
  probability_vec <- sapply(1:nrow(population_with_fitness), function(rank){
    get_selection_probability(rank, nrow(population_with_fitness))
  })
  sorted_population_with_fitness[
    sample(1:nrow(population_with_fitness), size = nrow(population_with_fitness), prob = probability_vec),
    1:ncol(population_with_fitness)-1
  ]
}

### INPUT:
# population_with_fitness: a matrix containing the chromosomes 
# fitness: a vector of the corresponding fitness of the chromosomes
# num_partitions: number of partitions in the matrix. Defaults to 
#                 the number of chromosomes divided by 3
### OUTPUT:
# A matrix with containing the best chromosomes from each partition.
# The number of rows is equal to the number of partitions. Adds the 
# fitness to the matrix as the last column (at column index: ncol(population)+1)
tournament_selection <- function(population_with_fitness, num_partitions=floor(nrow(population_with_fitness)/3), minimize = TRUE) {
  columns <- ncol(population_with_fitness) - 1
  rows <- nrow(population_with_fitness)

  assert_that(population_with_fitness[,1:columns] == 0 ||
              population_with_fitness[,1:columns] == 1 )
  
  combined_matrix <- shuffle_matrix(population_with_fitness)
  
  partitions <- matrix_partition(combined_matrix, num_partitions)
  selectedParents <- t(sapply(partitions, function(x) {
    if (minimize) {
      x[which.min(x[,columns + 1]),]
    } else {
      x[which.max(x[,columns + 1]),]
    }
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