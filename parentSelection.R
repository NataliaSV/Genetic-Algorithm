### STAT243 Group Project
### Parent Selection
### Hans Bak
m <- 200
n <- 100
test_matrix <- round(matrix(runif(m*n), n, m))
fitness <- c(runif(n))

tournamentSelection <- function(population, fitness, num_partitions=floor(nrow(population)/3)) {
  columns <- ncol(population)
  rows <- nrow(population)

  #Adds fitness as last column to population matrix
  combined_matrix <- cbind(population, fitness)
  combined_matrix <- shuffle_matrix(combined_matrix)


  partitions <- matrix_partition(combined_matrix, num_partitions)
  t(sapply(partitions, function(x) {
    x[which.max(x[,columns+1]),]
  }))
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

tournamentSelection(test_matrix, fitness)