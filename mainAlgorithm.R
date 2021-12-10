library(dplyr)


### Initialize and Generate Genes

# Generate the genes and set the chromosomes:
create_population <- function(chromosome_length, population_size){
  n <- chromosome_length * population_size
  chromosome <- as.vector(sample(0:1, n, replace=TRUE))
  population <- as.data.frame(matrix(chromosome, nrow = population_size, ncol = chromosome_length))
  names(population) <- dput(paste0('gen_', seq(1,chromosome_length,1)))
  return(population)
}

### Calculate Fitness

# Function to identify the genes that would be active in each model given a chromosome:
find_genes <- function(chromosome, variables_names){
  variables <- variables_names[grep(1,as.vector(chromosome))]
  return(variables)
}

# Function to construct the formulas given the active genes of a chromosome:
set_formulas <- function(active_genes, name_y){
  formulas <- as.formula(paste(name_y, paste(active_genes, sep = "", collapse = " + "), sep = " ~ "))
  return(formulas)
}

# Function to compute the fitness (AIC) given a formula and a dataset
# By default, it will fit a linear regression. Although, it can receive the parameters 
# for a generalized linear model
fitness <- function(formula, data, ...){
  fitness <- AIC(glm(formula = formula, data = data, ...))
  return(fitness)
}

# Compute the fitness of an entire generation:
# The result is sort by the fittest individual to the least fit
get_fitness <- function(X, name_y, generation){
  data_names <- names(X)[!names(X) %in% c(name_y)]
  variables <- apply(generation, 1, find_genes, data_names)
  formulas <- lapply(variables, set_formulas, name_y)
  fitness <- lapply(formulas, fitness, X)
  return(unlist(fitness))
}

# Be careful, this returns your generation sorted by fitness
gather_fitness_generation <- function(generation,fitness_scores){
  gathered <- cbind(generation,fitness_scores) # %>%
    # arrange(desc(fitness_scores))
  return(gathered)
}

### Select Parents

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
  selected_parents <- t(sapply(partitions, function(x) {
    x[which.min(x[,columns + 1]),]
  }))
  
  return(selected_parents[,1:(columns)])
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

### Crossover
# Default crossover function; user can provide any crossover function that
#   takes in the two parent matrices and outputs the resulting child matrix.
crossover <- function(parents_A, parents_B, num_split = 1) {
  
  # Create vector of where to split each chromosome
  # Random selection
  split_location <- matrix(rep(0, num_split * nrow(parents_A)), 
                           nrow = nrow(parents_A), ncol = num_split)
  for (i in 1:nrow(parents_A)) {
    split_location[i,] <- sort(sample(1:(ncol(parents_A) - 1),
                                      size = num_split, replace = F))
  }
  split_location <- cbind(rep(0,nrow(parents_A)), split_location, 
                          rep(ncol(parents_A), nrow(parents_A)))
  
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
  child_matrix <- matrix(rep(0, nrow(parents_A) * ncol(parents_A)),
                         nrow(parents_A), ncol(parents_A))
  for (i in 1:nrow(parents_A)) {
    child_matrix[i,] <- breed_single(parents_A[i,], 
                                     parents_B[i,], 
                                     split_location[i,])
  }

  return(child_matrix)
}

### Mutate

mutate <- function(chromosomes, mutate_probability = 0.01) {
  # Inputs: 
  #   chromosomes: matrix of chromosomes
  #   mutate_probability: the probability for each gene to mutate. Default
  #                       is 1%.
  # Output:
  #   Matrix of mutated chromosomes
  
  # Mutate by subtracting 1 from the element and taking the absolute value
  #   Changes 0 to 1, changes 1 to 0
  # Generate mutation matrix from Bernoulli(mutate_probablity) distribution
  mutated_chromosomes <- abs(chromosomes - 
                            matrix(rbinom(nrow(chromosomes) * ncol(chromosomes),
                                            1, mutate_probability), 
                                    nrow = nrow(chromosomes),
                                    ncol = ncol(chromosomes))
  )
  return(mutated_chromosomes)
}



### Full algorithm

set.seed(123)

main_algorithm <- function(data,
                          chromosomes,
                          predictor,
                          num_partitions = floor(population_size/3),
                          genetic_operator = crossover,
                          mutate_probability = 0.01,
                          ...
                          ) {
  
  # Generate fitness scores and combine with chromosomes matrix
  fitness_scores <- get_fitness(data, predictor, chromosomes)
  
  my_generation_info <- gather_fitness_generation(chromosomes, fitness_scores)
  
  ## Create parents A and B
  parents_A <- tournament_selection(as.matrix(my_generation_info), num_partitions)
  # Run tournament selection multiple times to keep number of individuals the 
  # same over each generation
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parents_A <- rbind(parents_A,
      tournament_selection(as.matrix(my_generation_info), num_partitions))
  }
  
  parents_B <- tournament_selection(as.matrix(my_generation_info), num_partitions)
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parents_B <- rbind(parents_B,
                      tournament_selection(as.matrix(my_generation_info), 
                                           num_partitions))
  }
  
  child <- genetic_operator(parents_A, parents_B, ...)
  
  mutated <- mutate(child, mutate_probability)
  
  return(mutated)
}

### Test main_algorithm

# Create data
chromosome_length <- 10
population_size <- sample(chromosome_length:(2*chromosome_length),
                          1, replace=TRUE)
x <- as.data.frame(matrix(runif(100*(chromosome_length+1),0,1),
                          ncol=(chromosome_length+1),nrow=100))
names(x) <- letters[1:(chromosome_length+1)]

# Create first generation
chromosomes <- create_population(10,30)


main_algorithm(chromosomes, data = x, 
              predictor = "a", num_partitions = 15, 
              genetic_operator = crossover, mutate_probability = 0.05, 
              num_split = 2)


### Iterated Algorithm

loop_algorithm <- function(num_iterations,
                          data, 
                          chromosome_length = ncol(data), 
                          population_size = sample(chromosome_length:(2*chromosome_length), 1, replace=TRUE),
                          predictor,
                          num_partitions = floor(population_size/3),
                          genetic_operator = crossover,
                          mutate_probability = 0.01, ...) {
  
  # Create first generation
  chromosomes <- create_population(chromosome_length, population_size)
  
  # Repeat the algorithm
  for (i in 1:num_iterations) {
    chromosomes <- main_algorithm(data, chromosomes, predictor, num_partitions, 
                                 genetic_operator, mutate_probability, ...)
  }
  
  # Return a list with the best individual, their score, and full matrix
  return(chromosomes)
}

# Test loop_algorithm
final <- loop_algorithm(num_iterations = 10, chromosome_length = 10, 
                       population_size = 30, data = x, predictor = "a", 
                       num_partitions = 15, genetic_operator = crossover, 
                       mutate_probability = 0.05)
scoresTest <- get_fitness(x, "a", final)
final2 <- cbind(final, scoresTest)
final3 <- final2[order(final2[,ncol(final2)], decreasing=FALSE), ]

# Check fitness score
 AIC(lm(a ~ c+f+i, data = x )) 


