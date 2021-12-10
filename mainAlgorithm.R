library(dplyr)


### Initialize and Generate Genes

# Generate the genes and set the chromosomes:
create_population <- function(chromosome_length, population_size){
  n <- chromosome_length * population_size
  chromosome <- as.vector(sample(0:1, n, replace=TRUE))
  population <- as.data.frame(matrix(chromosome, nrow = population_size, ncol = chromosome_length))
  names(population) <- paste('c("', paste(paste0('gen_', seq(1,chromosome_length,1)),collapse='","'), '")', sep='')
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
fitness <- function(formula, data, FUN = AIC, ...){
  
  model <- glm(formula = formula, data = data, ...)
  fitness <- FUN(model)
  
  return(fitness)
}

# Compute the fitness of an entire generation:
# The result is sort by the fittest individual to the least fit
get_fitness <- function(data, name_y, generation, FUN = AIC, ...){
  data_names <- names(data)[!names(data) %in% c(name_y)]
  variables <- apply(generation, 1, find_genes, data_names)
  
  variables[lengths(variables) == 0L] <- 1
  
  formulas <- lapply(variables, set_formulas, name_y)
  fitness <- lapply(formulas, fitness, data, FUN, ...)
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
tournamentSelection <- function(population, num_partitions=floor(nrow(population)/3)) {
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

### Crossover

crossover <- function(parentsA, parentsB) {
  
  # Create vector of where to split each chromosome
  # Random selection
  splitLocation <- sample(1:(ncol(parentsA) - 1),
                          size = nrow(parentsA), replace = T)
  
  # Combine genes left of the split location from parent A with
  #   genes right of the split location from parent B
  breedSingle <- function(A, B, splitIndex) {
    child <- c(A[1:splitIndex], B[(splitIndex + 1):length(B)])
    return(child)
  }
  
  # Apply breedSingle to every pair of parents
  childMatrix <- matrix(rep(0, nrow(parentsA) * ncol(parentsA)),
                        nrow(parentsA), ncol(parentsA))
  for (i in 1:nrow(parentsA)) {
    childMatrix[i,] <- breedSingle(parentsA[i,], parentsB[i,], splitLocation[i])
  }
  
  return(childMatrix)
}

### Mutate

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



### Full algorithm

set.seed(123)

mainAlgorithm <- function(data,
                          chromosomes,
                          predictor,
                          num_partitions = floor(population_size/3),
                          mutateProbability = 0.01,
                          FUN = AIC,
                          ...
                          ) {
  
  # Generate fitness scores and combine with chromosomes matrix
  fitness_scores <- get_fitness(data, predictor, chromosomes, FUN,...)
  
  my_generation_info <- gather_fitness_generation(chromosomes, fitness_scores)
  
  ## Create parents A and B
  parentsA <- tournamentSelection(as.matrix(my_generation_info), num_partitions)
  # Run tournament selection multiple times to keep number of individuals the 
  # same over each generation
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parentsA <- rbind(parentsA,
      tournamentSelection(as.matrix(my_generation_info), num_partitions))
  }
  
  parentsB <- tournamentSelection(as.matrix(my_generation_info), num_partitions)
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parentsB <- rbind(parentsB,
                      tournamentSelection(as.matrix(my_generation_info), num_partitions))
  }
  
  child <- crossover(parentsA, parentsB)
  
  mutated <- mutate(child, mutateProbability)
  
  return(mutated)
}

### Test mainAlgorithm

# Create data
chromosome_length <- 10
population_size <- sample(chromosome_length:(2*chromosome_length), 1, replace=TRUE)
x <- as.data.frame(matrix(runif(100*(chromosome_length+1),0,1),ncol=(chromosome_length+1),nrow=100))
names(x) <- letters[1:(chromosome_length+1)]

# Create first generation
chromosomes <- create_population(10,30)

# running the default statistic
mainAlgorithm(chromosomes, data = x, 
              predictor = "a", num_partitions = 15, mutateProbability = 0.05)

# running with bic
mainAlgorithm(chromosomes, data = x, 
              predictor = "a", num_partitions = 15, mutateProbability = 0.05,
              FUN = BIC)

# Running a glm with BIC
mainAlgorithm(chromosomes, data = round(x,0), 
              predictor = "a", num_partitions = 15, mutateProbability = 0.05,
              FUN = BIC,
              family = binomial)


### Iterated Algorithm

loopAlgorithm <- function(num_iterations,
                          data, 
                          chromosome_length = ncol(data), 
                          population_size = sample(chromosome_length:(2*chromosome_length), 1, replace=TRUE),
                          predictor,
                          num_partitions = floor(population_size/3),
                          mutateProbability = 0.01,
                          FUN = AIC,
                          ...) {
  
  # Create first generation
  chromosomes <- create_population(chromosome_length, population_size)
  
  # Repeat the algorithm
  for (i in 1:num_iterations) {
    chromosomes <- mainAlgorithm(data, chromosomes, predictor, num_partitions, mutateProbability, FUN,...)
  }
  
  scores_test <- get_fitness(x, "a", chromosomes)
  chromosome_fitness_matrix <- cbind(chromosomes, scores_test)
  chromosome_fitness_matrix <- chromosome_fitness_matrix[order(chromosome_fitness_matrix[,ncol(chromosome_fitness_matrix)], decreasing=TRUE), ]

  return(
    list(
      chromosomes = chromosome_fitness_matrix[, 1:ncol(chromosome_fitness_matrix)-1],
      chromosomes_and_fitness = chromosome_fitness_matrix,
      fitness_vec = chromosome_fitness_matrix[, ncol(chromosome_fitness_matrix)],
      best_individual = chromosome_fitness_matrix[1, 1:ncol(chromosome_fitness_matrix)-1],
      best_fitness = chromosome_fitness_matrix[1, ncol(chromosome_fitness_matrix)]
    )
  )
}

# Tests loopAlgorithm
# Simple linear regression
final <- loopAlgorithm(num_iterations = 10, chromosome_length = 10, population_size = 30, data = x, 
              predictor = "a", num_partitions = 15, mutateProbability = 0.05)
final
# Check fitness score

BIC(glm(a ~ h, data = round(x,0), family = binomial )) 

