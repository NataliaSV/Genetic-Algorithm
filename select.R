library(dplyr)

source("Functions.R")
source("parentSelection.R")
source("projectCrossover.R")
source("projectMutation.R")

### Full algorithm

main_algorithm <- function(data,
                          chromosomes,
                          predictor,
                          num_partitions = floor(population_size/3),
                          genetic_operator = crossover,
                          mutate_probability = 0.01,
                          FUN = AIC,
                          minimize = TRUE,
                          num_split = 1,
                          ...
                          ) {
  
  # Generate fitness scores and combine with chromosomes matrix
  fitness_scores <- get_fitness(data = data, 
                                name_y = predictor, 
                                generation = chromosomes, 
                                FUN = FUN, 
                                minimize = TRUE, 
                                ...)
  
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
  
  child <- genetic_operator(parents_A, parents_B, num_split = 1)
  
  mutated <- mutate(child, mutate_probability)
  
  return(mutated)
}


### Test main_algorithm
set.seed(123)

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
              genetic_operator = crossover, num_split = 3, 
              mutate_probability = 0.05)


# running with bic
main_algorithm(chromosomes, data = x, 
               predictor = "a", num_partitions = 15, 
               genetic_operator = crossover, mutate_probability = 0.05,
               FUN = BIC)

# Running a glm with BIC
main_algorithm(chromosomes, data = round(x,0), 
              predictor = "a", num_partitions = 15, mutate_probability = 0.05,
              genetic_operator = crossover, FUN = BIC,
              family = binomial)


### Iterated Algorithm

select <- function(num_iterations,
                          data, 
                          chromosome_length = ncol(data), 
                          population_size = sample(chromosome_length:(2*chromosome_length), 1, replace=TRUE),
                          predictor,
                          num_partitions = floor(population_size/3),
                          genetic_operator = crossover,
                          mutate_probability = 0.01,
                          FUN = AIC,
                          minimize = TRUE,
                          num_split = 1,
                          ...) {

  
  # Create first generation
  chromosomes <- create_population(chromosome_length, population_size)
  
  # Repeat the algorithm
  for (i in 1:num_iterations) {
    chromosomes <- main_algorithm(data = data, 
                                  chromosomes = chromosomes, 
                                  predictor = predictor, 
                                  num_partitions = num_partitions, 
                                  genetic_operator = genetic_operator, 
                                  mutate_probability = mutate_probability, 
                                  FUN = FUN,
                                  minimize = minimize,
                                  num_split = num_split,
                                  ...)
  }
  

  
  scores_test <- get_fitness(x, predictor, chromosomes)
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



# Tests select
# Simple linear regression
final <- select(num_iterations = 10, 
                        chromosome_length = 10, 
                        population_size = 30, 
                        data = x, 
                        predictor = "a", 
                        num_partitions = 15, 
                        genetic_operator = crossover, 
                        num_split = 2,
                        mutate_probability = 0.05)
final

# Check fitness score

AIC(glm(a ~ f+h, data = x) )

final <- select(num_iterations = 10, 
                        chromosome_length = 10, 
                        population_size = 30, 
                        data = round(x,0), 
                        predictor = "a", 
                        num_partitions = 15, 
                        genetic_operator = crossover, 
                        num_split = 3,
                        mutate_probability = 0.05,
                        FUN = BIC, family = binomial)
final


BIC(glm(a~b+c+e+f+g+i+k, data = round(x,0), family = binomial))


