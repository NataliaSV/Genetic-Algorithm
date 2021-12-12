### STAT243 Group Project
### Select Function - Full Algorithm
### Jonathan Ling, Hans Bak Nielsen, Natalia Sarabia


library(dplyr)

# setwd(file.path('..','GA/'))

source("initialization.R")
source("parentSelection.R")
source("crossover.R")
source("mutation.R")

### Full algorithm

main_algorithm <- function(data,
                          chromosomes,
                          predictor,
                          FUN = AIC,
                          minimize = TRUE,
                          num_partitions = floor(population_size/3),
                          genetic_operator = crossover,
                          num_split = 1,
                          mutate_probability = 0.01,
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
  parents_A <- tournament_selection(as.matrix(my_generation_info), 
                                    num_partitions)
  # Run tournament selection multiple times to keep number of individuals the 
  # same over each generation
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parents_A <- rbind(parents_A,
      tournament_selection(as.matrix(my_generation_info), num_partitions))
  }
  
  parents_B <- tournament_selection(as.matrix(my_generation_info), 
                                    num_partitions)
  
  for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
    parents_B <- rbind(parents_B,
                      tournament_selection(as.matrix(my_generation_info), 
                                           num_partitions))
  }
  
  if (all.equal(genetic_operator, crossover)) {
    child <- genetic_operator(parents_A, parents_B, num_split = 1)
  }
  else{
    child <- genetic_operator(parents_A, parents_B)
  }
  
  mutated <- mutate(child, mutate_probability)
  
  return(mutated)
}


### Iterated Algorithm

#' @title select
#' @description Conducts variable selection for a linear model
#' @param num_iterations number of iterations the user expect the algorithm performs
#' @param data full data set 
#' @param chromosome_length number of chromosomes (variables) that the user wants to be included
#' @param population_size size of the generation/population
#' @param response string with the name of the response variable
#' @param num_partitions number of partitions in the selection step
#' @param genetic_opterator type of genetic operator the user wants to use
#' @param mutate_probability probability of mutation
#' @param FUN fitness function, default AIC, but it could be any function that receives a glm model as parameter
#' @param minimize depending on the FUN, if the user wants to minimize or maximize it
#' @param num_split number of splits in the crossover
#' @param ... other parameters for the glm function, for instance, family.
#' @return data frame with the population
#' @examples
#' chromosome_length <- 10
#' x <- as.data.frame(matrix(runif(100*(chromosome_length+1),0,1),
#' ncol=(chromosome_length+1),nrow=100))
#' names(x) <- letters[1:(chromosome_length+1)]
#'  select(num_iterations = 10, 
#'  chromosome_length = chromosome_length, 
#'  data = round(x,0), 
#'  response = "a", 
#'  num_partitions = 15, 
#'  genetic_operator = crossover, 
#'  num_split = 3,
#'  mutate_probability = 0.05,
#'  FUN = BIC, family = binomial)
#'  

select <- function(num_iterations,
                   data, 
                   chromosome_length = ncol(data), 
                   population_size = 2 * chromosome_length,
                   predictor,
                   FUN = AIC,
                   minimize = TRUE,
                   num_partitions = floor(population_size/3),
                   genetic_operator = crossover,
                   num_split = 1,
                   mutate_probability = 0.01,
                   ...) {
  
  # Create first generation
  chromosomes <- create_population(chromosome_length, population_size)
  
  # Repeat the algorithm
  for (i in 1:num_iterations) {
    chromosomes <- main_algorithm(data = data, 
                                  chromosomes = chromosomes, 
                                  predictor = predictor, 
                                  FUN = FUN,
                                  minimize = minimize,
                                  num_partitions = num_partitions, 
                                  genetic_operator = genetic_operator, 
                                  num_split = num_split,
                                  mutate_probability = mutate_probability, 
                                  ...)
  }
  
  scores_test <- get_fitness(data, 
                             predictor, 
                             chromosomes, 
                             FUN = FUN, 
                             minimize = minimize,
                             ...)
  chromosome_fitness_matrix <- cbind(chromosomes, scores_test)
  chromosome_fitness_matrix <- 
    as.data.frame(chromosome_fitness_matrix[order(chromosome_fitness_matrix[,ncol(chromosome_fitness_matrix)], decreasing=TRUE), ])
  names(chromosome_fitness_matrix) <- 
    c(names(data)[!names(data) %in% c(predictor)],"score")
    
  return(
    list(
      chromosomes = chromosome_fitness_matrix[, 1:ncol(chromosome_fitness_matrix)-1],
      chromosomes_and_fitness = chromosome_fitness_matrix,
      fitness_vec = chromosome_fitness_matrix[, ncol(chromosome_fitness_matrix)],
      best_individual = as.vector(chromosome_fitness_matrix[1, 1:ncol(chromosome_fitness_matrix)-1]),
      best_fitness = chromosome_fitness_matrix[1, ncol(chromosome_fitness_matrix)]
    )
  )
}

###################### 
### Tests select
set.seed(123)

# Create data
chromosome_length <- 10
population_size <- sample(chromosome_length:(2*chromosome_length),
                          1, replace=TRUE)
x <- as.data.frame(matrix(runif(100*(chromosome_length+1),0,1),
                          ncol=(chromosome_length+1),nrow=100))
names(x) <- letters[1:(chromosome_length+1)]

# Test 1 
#### Simple linear regression
# Using select()
final <- select(num_iterations = 10, 
                data = x, 
                chromosome_length = 10, 
                population_size = 30, 
                predictor = "a", 
                num_partitions = 15, 
                genetic_operator = crossover,
                mutate_probability = 0.05,
                num_split = 2)
final

# Using other functions from R
active <- find_genes(as.vector(unlist(final$best_individual)), 
                     names(x[2:length(x)]))
formula <- set_formulas(active, name_y = "a")

# We obtain the same result
abs(sum(AIC(glm(formula, data = x)), final$best_fitness)) < .Machine$double.eps

# Test 2 
#### GLM, family binomial, using BIC
# Using select()
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

# Using other functions from R
active <- find_genes(as.vector(unlist(final$best_individual)), names(x[2:length(x)]))
formula <- set_formulas(active, name_y = "a")

# We obtain the same result
abs(sum(BIC(glm(formula, data = round(x,0), family = binomial)), final$best_fitness)) < .Machine$double.eps




