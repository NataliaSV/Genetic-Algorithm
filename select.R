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
  
  ## Store the genes and fitness of the fittest parent
  fittest_parent <- my_generation_info[which.max(fitness_scores), 1:ncol(chromosomes)]
  highest_fitness <- my_generation_info[which.max(fitness_scores), ncol(chromosomes) + 1]
  
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
    child <- genetic_operator(parents_A, parents_B, num_split = num_split)
  }
  else{
    child <- genetic_operator(parents_A, parents_B)
  }
  
  mutated <- mutate(child, mutate_probability)
  
  return(list(mutated, highest_fitness, fittest_parent))
}


### Iterated Algorithm

#' @title select
#' @description Conducts variable selection for a linear model
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
#' @param stop_criterion stops the algorithm when the last 5 generations vary by less than this percentage
#' @param ... other parameters for the glm function, for instance, family.
#' @return data frame with the population
#' @examples
#' chromosome_length <- 10
#' x <- as.data.frame(matrix(runif(100*(chromosome_length+1),0,1),
#' ncol=(chromosome_length+1),nrow=100))
#' names(x) <- letters[1:(chromosome_length+1)]
#'  select(chromosome_length = chromosome_length, 
#'  data = round(x,0), 
#'  response = "a", 
#'  num_partitions = 15, 
#'  genetic_operator = crossover, 
#'  num_split = 3,
#'  mutate_probability = 0.05,
#'  stop_criterion = 0.03
#'  FUN = BIC, family = binomial)
#'  

select <- function(data, 
                   chromosome_length = ncol(data) - 1, 
                   population_size = 2 * chromosome_length,
                   predictor,
                   FUN = AIC,
                   minimize = TRUE,
                   num_partitions = floor(population_size/3),
                   genetic_operator = crossover,
                   num_split = 1,
                   mutate_probability = 0.01,
                   stop_criterion = 0.05,
                   ...) {
  # Keep track of number of iterations ran
  iteration_counter <- 0
  
  # Create first generation
  chromosomes_list <- list(create_population(chromosome_length, population_size),
                      -Inf,0)
  
  overall_best_fitness <- -Inf
  overall_best_individual <- NA
  previous_gen_fitness <- rep(-Inf,5)
  
  stop <- FALSE
  # Repeat the algorithm until fitness stops improving
  while (stop == FALSE) {
    # Perform the algorithm
    chromosomes_list <- main_algorithm(data = data, 
                                  chromosomes = chromosomes_list[[1]], 
                                  predictor = predictor, 
                                  FUN = FUN,
                                  minimize = minimize,
                                  num_partitions = num_partitions, 
                                  genetic_operator = genetic_operator, 
                                  num_split = num_split,
                                  mutate_probability = mutate_probability, 
                                  ...)
    
    # Keep track of overall best fitness and genes
    if (chromosomes_list[[2]] > overall_best_fitness) {
      overall_best_fitness <- chromosomes_list[[2]]
      overall_best_individual <- chromosomes_list[[3]]
    }
    
    # Keep track of the best fitness in the last 5 iterations
    previous_gen_fitness <- c(chromosomes_list[[2]], previous_gen_fitness[1:4])
    fitness_change <- max(previous_gen_fitness) - min(previous_gen_fitness)
    
    # If best fitness remains about the same for 5 generations, stop running
    if (!is.na(fitness_change)) {
      if (abs(fitness_change) < abs(stop_criterion * previous_gen_fitness[1])) {
        stop <- TRUE
      }
    }
    iteration_counter <- iteration_counter + 1
  }
  
  # Check fitness of the final generation
  scores_test <- get_fitness(data, 
                             predictor, 
                             chromosomes_list[[1]], 
                             FUN = FUN, 
                             minimize = minimize,
                             ...)
  chromosome_fitness_matrix <- cbind(chromosomes_list[[1]], scores_test)
  
  # Order the matrix from most fit to least fit
  chromosome_fitness_matrix <- 
    as.data.frame(chromosome_fitness_matrix[order(chromosome_fitness_matrix[,ncol(chromosome_fitness_matrix)], decreasing=TRUE), ])
  names(chromosome_fitness_matrix) <- 
    c(names(data)[!names(data) %in% c(predictor)],"score")
  
  # Find the best genes in the last generation
  last_gen_best_fitness <- chromosome_fitness_matrix[1, ncol(chromosome_fitness_matrix)]
  last_gen_best_individual <- as.vector(chromosome_fitness_matrix[1, 1:ncol(chromosome_fitness_matrix)-1])
  
  if (last_gen_best_fitness > overall_best_fitness) {
    overall_best_fitness <- last_gen_best_fitness
    overall_best_individual <- last_gen_best_individual
  }
  
  names(overall_best_individual) <- names(last_gen_best_individual)
  return(
    list(
      overall_best_individual = overall_best_individual,
      overall_best_fitness = overall_best_fitness,
      last_gen_best_individual = last_gen_best_individual,
      last_gen_best_fitness = last_gen_best_fitness,
      last_gen_chromosomes_and_fitness = chromosome_fitness_matrix,
      fitness_vec = chromosome_fitness_matrix[, ncol(chromosome_fitness_matrix)],
      last_5_gen_fitness = previous_gen_fitness,
      iteration_counter = iteration_counter
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
final <- select(data = x, 
                chromosome_length = 10, 
                population_size = 30, 
                predictor = "a", 
                num_partitions = 15, 
                genetic_operator = crossover,
                num_split = 4,
                mutate_probability = 0.03,
                stop_criterion = 0.05)
final

# Using other functions from R
# Find fitness of the overall best individual and last best individual
active_overall <- find_genes(as.vector(unlist(final$overall_best_individual)), 
                     names(x[2:length(x)]))
formula_overall <- set_formulas(active_overall, name_y = "a")

active_last <- find_genes(as.vector(unlist(final$last_gen_best_individual)), 
                     names(x[2:length(x)]))
formula_last <- set_formulas(active_last, name_y = "a")

# We obtain the same result for both
abs(sum(AIC(glm(formula_overall, data = x)), final$overall_best_fitness)) < .Machine$double.eps
abs(sum(AIC(glm(formula_last, data = x)), final$last_gen_best_fitness)) < .Machine$double.eps

# Test 2 
#### GLM, family binomial, using BIC
# Using select()
final <- select(chromosome_length = 10, 
                population_size = 30, 
                data = round(x,0), 
                predictor = "a", 
                num_partitions = 15, 
                genetic_operator = crossover, 
                num_split = 2,
                mutate_probability = 0.02,
                stop_criterion = 0.03,
                FUN = BIC, family = binomial)
final

# Using other functions from R
# Find fitness of the overall best individual and last best individual
active_overall <- find_genes(as.vector(unlist(final$overall_best_individual)), 
                             names(x[2:length(x)]))
formula_overall <- set_formulas(active_overall, name_y = "a")

active_last <- find_genes(as.vector(unlist(final$last_gen_best_individual)), 
                          names(x[2:length(x)]))
formula_last <- set_formulas(active_last, name_y = "a")

# We obtain the same result for both
abs(sum(BIC(glm(formula_overall, data = round(x,0), family = binomial)), final$overall_best_fitness)) < .Machine$double.eps
abs(sum(BIC(glm(formula_last, data = round(x,0), family = binomial)), final$last_gen_best_fitness)) < .Machine$double.eps



