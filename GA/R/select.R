#' @import dplyr

#' @export 
#' @title select
#' @description Conducts variable selection for a linear model
#' @param data full data set 
#' @param response string with the name of the response variable
#' @param chromosome_length number of chromosomes (variables) that the user wants to be included
#' @param population_size size of the generation/population
#' @param num_partitions number of partitions in the selection step
#' @param genetic_opterator type of genetic operator the user wants to use
#' @param mutate_probability probability of mutation
#' @param FUN fitness function, default AIC, but it could be any function that receives a glm model as parameter
#' @param minimize depending on the FUN, if the user wants to minimize or maximize it
#' @param num_split number of splits in the crossover
#' @param stop_criterion stops the algorithm when the last 5 generations vary by less than this percentage
#' @param parent_selection which parent selection algorithm should be used. Can be: 'rank' or 'tournament'
#' @param ... other parameters for the glm function, for instance, family.
#' @return data frame with the population
#' @examples # Examples
#' library(GA)
#'set.seed(34)
#'
#'##  Simple linear regression:
#'# Create data
#'rows <- 1000
#'columns <- 100
#'data <- as.data.frame(matrix(runif(rows * columns,0,1),
#'                             ncol = columns, nrow = rows))
#'
#'chromosome_length <- columns - 1
#'
#'final <- select(data = data, 
#'                response = "V1", 
#'                chromosome_length = chromosome_length,
#'                num_partitions = 15, 
#'                mutate_probability = 0.01,
#'                num_split = 3,
#'                stop_criterion = 0.05,
#'                minimize = FALSE)
#'names(final)
#'# Find the fittest individual:
#'final$overall_best_individual
#'final$last_5_gen_fitness
#'final$overall_best_fitness
#'final$last_gen_best_fitness
#'final$fitness_vec
#'## GLM: binomial family:
#'# Create data:
#'rows <- 1000
#'columns <- 100
#'data <- as.data.frame(matrix( runif(rows*columns,0,1),
#'                              ncol = columns, nrow = rows))
#'head(data)
#'
#'# Convert the response in binary so we can use family = binomial:
#'data$V1 <- round(data$V1,0)
#'
#'# Run the Genetic algorithm:
#'final <- select(data = data, 
#'                response = "V1", 
#'                chromosome_length = chromosome_length, 
#'                num_partitions = 15, 
#'                mutate_probability = 0.02,
#'                num_split = 4,
#'                stop_criterion = 0.04,
#'                family = binomial)
#'
#'# Find the fittest individual:
#'final$overall_best_individual
select <- function(data, 
                   response,
                   chromosome_length = ncol(data) - 1, 
                   population_size = 2 * chromosome_length,
                   FUN = AIC,
                   minimize = TRUE,
                   num_partitions = floor(population_size/3),
                   genetic_operator = crossover,
                   num_split = 1,
                   mutate_probability = 0.01,
                   stop_criterion = 0.05,
                   parent_selection = 'tournament', ...) {
                     
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
    chromosomes_list <- get_next_generation(data = data, 
                                            response = response,
                                            chromosomes = chromosomes_list[[1]], 
                                            FUN = FUN,
                                            minimize = minimize,
                                            num_partitions = num_partitions, 
                                            genetic_operator = genetic_operator, 
                                            num_split = num_split,
                                            mutate_probability = mutate_probability, 
                                            parent_selection = parent_selection,
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
                             response, 
                             chromosomes_list[[1]], 
                             FUN = FUN, 
                             minimize = minimize,
                             ...)
  chromosome_fitness_matrix <- cbind(chromosomes_list[[1]], scores_test)
  
  # Order the matrix from most fit to least fit
  chromosome_fitness_matrix <- 
    as.data.frame(chromosome_fitness_matrix[order(chromosome_fitness_matrix[,ncol(chromosome_fitness_matrix)], decreasing=TRUE), ])
  names(chromosome_fitness_matrix) <- 
    c(names(data)[!names(data) %in% c(response)],"score")
  
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

