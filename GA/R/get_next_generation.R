get_next_generation <- function(data, chromosomes,
                                response,
                                FUN = AIC,
                                minimize = TRUE,
                                num_partitions = floor(population_size/3),
                                genetic_operator = crossover,
                                num_split = 1,
                                mutate_probability = 0.01,
                                parent_selection = 'tournament',
                                ...) {
  
  # Generate fitness scores and combine with chromosomes matrix
  fitness_scores <- get_fitness(data = data, 
                                name_y = response, 
                                generation = chromosomes, 
                                FUN = FUN, 
                                minimize = TRUE, 
                                ...)
  
  my_generation_info <- gather_fitness_generation(chromosomes, fitness_scores)
  
  ## Store the genes and fitness of the fittest parent
  fittest_parent <- my_generation_info[which.max(fitness_scores), 1:ncol(chromosomes)]
  highest_fitness <- my_generation_info[which.max(fitness_scores), ncol(chromosomes) + 1]
  
  ## Create parents A and B 
  if (parent_selection == 'tournament') {
    parents_A <- tournament_selection(as.matrix(my_generation_info), 
                                      num_partitions)
    # Run tournament selection multiple times to keep number of individuals the 
    # same over each generation
    for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
      parents_A <- rbind(parents_A,
        tournament_selection(as.matrix(my_generation_info), 
                             num_partitions))
    }
    
    parents_B <- tournament_selection(as.matrix(my_generation_info), 
                                      num_partitions)
    
    for(i in 1:floor(nrow(chromosomes) / num_partitions - 1)) {
      parents_B <- rbind(parents_B,
                        tournament_selection(as.matrix(my_generation_info), 
                                             num_partitions))
    }
  } else {
    parents_A <- rank_selection(as.matrix(my_generation_info))
    parents_B <- rank_selection(as.matrix(my_generation_info))
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
