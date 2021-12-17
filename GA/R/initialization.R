# create_population
# Generates a population composed by individuals with chromosomes
# chromosome_length: length of the chromosome of each individual in the population
# population_size: number of individuals in the population
# output: data frame with the population

create_population <- function(chromosome_length, population_size){
  n <- chromosome_length * population_size
  chromosome <- as.vector(sample(0:1, n, replace=TRUE))
  population <- as.data.frame(matrix(chromosome, nrow = population_size, ncol = chromosome_length))
  
  return(population)
}




#Given a chromosome, it detects the active genes on it and returns the name of the gene on the original dataset
#chromosome: chromosome to detect the active genes
#variable_names: name of the explanatory variables in the dataset
#returns: vector with the active genes in each individual

find_genes <- function(chromosome, variables_names){
  return(variables_names[grep(1,as.vector(chromosome))])
}


#Create the formulas to fit the model for each individual in the generation
#active_genes: character vector with the active genes in each individual
#name_y: string with the name of the variable the user is trying to estimate, target
#returns: object type 'formula' per individual

set_formulas <- function(active_genes, name_y){
  formulas <- as.formula(paste(name_y, paste(active_genes, sep = "", collapse = " + "), sep = " ~ "))
  
  return(formulas)
}

#formula: formula to fit to each individual
#data: dataset with all the variables of interest
#FUN: function to measure the fitness of an individual, default is AIC, it must take a glm() model as input
#minimize: depending on FUN, either minimize or not
#...: parameters for the glm model
#returns: numeric fitness computed to the individual
fitness_function <- function(formula, data, FUN = AIC, minimize = TRUE, ...) {
  model <- glm(formula = formula, data = data, ...)
  fitness_score <- FUN(model)
  
  # Lower AIC and BIC scores = fitter
  if (minimize) {
    return(-fitness_score)
  }
  # Other objective functions may have higher score = better
  else {
    return(fitness_score)
  }
}


#data: dataset with the covariates and the variable the user is trying to estimate
#name_y: name of the variable that the user is trying to estimate
#generation: dataframe containing the generation of individuals
#FUN: function to measure the fitness of an individual, default is AIC, it must take a glm() model as input
#minimize: depending on FUN, either minimize or not
#...: parameters for the glm model
#returns: numeric vector with the fitness of the generation

get_fitness <- function(data, name_y, generation, FUN = AIC, minimize = TRUE, ...) {
  data_names <- names(data)[!names(data) %in% c(name_y)]
  
  variables <- apply(generation, 1, find_genes, data_names)
  variables[lengths(variables) == 0L] <- 1
  
  formulas <- lapply(variables, set_formulas, name_y)
  fitness_scores <- lapply(formulas, fitness_function, data, FUN, minimize, ...)
  
  return(unlist(fitness_scores))
}

#generation: dataframe containing the generation of individuals
#fitness_scores: estimated fitness per individual
#returns: data frame with the information of the generation and its respective scores
gather_fitness_generation <- function(generation,fitness_scores){
  return(cbind(generation,fitness_scores))
}
