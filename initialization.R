### STAT243 Group Project
### Initialization step: Last updated Dec 12, 2021
### Natalia Sarabia

library('assertthat')

#' @title create_population
#' @description Generates a population composed by individuals with chromosomes
#' @param chromosome_length length of the chromosome of each individual in the population
#' @param population_size number of individuals in the population
#' @return data frame with the population
#' @examples
#'  create_population(chromosome_length = 5, population_size = 10)
#'  

create_population <- function(chromosome_length, population_size){
  
  assert_that(chromosome_length > 0 & population_size > 0,
              msg="chromosome_length and population_size must be positive integer values")
  
  assert_that(is.count(chromosome_length),
              msg="chromosome_length must be a positive integer value")
  
  assert_that(is.count(population_size),
              msg="population_size must be a positive integer value")
  
  n <- chromosome_length * population_size
  chromosome <- as.vector(sample(0:1, n, replace=TRUE))
  population <- as.data.frame(matrix(chromosome, nrow = population_size, ncol = chromosome_length))
  # names(population) <- paste('c("', paste(paste0('gen_', seq(1,chromosome_length,1)),collapse='","'), '")', sep='')
  
  return(population)
}



#' @title find_genes
#' @description Given a chromosome, it detects the active genes on it and returns the name of the gene on the original dataset
#' @param chromosome chromosome to detect the active genes
#' @param variable_names name of the explanatory variables in the dataset
#' @return vector with the active genes in each individual
#' @examples
#'  find_genes(chromosome = c(1,0,1), varaible_names = c("a","b","c"))
#'  

find_genes <- function(chromosome, variables_names){
  
  assert_that(not_empty(chromosome) & not_empty(variables_names),
              msg="chromosome or variable_names is empty")
  
  assert_that(is.vector(chromosome) & (is.vector(variables_names)),
              msg="chromosome and variable_names must be a vector")
  
  assert_that(noNA(chromosome) & noNA(variables_names),
              msg="chromosome and variable_names cannot contain NAs")
  
  variables <- variables_names[grep(1,as.vector(chromosome))]
  
  return(variables)
}


#' @title set_formulas
#' @description Create the formulas to fit the model for each individual in the generation
#' @param active_genes character vector with the active genes in each individual
#' @param name_y string with the name of the variable the user is trying to estimate, target
#' @return object type 'formula' per individual
#' @examples
#'  set_formulas(active_genes = c("age","height","gender"), name_y = "weight")
#'  

set_formulas <- function(active_genes, name_y){
  
  assert_that(is.string(name_y),
              msg="the name of the target variable is not a string")
  
  formulas <- as.formula(paste(name_y, paste(active_genes, sep = "", collapse = " + "), sep = " ~ "))
  
  return(formulas)
}

#' @title fitness_function
#' @description Fitness function
#' @param formula formula to fit to each individual
#' @param data dataset with all the variables of interest
#' @param FUN function to measure the fitness of an individual, default is AIC, it must take a glm() model as input
#' @param minimize depending on FUN, either minimize or not
#' @param ... parameters for the glm model
#' @return numeric fitness computed to the individual
#' @examples
#'  data <-  matrix(runif(100), ncol=10, nrow = 10)
#'  fitness(formula = as.formula(y ~ x), data)
#'  
#'  Using the BIC function:
#   fitness(formula = as.formula(y ~ x), data, FUN = BIC)

fitness_function <- function(formula, data, FUN = AIC, minimize = TRUE, ...){
  
  assert_that(inherits(formula, "formula"),
              msg="the input is not an actual formula")
  
  assert_that(not_empty(data),
              msg = "data cannot be empty")
  
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


#' @title get_fitness
#' @description Compute the fitness of a generation
#' @param data dataset with the covariates and the variable the user is trying to estimate
#' @param name_y name of the variable that the user is trying to estimate
#' @param generation dataframe containing the generation of individuals
#' @param FUN function to measure the fitness of an individual, default is AIC, it must take a glm() model as input
#' @param minimize depending on FUN, either minimize or not
#' @param ... parameters for the glm model
#' #' @return numeric vector with the fitness of the generation
#' @examples
#'  data <-  matrix(runif(100), ncol=10, nrow = 10)
#'  names(data) <- c("var1","var2","var3","var4","var5","var6","var7","var8","var9","var10")
#'  name_y <- "var8"
#'  generation <- matrix(rbern(100), ncol=9, nrow = 10)
#'  
#   get_fitness(data, name_y, generation)

get_fitness <- function(data, name_y, generation, FUN = AIC, minimize = TRUE, ...){
  
  assert_that(ncol(as.data.frame(generation)) == (ncol(as.data.frame(data))-1),
              msg = "the number of chromosomes must be equal to the number of columns of your data - 1")
  
  assert_that(is.string(name_y),
              msg="the name of the target variable is not a string")
  
  assert_that(not_empty(generation),
              msg = "generation cannot be empty")
  
  assert_that(not_empty(data),
              msg = "data cannot be empty")
  
  data_names <- names(data)[!names(data) %in% c(name_y)]
  
  variables <- apply(generation, 1, find_genes, data_names)
  variables[lengths(variables) == 0L] <- 1
  
  formulas <- lapply(variables, set_formulas, name_y)
  fitness_scores <- lapply(formulas, fitness_function, data, FUN, minimize, ...)
  
  return(unlist(fitness_scores))
}

#' @title gather_fitness_generation
#' @description Concatenate the generation with its respective fitness
#' @param generation dataframe containing the generation of individuals
#' @param fitness_scores estimated fitness per individual
#' @return data frame with the information of the generation and its respective scores
#' @examples
#'  generation <- as.data.frame(matrix(rbern(100), ncol=9, nrow = 10))
#'  scores <- runif(10)
#   gather_fitness_generation(generation,fitness_scores)

gather_fitness_generation <- function(generation,fitness_scores){
  
  assert_that(not_empty(generation),
              msg = "generation cannot be empty")
  
  assert_that(not_empty(fitness_scores),
              msg = "fitness_scores cannot be empty")
  
  assert_that(is.numeric(fitness_scores),
              msg = "fitness_scores need to be numeric")
  
  gathered <- cbind(generation,fitness_scores)
  return(gathered)
}
