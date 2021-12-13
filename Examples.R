source("select.R")

# Examples
set.seed(34)

##  Simple linear regression:
# Create data
rows <- 1000
columns <- 100
data <- as.data.frame(matrix( runif(rows * columns,0,1),
                             ncol = columns, nrow = rows))

chromosome_length <- columns - 1

final <- select(data = data, 
                chromosome_length = chromosome_length, 
                predictor = "V1", 
                num_partitions = 15, 
                genetic_operator = crossover,
                mutate_probability = 0.01,
                num_split = 3,
                stop_criterion = 0.05)

# Find the fittest individual:
final$overall_best_individual

## GLM: binomial family:
# Create data:
rows <- 1000
columns <- 100
data <- as.data.frame(matrix( runif(rows*columns,0,1),
                              ncol = columns, nrow = rows))
head(data)

# Convert the response in binary so we can use family = binomial:
data$V1 <- round(data$V1,0)

# Run the Genetic algorithm:
final <- select(data = data, 
                chromosome_length = chromosome_length, 
                predictor = "V1", 
                num_partitions = 15, 
                genetic_operator = crossover,
                mutate_probability = 0.02,
                num_split = 4,
                stop_criterion = 0.04,
                family = binomial)

# Find the fittest individual:
final$overall_best_individual

