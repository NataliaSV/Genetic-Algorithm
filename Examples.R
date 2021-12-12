# Examples
set.seed(123)

##  Simple linear regression:
# Create data
rows <- 1000
columns <- 100
data <- as.data.frame(matrix( runif(rows * columns,0,1),
                             ncol = columns, nrow = rows))

chromosome_length <- columns - 1

final <- select(num_iterations = 30, 
                data = data, 
                chromosome_length = chromosome_length, 
                response = "V1", 
                num_partitions = 15, 
                genetic_operator = crossover,
                mutate_probability = 0.05,
                num_split = 2)

# Find the fittest individual:
final$best_individual

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
final <- select(num_iterations = 30, 
                data = data, 
                chromosome_length = chromosome_length, 
                response = "V1", 
                num_partitions = 15, 
                genetic_operator = crossover,
                mutate_probability = 0.05,
                num_split = 2,
                family = binomial)

# Find the fittest individual:
final$best_individual

