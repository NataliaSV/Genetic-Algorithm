library("titanic")

source("select.R")

set.seed(11)

# We are using the titanic_train dataset from the titanic package.

# Convert the gender column to binary
gender <- rep(-1, nrow(titanic_train))
for (i in 1:nrow(titanic_train)) {
  if (titanic_train[i,5] == "male") {
    gender[i] <- 1
  }
  else if (titanic_train[i,5] == "female") {
    gender[i] <- 0
  }
  else{
    gender[i] <- NA
  }
}

# Add some unrelated columns that the algorithm should choose to leave out
unrelated_1 <- rnorm(nrow(titanic_train), 0, 100)
unrelated_2 <- rnorm(nrow(titanic_train), 0, 10) * 20


# Organize the numeric data into one matrix
titanic_data <- cbind(Survived = titanic_train[,2], ID = titanic_train[,1], 
                      Pclass = titanic_train[,3], gender, unrelated_1, 
                      titanic_train[,6:8], unrelated_2, 
                      fare = titanic_train[,10])

# Columns e and i are the unrelated data


# Get rid of rows with NA
na_sums <- rep(-1, nrow(titanic_data))
for (i in 1:nrow(titanic_data)) {
  na_sums[i] <- sum(is.na(titanic_data[i,]))
}

cleaned_data <- titanic_data[na_sums == 0,]
names(cleaned_data) <- c("a","b","c","d","e","f","g","h","i","j")

# Test our select() function

best_titanic <- select(data = cleaned_data,
                predictor = "a")
best_titanic

# We expect columns b (Passenger ID), e, and i (randomly generated numbers) to
#   be unrelated to the survival variable, so the best fit model should have 0s
#   in those columns.

# AIC(glm(a~c+d+f+g+h+j, data = cleaned_data))
