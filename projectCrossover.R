### STAT243 Group Project
### Crossover
### Jonathan Ling

crossover <- function(parentsA, parentsB) {
  
  # Create vector of where to split each chromosome
  # Random selection
  splitLocation <- sample(1:(ncol(parentsA) - 1),
                          size = nrow(parentsA), replace = T)
  
  # Combine genes left of the split location from parent A with
  #   genes right of the split location from parent B
  breedSingle <- function(A, B, splitIndex) {
    child <- c(A[1:splitIndex], B[(splitIndex + 1):length(B)])
    return(child)
  }
  
  # Apply breedSingle to every pair of parents
  childMatrix <- matrix(rep(0, nrow(parentsA) * ncol(parentsA)),
                        nrow(parentsA), ncol(parentsA))
  for (i in 1:nrow(parentsA)) {
    childMatrix[i,] <- breedSingle(parentsA[i,], parentsB[i,], splitLocation[i])
  }
  
  return(childMatrix)
}

### Test case
# testMatA <- matrix(sample(c(0,1), 100, replace = T),10,10)
# testMatB <- matrix(sample(c(0,1), 100, replace = T),10,10)
# 
# testChild <- crossover(testMatA, testMatB)
