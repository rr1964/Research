source("functions.R")
n = 200
p = 1000

set.seed(7919)
for (set in 1:100) {
  X = matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) {
    X[, i] = sample(0:1, n, replace = TRUE)
  }
  Y = X[, 1] * X[, 2] # + X[, 3] * X[, 4] ###A second additive interaction term 
  data = cbind(Y, X)
  
  Rmat = R3(data)
  Hmat = huang(data)
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Rmat, paste("Rmat", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Hmat, paste("Hmat", set, ".csv", sep = ""), row.names = FALSE)
}
