source("functions.R")
n = 20
p = 100


set.seed(7919)

Zmat = matrix(NA, 100, 5)
for (set in 1:100) {
  X = matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) {
    X[, i] = rnorm(n, 0, 2)
    #X[, i] = rnorm(n, 0, 1)
  }
  Y = X[, 1] * X[, 2]  + X[, 3] * X[, 4]
  #Y = X[, 1] * X[, 2]  #
  data = cbind(Y, X)
  
  zres = zhang(data)

  Zmat[set, ] = zres[grep("_", zres)]
  Rmat = R3(data)
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Rmat, paste("Rmat", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Zmat, "Zmat.csv", row.names = FALSE)
}
