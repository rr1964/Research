source("functions.R")
n = 100
p = 500

Zmat = matrix(NA, 100, 5)
for (set in 1:100) {
  rho = 0.01
  Sigma = diag(p)
  Sigma = rho ^ abs(row(Sigma) - col(Sigma)) 
  X = mvrnorm(n, rep(0, p), Sigma)
  Y = 3 * X[, 1] * X[, 3] + 3 * X[, 6] * X[, 10] + 1 * X[, 1] + 1 * X[, 3] + 1 * X[, 6] + 1 * X[, 10]
  data = data.frame(cbind(Y, X))
  
  zres = zhang(data)
  Zmat[set, ] = zres[grep("_", zres)]
  Rmat = R3(data)
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Rmat, paste("Rmat", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Zmat, "Zmat.csv", row.names = FALSE)
}




for(i in 1:5)
{
  timestamp()
  
}