library(MASS)
source("functions.R")
n = 200 
p = 1000

DCmat = matrix(NA, 500, 10)
#MMmat = matrix(NA, 500, 5)
#HUmat = matrix(NA, 500, 5)
CAmat = matrix(NA, 500, 10)

rho = 0.2
Sigma = diag(p)
Sigma = rho ^ abs(row(Sigma) - col(Sigma))

for (set in 1:5) {
  #X = matrix(1, n, p)
  X = mvrnorm(n, rep(0, p), Sigma)
  #X[which(x < -0.67, arr.ind = TRUE)] = 0
  #X[which(x > 0.67, arr.ind = TRUE)] = 2
  coef = c(5, -5, 5.5, -6, 6, 4, 4.5, -5.5, 5, -4)
  Y = X[, 1:10] %*% coef
  #Y = rep(0, n)
  #Y[y > median(y)] = 1
  data = cbind(Y, X)
  
  # compare results
  DCres = DCSIS(data)
  DCmat[set, ] = (p - rank(DCres) + 1)[1:10]
  #MMres = MMLE(data)
  #MMmat[set, ] = (p - rank(MMres) + 1)[1:5]
  #HUres = Huang(data)
  #HUmat[set, ] = (p - rank(HUres) + 1)[1:5]
  CAres = CATrend(data)
  CAmat[set, ] = (p - rank(CAres) + 1)[1:10]
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(DCmat, "DCmat", row.names = FALSE)
  #write.csv(MMmat, "MMmat", row.names = FALSE)
  #write.csv(HUmat, "HUmat", row.names = FALSE)
  write.csv(CAmat, "CAmat", row.names = FALSE)
}
