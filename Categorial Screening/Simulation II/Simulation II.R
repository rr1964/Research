source("functions.R")
n = 200 
p = 5000

DCmat = matrix(NA, 500, 10)
MMmat = matrix(NA, 500, 10)
HUmat = matrix(NA, 500, 10)
CAmat = matrix(NA, 500, 10)

for (set in 1:500) {
  pr = runif(1, 0.05, 0.95)
  Y = sample(0:1, n, replace = TRUE, prob = c(pr, 1 - pr))
  Kai = matrix(c(0.35, 0.4, 0.4, 0.3, -0.213, 0.4, 0, 0.35, 0.45, 0.213, 
                 0.55, 0.5, 0.55, 0.6, 1.213, 0.5, 1, 0.65, 0.55, 0.787), nrow = 2, byrow = TRUE)
  X = matrix(1, n, p)
  for (i in 1:10) {
    tempi = rep(0, n)
    tempi[Y == 0] = rnorm(length(which(Y == 0)), 0, 1)
    tempi[Y == 1] = rnorm(length(which(Y == 1)), 1, 1)
    X[tempi < Kai[1, i], i] = 0
    X[tempi > Kai[2, i], i] = 2
  }
  for (i in 11:p) {
    pr = runif(1, 0.05, 0.95)
    X[, i] = rbinom(n, 2, pr)
  }
  data = cbind(Y, X)
  
  # compare results
  DCres = DCSIS(data)
  DCmat[set, ] = (p - rank(DCres) + 1)[1:10]
  MMres = MMLE(data)
  MMmat[set, ] = (p - rank(MMres) + 1)[1:10]
  HUres = Huang(data)
  HUmat[set, ] = (p - rank(HUres) + 1)[1:10]
  CAres = CATrend(data)
  CAmat[set, ] = (p - rank(CAres) + 1)[1:10]
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(DCmat, "DCmat", row.names = FALSE)
  write.csv(MMmat, "MMmat", row.names = FALSE)
  write.csv(HUmat, "HUmat", row.names = FALSE)
  write.csv(CAmat, "CAmat", row.names = FALSE) 
}
