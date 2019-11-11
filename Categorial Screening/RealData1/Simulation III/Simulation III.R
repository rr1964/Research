source("functions.R")
n = 200 
p = 5000

DCmat = matrix(NA, 500, 5)
MMmat = matrix(NA, 500, 5)
HUmat = matrix(NA, 500, 5)
CAmat = matrix(NA, 500, 5)

for (set in 1:500) {
  Y = sample(0:1, n, 1 / 2)
  X = matrix(1, n, p)
  for (i in 1:p) {
    X[, i] = sample(0:2, n, replace = TRUE, prob = rep(1 / 3, 3))
  }
  coef = matrix(c(0, -5, 2, -6, 1, #-6, 0, 3, -1, 4, 
                  3, -3, 4, -4, 3, #-4, 1, 2, -2, 3,
                  5, -1, 6, -2, 5), #-2, 2, 1, -3, 2), 
                  nrow = 3, byrow = TRUE)
  Y = (X[, 1:5] == 0) %*% coef[1, ] + (X[, 1:5] == 1) %*% coef[2, ] + (X[, 1:5] == 2) %*% coef[3, ]
  prob = 1 / (1 + exp(-Y))
  Y = rep(0, n)
  Y[prob > 0.5] = 1
  data = cbind(Y, X)
  
  # compare results
  DCres = DCSIS(data)
  DCmat[set, ] = (p - rank(DCres) + 1)[1:5]
  MMres = MMLE(data)
  MMmat[set, ] = (p - rank(MMres) + 1)[1:5]
  HUres = Huang(data)
  HUmat[set, ] = (p - rank(HUres) + 1)[1:5]
  CAres = CATrend(data)
  CAmat[set, ] = (p - rank(CAres) + 1)[1:5]
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(DCmat, "DCmat", row.names = FALSE)
  write.csv(MMmat, "MMmat", row.names = FALSE)
  write.csv(HUmat, "HUmat", row.names = FALSE)
  write.csv(CAmat, "CAmat", row.names = FALSE)
}
