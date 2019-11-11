source("functions.R")
n = 200
p = 1000

for (set in 1:100) {
  Theta = matrix(c(c(0.3, 0.4, 0.5, 0.3), c(0.95, 0.9, 0.9, 0.95)), nrow = 2, byrow = TRUE)
  Y = sample(c(0, 1), n, replace = TRUE, prob = c(0.75, 0.25))
  X = matrix(0, nrow = n, ncol = p)
  for (i in 1:4) {
    prob = (Theta[, i])[Y + 1]
    X[, 2 * i - 1] = unlist(lapply(prob, function(x) sample(1:0, 1, prob = c(x, 1 - x))))
    prob2 = apply(cbind(prob, X[, 2 * i - 1]), 1, 
                  function(x) ifelse(x[2] == 0, 
                                     0.6 * ifelse(x[1] > 0.5, 1, 0) + 0.4 * ifelse(x[1] <= 0.5, 1, 0),
                                     0.95 * ifelse(x[1] > 0.5, 1, 0) + 0.05 * ifelse(x[1] <= 0.5, 1, 0)))
    X[, 2 * i] = unlist(lapply(prob2, function(x) sample(1:0, 1, prob = c(x, 1 - x))))
  }
  for (i in 9:p) {
    X[, i] = sample(1:0, n, replace = TRUE)
  }
  data = cbind(Y, X)
  
  Rmat = R3(data)
  Hmat = huang(data)
  
  write.csv(data, paste("Simu_data", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Rmat, paste("Rmat", set, ".csv", sep = ""), row.names = FALSE)
  write.csv(Hmat, paste("Hmat", set, ".csv", sep = ""), row.names = FALSE)
}
