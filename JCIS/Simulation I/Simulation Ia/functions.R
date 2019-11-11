library(plyr)
library(boot)

### Huang's paper
prob3 = function(Y, X1, X2) {
  n = length(Y)
  sum = 0
  temp = data.frame(cbind(Y, X1, X2))
  names(temp) = c('Y', 'X1', 'X2')
  ctab = count(temp, c('Y', 'X1', 'X2'))
  #ctab1 = count(cbind(Y, X1), c('Y', 'X1'))
  #ctab2 = count(cbind(Y, X2), c('Y', 'X2'))
  for (k in 1:dim(ctab)[1]) {
    k1 = sum(ctab[(ctab[, 1] == ctab[k, 1] & ctab[, 2] == ctab[k, 2]), 4])
    k2 = sum(ctab[(ctab[, 1] == ctab[k, 1] & ctab[, 3] == ctab[k, 3]), 4])
    sum = sum + (k1 * k2 / n ^ 2 - ctab[k, 4] / n) ^ 2 / (k1 * k2 / n ^ 2)  
  }
  sum
}
#system.time(prob3(Y, X1, X2))

huang = function(data) {
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  Hmat = matrix(0, nrow = p, ncol = p)
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      Hmat[i, j] = prob3(Y, X[, i], X[, j])
    }
  }
  Hmat
}


### formula 3
cum2 = function(a, b = a) {
  n = length(a)
  sum((a - mean(a)) * (b - mean(b))) / n
}

R3 = function(data) {
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  Rmat = matrix(0, nrow = p, ncol = p)
  K2vec = rep(0, p)
  K2y = cum2(Y)
  for (i in 1:p) {
    K2vec[i] = cum2(X[, i])
  }
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      Rmat[i, j] = cum3(Y, X[, i], X[, j], unbiased = FALSE) / sqrt(K2y * K2vec[i] * K2vec[j])
    }
  }
  Rmat
}

