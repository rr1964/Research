# DC-SIS
dc = function(u, v, n){
  u = as.numeric(u)
  v = as.numeric(v)
  m.u = matrix(u, n, n, byrow = TRUE)
  m.v = matrix(v, n, n, byrow = TRUE)
  s.u = abs(m.u - t(m.u))
  s.v = abs(m.v - t(m.v))
  s1 = sum((s.u) * (s.v)) / (n * n)
  s2 = sum(s.u) * sum(s.v) / (n * n * n * n)
  s3 = sum(rowSums(s.u) * (s.v)) / (n * n * n)
  sqrt(abs(s1 + s2 - 2 * s3))
}

DCSIS = function(data) {
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  omega = rep(0, p)
  dcov.y = dc(Y, Y, n)
  for(i in 1:p){
    dcov.xy = dc(X[, i], Y, n)
    dcov.xx = dc(X[, i], X[, i], n)
    dcorr = dcov.xy / sqrt(dcov.xx * dcov.y)
    omega[i] = dcorr * dcorr      
  } 
  omega
}


# MMLE
MMLE = function(data) {
  Y = data[, 1]
  X = data[, -1]
  p = dim(X)[2]
  
  Beta_jHatVect = rep(0, p)
  for(i in 1:p) {
    Beta_jHatVect[i] = glm(Y ~ X[, i], family = "binomial")$coefficients[[2]]
  }
  abs(Beta_jHatVect)
}


# Huang et al
Huang = function(data) {
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  as.numeric(apply(X, MARGIN = 2, function(X_jColumn, y) 
    {return(1 / n * as.numeric(chisq.test(x = X_jColumn, y = Y)$statistic))}, y = Y))
}


# CA-Trend
cum2 = function(a, b = a, n) {
  sum((a - mean(a)) * (b - mean(b))) / n
}

CATrend = function(data) {
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  K2vec = rep(0, p)
  for (i in 1:p) {
    K2vec[i] = cum2(Y, X[, i], n) / sqrt(cum2(Y, n = n) * cum2(X[, i], n = n))
  }
  abs(K2vec)
}

#
# Test DCSIS(data) MMLE(data) Huang(data) CATrend(data)
#
