library(MASS)
library(boot)

# Zhang's paper
BICn = function(data) {
  #data = data.frame(cbind(Y, X))
  BIC(lm(Y ~ ., data = data))
}

zhang = function(data) {
  data = data.frame(scale(data))
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  d = 5
  candidateX = X
  
  ##print(candidateX)
  
  modelmat = data.frame(Y)
  select = c()
  main = c()
  candidate = as.character(1:p)
  inter = 0
  while(inter < 5) {
    BICvec= rep(0, dim(candidateX)[2])
    for(i in 1:dim(candidateX)[2]) 
      {
        print(i)
      print(dim(candidateX)[2])
        tempmat = data.frame(cbind(modelmat, candidateX[, i]))
        BICvec[i] = BICn(tempmat)
      }
    ns = candidate[which(BICvec == min(BICvec))]
    select = c(select, ns)
    modelmat = data.frame(cbind(modelmat, candidateX[, which(BICvec == min(BICvec))]))
    candidate = candidate[-which(BICvec == min(BICvec))]
    candidateX = candidateX[, -which(BICvec == min(BICvec))]
    if(length(grep("_", ns)) == 0) {
      if(length(main) > 0) {
        candidate = c(candidate, paste(ns, main, sep = "_"))
        candidateX = cbind(candidateX, modelmat[, dim(modelmat)[2]] * mainX)
        mainX = data.frame(cbind(mainX, modelmat[, dim(modelmat)[2]]))
      } else {
        mainX = data.frame(modelmat[, dim(modelmat)[2]])
      }
      main = c(main, ns)     
    } else {
      inter = inter + 1
    }
  }
  #print(select)
  select
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
