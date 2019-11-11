####
### Model Free Screening of ZLLZ.

pig = c(1,2,3,4,5)
hog = rep(rpois(5,1))

indicator <- function(lower, upper) ###As in I(lower < upper)
{
  comps = ifelse(lower < upper, 1,0)
  
  return(prod(comps))
}


indicator(hog, pig)



zllz <-function(data, q)
{
  
  Y = data[, 1:q]
  X = data[, -(1:q)]
  X = scale(X) ###They assume that the data is standardized. 
  
  n = dim(X)[1]
  p = dim(X)[2] ###we end up not even needing this.
  
  omegaHats = apply(X, MARGIN = 2, omegaCalc_jointInd, sampSize = n, dataY = Y, q = q)
  
  return(omegaHats)
  
}

omegaCalc_jointInd <-function(X_j, dataY, sampSize, q) ##dataY = Y, sampSize = n. Wrappers.
{
  sumMat = matrix(0, nrow = sampSize, ncol = sampSize)
  
  
  colSumsSqrd = rep(0, sampSize)
  
  
  for(k in 1:sampSize)
  {
    for(i in 1:sampSize)
    {
      if(q>1)
      {sumMat[i,k] = X_j[i]*indicator(dataY[i,], dataY[k,])}
      else
      {sumMat[i,k] = X_j[i]*indicator(dataY[i], dataY[k])}
      
    }
    
    colSumsSqrd[k] = sum(sumMat[,k])^2
  }
    
  omegaHat_j = sum(colSumsSqrd)/(sampSize^3)
  
  return(omegaHat_j)
}


omegaCalc_marginalInd <-function(X_j, dataY, sampSize, q) ##dataY = Y, sampSize = n. Wrappers.
{
  sumArray = array(0, dim = rep(sampSize,4))
  
  colSumsSqrd = array(0, dim = rep(sampSize,3))
  
  for(k_1 in 1:sampSize)
    for(k_2 in 1:sampSize)
      for(k_3 in 1:sampSize)
      {
        
        compVector = c(dataY[k_1, 1], dataY[k_2, 2], dataY[k_3, 3])
        
        for(i in 1:sampSize)
        {
          
          sumArray[i,k_1, k_2, k_3] = X_j[i]*indicator(dataY[i,], compVector)
          
        }
        
        colSumsSqrd[k_1, k_2, k_3] = sum(sumArray[,k_1, k_2, k_3])^2
    }
    
    omegaHat_j = sum(colSumsSqrd)/(sampSize^3)
    
    return(omegaHat_j)
}

n = 50
p = 200
q = 4

X = matrix(NA, nrow = n, ncol = p)
for (i in 1:p) 
{
 # X[, i] = rnorm(n, 0, 5)
  X[, i] = rnorm(n, 0, 1)
}

Y = matrix(0, nrow = n, ncol = q)

Y[,1] = X[, 1] -X[,3] + 3*X[,5]
Y[,2] = 2*X[, 1] +2*X[,3] + X[,5]
Y[,3] = 3*X[, 1] +0* X[,3] - X[,5]
Y[,4] = -X[, 1] -2.5*X[,3] +X[,4] #+X[,5] 

Y = scale(Y)

#q = 1
#Y = X[, 1] +X[,3] - X[,5]

testData = cbind(Y, X)

#testData = matrix(rnorm(300, 0,1), ncol = 30, nrow = 10)
omega = zllz(data = testData, q = q)

head(omega)
sorted_omega = sort.int(omega, decreasing  = TRUE, index.return = TRUE)
head(sorted_omega$ix)

