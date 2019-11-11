###### Generalized Joint Cumulant Screening. 
####### Coding for Paper 3. 
#####
library(boot)

### Biased SD
cum2 = function(a, b = a) {
  n = length(a)
  sum((a - mean(a)) * (b - mean(b))) / n
}

indicator <- function(lower, upper) ###As in I(lower < upper)
{
  comps = ifelse(lower < upper, 1,0)
  
  return(prod(comps))
}

isAllZero <- function(testThisVect)
{
  nonZero = (testThisVect != 0)
  if(sum(nonZero) > 0)
  {
    return(FALSE)
  }
  else
  {return(TRUE)}
}

###There is a built in norm function, but it is slow. Used SVD. Ours is UBE. 
norm_vec <- function(x){sqrt(sum(x^2))}


#########################################################################################
#########################################################################################
## ZLLZ
#########################################################################################
#########################################################################################


zllz <-function(data, q)
{
  
  Y = data[, 1:q]
  X = data[, -(1:q)]
  X = scale(X) ###They assume that the data is standardized. 
  
  n = dim(X)[1]
  p = dim(X)[2] 
  
  omegaHats = apply(X, MARGIN = 2, omegaCalc_jointInd, sampSize = n, dataY = Y, q = q)
  
  if(isAllZero(omegaHats))
  {
    ####return(1:p)####A cheap way of forcing the scores to be heavily penalized when we get all zeros. 
    ###This is too heavy of a penalty perhaps. 
    
    randomRanks = sample(1:p)
    print("All omegas were zero")
    #print(paste("Returning ", 3001-randomRanks[1], 3001-randomRanks[2], 3001-randomRanks[3]))
    ###This just makes it some that randomRanks actually has the ranks.
    
    return(randomRanks)#####The simulation will take care of sorting these. 
  } 
  ####A cheap way of forcing the scores to be heavily penalized when we get all zeros. 
  
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

#########################################################################################
#########################################################################################
### DC-SIS
#########################################################################################
#########################################################################################


DCSIS_MV<-function(data, q)
{
  
  Y = data[, 1:q]
  X = data[, -(1:q)]
  
  
  n = dim(X)[1]
  p = dim(X)[2] 
  dc = rep(0,p)
  
  dcov.y = DistanceCov(Y,Y, n = n)
  
  for(j in 1:p)
  {
    X.j = X[, j]
    dcov.xy = DistanceCov(X.j, Y, n = n)
    dcov.xx = DistanceCov(X.j, X.j, n = n)
    dcorr = dcov.xy / sqrt(dcov.xx * dcov.y)
    dc[j] = dcorr * dcorr      
  } 
  
  return(dc)
  
}

DistanceCov <- function(u, v, n)
{
  u = as.matrix(as.numeric(u))###cast vectors into 1 by n column matrix.  
  v = as.matrix(as.numeric(v))###cast vectors into 1 by n column matrix. 
  
  u_q = ncol(u)
  v_q = ncol(v)
  
  diff_uArr = array(0,dim = c(n,n, u_q))
  diff_vArr = array(0,dim = c(n,n, v_q))
  
  for(m in 1:u_q)
  {
    ###Make an (n by n)  matrix whose rows consist of repeated instances of Y^(m). 
    uArr= matrix(u[,m], nrow = n, ncol = n, byrow = TRUE)
    
    ###@Now take the difference array[,,m] - t(array[,,])
    diff_uArr[,,m] = uArr - t(uArr)
    
  }
  
  for(m in 1:v_q)
  {
    ###Make an (n by n)  matrix whose rows consist of repeated instances of Y^(m). 
    vArr= matrix(v[,m], nrow = n, ncol = n, byrow = TRUE)
    
    ###@Now take the difference array[,,m] - t(array[,,])
    diff_vArr[,,m] = vArr - t(vArr)
    
  }
  
  
  #m.u = matrix(u, n, n, byrow = TRUE)
  #m.v = matrix(v, n, n, byrow = TRUE)
  s.u = apply(diff_uArr, MARGIN = c(1,2), norm_vec)
  s.v = apply(diff_vArr, MARGIN = c(1,2), norm_vec)
  
  
  s1 = sum((s.u) * (s.v)) / (n^2)
  s2 = sum(s.u) * sum(s.v) / (n^4)
  s3 = sum(rowSums(s.u) * (s.v)) / (n^3)
  
  dcov <- sqrt(abs(s1 + s2 -2 * s3))
  return(dcov)
}

#########################################################################################
#########################################################################################
### CovarProd (small phi). The cheap phi method. 
#########################################################################################
#########################################################################################

CovarProd_MV_main <- function(data, q)
{ #####This is the function for q many components 
  Y = data[, 1:q]
  X = data[, -(1:q)]
  n = dim(X)[1]
  p = dim(X)[2]
  phiVect = rep(0,p)
  covProdVect = rep(0,p)
  
  
  K2vec = rep(0, p)
  #K2yTEST = rep(0, q)
  ##Create a vector of the estimated SDs for each component of Y. 
  
  if(q>1)
  {
    K2y = apply(Y, MARGIN =2, cum2) 
  }
  else
  {
    K2y = cum2(Y)
  }
  
  
  
  ######################################
  
  ###Get the SD for each covariate. 
  for (j in 1:p) {
    K2vec[j] = cum2(X[, j])
  }
  
  
  for (j in 1:p) 
  {
    
    
    for(m in 1:q)
    {
      covProdVect[j] = prod(apply(Y, MARGIN = 2, cum2, a = X[,j]))
    }
    #print(length(prod_Ycentered))
    #print(length(X[,j] - mean(X[,j])))
    phiVect[j] =  covProdVect[j]/sqrt(K2vec[j]^q*prod(K2y))
    
  }
  
  return(abs(phiVect))
}

#########################################################################################
#########################################################################################
### Correlation Matrix. Generalized correlation. "Big" phi. 
#########################################################################################
#########################################################################################

CorrMat_MV_main = function(data, q, normToUse = 1)
{ #####This is the function for q many components 
  Y = data[, 1:q]
  X = data[, -(1:q)]
  n = dim(X)[1]
  p = dim(X)[2]
  phiVect = rep(0,p)
  
  
  for (j in 1:p) 
  {
    
    
    
    covMat = abs(cov(cbind(X[,j], Y)))
    #print(covMat)
    ##sdMat = (1/sqrt(diag(covMat)))
    
    ###if(!(j %% 1000)){print(covMat)}
    
    #print(sdMat %*% covMat %*% sdMat)
    #print(cor(cbind(X[,j], Y)))
    #print(sdMat)
    
    #phiVect[j] = det(sdMat %*% covMat %*% sdMat)
    
    if(normToUse == 1)
    {
      sdVec = (1/sqrt(diag(covMat)))
      
      phiVect[j] = sdVec %*% covMat %*% sdVec
    }
    else if(normToUse == 2)
    {
      ##print("Using the Frobenius norm")
      
      sdMat = diag(1/sqrt(diag(covMat)))
      
      phiVect[j] = norm_vec(sdMat %*% covMat %*% sdMat)
      
    }
    
    
    #print(sdMat %*% covMat %*% sdMat)
    
  }
  
  return(abs(phiVect))
}

#########################################################################################
#########################################################################################
### Interact Interact. Generalized correlation WITH interactions. "Super" phi. INTERACT
#########################################################################################
#########################################################################################

CorrMat_MV_Interact = function(data, q, normToUse = 1)
{ #####This is the function for q many components 
  Y = data[, 1:q]
  X = data[, -(1:q)]
  n = dim(X)[1]
  p = dim(X)[2]
  PhiMat = matrix(0, nrow = p, ncol = p) ###This can be trimmed to (p-1) by (p-1) if need be. 
  ##We could even start recording only the combinations that matter. The long form like in Real Data, Paper 2. 
  
  
  for (j_1 in 1:(p - 1)) 
  {
    for (j_2 in (j_1 + 1):p)
    {
      ###This is the covariance matrix with the 3 way joint cumulant entries padding the first row/col
      fullMat = matrix(0, nrow = q+1, ncol = q+1) 
      
      ##This is approach number 1 for the first row/col.
      ##firstRowCol = c(cov(X[,j_1],X[,j_2]), apply(Y, MARGIN = 2,cum3, b = X[,j_1], c = X[,j_2], unbiased = FALSE))
      
      ## This is approach number 2 for the first row/col.
      firstRowCol = c(var(X[,j_1])*var(X[,j_2]), apply(Y, MARGIN = 2,cum3, b = X[,j_1], c = X[,j_2], unbiased = FALSE))
      
      ###covYMat = cov(Y)
      
      fullMat[2:(q+1), 2:(q+1)] = cov(Y)
      fullMat[1,] = firstRowCol
      fullMat[,1] = firstRowCol ###Overwrite on [1,1], but that matters very little here. UBE. 
      
      fullMat = abs(fullMat) ##We only care about the magnitude of the relationship, not the directions of the relationships. 
      
      
      
      ###if(!(j %% 1000)){print(covMat)}
      
      if(normToUse == 1)
      {
        sdMat = 1/sqrt(diag(fullMat))
        PhiMat[j_1, j_2] = (sdMat %*% fullMat %*% sdMat)
      }
      
      else if(normToUse == 2)
      {
        ##print("Using the Frobenius norm")
        
        sdMat = diag(1/sqrt(diag(fullMat)))
        
        PhiMat[j_1, j_2] = norm_vec(sdMat %*% fullMat %*% sdMat)
        
      }
      
      
    }
  }
  
  
  
  return(abs(PhiMat))
}

### *****************************************************************************************

### *****************************************************************************************

### *****************************************************************************************

### *****************************************************************************************

### *****************************************************************************************
CorrMat_MV_Interact_WithinGroup <- function(data, q, normToUse = 2, names)
{ #####This is the function for q many components 
  ## WithinGroup is used to indicate that the data is 
  ##          (Y,X) and we only need to look at one way pairs (i.e. groups 1,2,3,4)
  ##          on the diagonal. 
  Y = data[, 1:q]
  X = data[, -(1:q)]
  n = dim(X)[1]
  p = dim(X)[2]
  
  rowNum = 1
  eighty_fifth10k = 0 ##Initialize as 0. 
  
  PhiLONGmat = matrix(0, nrow = choose(p,2), ncol = 3)
  
  ##print(dimnames(Rmat))
  
  
  colnames(PhiLONGmat) = c("SNP_1", "SNP_2", "Phi")
  
  K2y = rep(0, q)
  K2vec = rep(0, p)
  
  ##We could even start recording only the combinations that matter. The long form like in Real Data, Paper 2. 
  for(m in 1:q)
  {
    K2y[m] = cum2(Y[,m])
  }
  
  for (j in 1:p) 
  {
    K2vec[j] = cum2(X[, j])
  }
  
  
  for (j_1 in 1:(p - 1)) 
  {
    for (j_2 in (j_1 + 1):p)
    {
      
      if(rowNum == 10001)#Take the 85th quantile and above. 
      {
        eighty_fifth10k = as.numeric(quantile(as.numeric(PhiLONGmat[1:10000,3]), probs = 0.85))
      }
      
      ## This is approach number 2 for the first row/col.
      firstRowCol = apply(Y, MARGIN = 2,cum3, b = X[,j_1], c = X[,j_2], unbiased = FALSE)
      
      normalizedFirstRowCol = firstRowCol/sqrt(K2y*K2vec[j_1]*K2vec[j_2])
      
      if(normToUse == 1)
      {
        Phi_j1_j2 = round(sum(abs(normalizedFirstRowCol)), 7)
        
        
        if(rowNum < 10001 | (rowNum > 10000 & Phi_j1_j2 > eighty_fifth10k))
        {
          #PhiLONGmat[rowNum,] = c(names[j_1], names[j_2], Phi_j1_j2)
          
          PhiLONGmat[rowNum,1:2] = c(names[j_1], names[j_2])
          PhiLONGmat[rowNum,3] = Phi_j1_j2
          
          rowNum = rowNum + 1
        }
      }
      
      else if(normToUse == 2)
      {
        Phi_j1_j2 = round(norm_vec(normalizedFirstRowCol), 7)
        ###We do not need to report 15 digits or whatever. 
        
        
        ###This is a simplified version. Only looks at the values that will differ. 
        
        ##record the first 10k. Then only record the value if it is above the 75th percentile.  
        if(rowNum < 10001 | Phi_j1_j2 > eighty_fifth10k)
        {
          #PhiLONGmat[rowNum,] = c(names[j_1], names[j_2], Phi_j1_j2)
          PhiLONGmat[rowNum,1:2] = c(names[j_1], names[j_2])
          PhiLONGmat[rowNum,3] = Phi_j1_j2
          rowNum = rowNum + 1
        }
        
      }
      
      
    }
  }
  
  
  
  return(PhiLONGmat[1:(rowNum),]) ###Check for tailing zeros. 
  #Remember that rowNum has already been incremented to one past the last recordable value. 
}



### *****************************************************************************************

### *****************************************************************************************
CorrMat_MV_Interact_SeparateGroup= function(data, q, normToUse = 2, namesX1, namesX2)
{ #####This is the function for q many components 
  ## separateGroup is used to indicate that the data is 
  ##          (Y,X1, X2) and we need to look at ALL pairs 
  
  
  X_1Size = length(namesX1)
  #n = dim(X_1)[1]
  Y = data[, 1:q]
  X_1 = data[,(q+1):(X_1Size + q)]
  X_2 = data[, -(1:(X_1Size + q))]
  
  # Y = Y
  # X_1 = as.matrix(X1)
  # X_2 = as.matrix(X2)
  # 
  # namesX1 =  colnames(X1)
  # namesX2 = colnames(X2)
  
  p_1 = dim(X_1)[2]
  p_2 = dim(X_2)[2]
  
  # if(p_1 != p_2)
  # {
  #   print("p_1 should have equaled p_2 in our case! 11107 is the group size.")
  # }
  
  rowNum = 1
  eighty_fifth10k = 0 ##Initialize as 0. 
  
  PhiLONGmat = matrix(0, nrow = max(p_1, p_2)^2, ncol = 3)
  
  ##print(dimnames(Rmat))
  
  
  colnames(PhiLONGmat) = c("SNP_1", "SNP_2", "Phi")
  
  K2y = rep(0, q)
  K2X1vec = rep(0, p_1)
  K2X2vec = rep(0, p_2)
  
  ##We could even start recording only the combinations that matter. 
  #The long form like in Real Data, Paper 2. 
  for(m in 1:q)
  {
    K2y[m] = cum2(Y[,m])
  }
  
  for (j in 1:p_1) 
  {
    #print("Made it to the first loop")
    K2X1vec[j] = cum2(X_1[,j])
  }
  for (j in 1:p_2) 
  {
    #print("Made it to the second loop")
    K2X2vec[j] = cum2(X_2[,j])
  }
  
  
  for (j_1 in 1:p_1) 
  {
    for (j_2 in 1:p_2)
    {
      
      if(rowNum == 10001)#Take the 85th quantile and above. 
      {
        print("Reached 10000th row.")
        #eighty_fifth10k = as.numeric(quantile(PhiLONGmat[1:10000,3], probs = 0.85))
        eighty_fifth10k = as.numeric(quantile(as.numeric(PhiLONGmat[1:10000,3]), 
                                              probs = 0.85, na.rm = TRUE))
      }
      
      ## This is approach number 2 for the first row/col.
      firstRowCol = apply(Y, MARGIN = 2,cum3, b = X_1[,j_1], c = X_2[,j_2], unbiased = FALSE)
      
      normalizedFirstRowCol = firstRowCol/sqrt(K2y*K2X1vec[j_1]*K2X2vec[j_2])
      
      if(normToUse == 1)
      {
        Phi_j1_j_2 = round(sum(abs(normalizedFirstRowCol)), 7)
        
        
        if(rowNum < 10001 | (rowNum > 10000 & Phi_j1_j2 > eighty_fifth10k))
        {
          PhiLONGmat[rowNum,1:2] = c(namesX1[j_1], namesX2[j_2])
          PhiLONGmat[rowNum,3] = Phi_j1_j2
          
          rowNum = rowNum + 1
        }
      }
      
      else if(normToUse == 2)
      {
        Phi_j1_j2 = round(norm_vec(normalizedFirstRowCol), 7)
        ###We do not need to report 15 digits or whatever. 
        
        
        ###This is a simplified version. Only looks at the values that will differ. 
        
        ##record the first 10k. Then only record the value if it is above the 85th percentile.  
        if(rowNum < 10001 | Phi_j1_j2 > eighty_fifth10k)
        {
          PhiLONGmat[rowNum,1:2] = c(namesX1[j_1], namesX2[j_2])
          PhiLONGmat[rowNum,3] = Phi_j1_j2
          rowNum = rowNum + 1
        }
        
        if((rowNum) %% 100000 == 0)
        {
          timestamp()
          print(paste("Benchmark", rowNum))
        }
        
        
      }
      
      
    }
  }
  
  
  
  return(PhiLONGmat[1:(rowNum),]) ###Check for tailing zeros. 
}#Remember that rowNum has already been incremented to one past the last recordable value.




#########################################################################################
#########################################################################################
### Generalized Joint cumulants. 
#########################################################################################
#########################################################################################

# gjc_MV_main = function(data, q)
# { #####This is the function for q many compone 
#   Y = data[, 1:q]
#   X = data[, -(1:q)]
#   n = dim(X)[1]
#   p = dim(X)[2]
#   phiVect = rep(0,p)
#   
#   K2vec = rep(0, p)
#   K2yTEST = rep(0, q)
#   
#   
#   
#   
#   ##Create a vector of the estimated SDs for each component of Y. 
#   
#   if(q>1)
#   {
#     K2y = apply(Y, MARGIN =2, cum2) 
#   }
#   else
#   {
#     K2y = cum2(Y)
#   }
#   
#   
#   Ycentered = scale(Y, scale = FALSE)####A matrix with the columns each centered based on the col mean. 
#     ####Not scaled by SD though. ( Y(m)_i - mean(Y(m)) )
#   
#   prod_Ycentered =   apply(Ycentered, MARGIN = 1, prod)
#     ###This gives the product of ( Y(m)_i - mean(Y(m)) ) for m in 1:q. 
#     ###This is useful because now we just can multiply by (X_ij - mean(X_j))
#   
#   ####print(length(prod_Ycentered))
#   
#   ######################################
#   ##This test code can later be removed. 
#   if(q > 1)
#   {
#     for (m in 1:q) {
#       K2yTEST[m] = cum2(Y[, m])
#     }
#     if(sum(K2y == K2yTEST) < q)
#     {
#       warning("Apply and the loop for SD of Y are not equal, but should be.")
#     }
#   }
#   
#   ######################################
#   
#   ###Get the SD for each covariate. 
#   for (j in 1:p) {
#     K2vec[j] = cum2(X[, j])
#   }
#   
#   
#   for (j in 1:p) 
#   {
#     #print(length(prod_Ycentered))
#     #print(length(X[,j] - mean(X[,j])))
#     phiVect[j] = sum(prod_Ycentered * (X[,j] - mean(X[,j])) ) /(n*sqrt(K2vec[j]*prod(K2y)))
#     
#   }
#   
#   return(abs(phiVect))
#  }


#########################################################################################
#########################################################################################
### Simulation Replications. Move this at some point to a separate file perhaps. 
#########################################################################################
#########################################################################################

# n = 60
# p = 3000
# q = 6
# score = c(0,0,0)
# 
# for(r in 1:200)
# {
#   X = matrix(NA, nrow = n, ncol = p)
#   for (i in 1:p) 
#   {
#     X[, i] = rnorm(n, 0, 5)
#     #X[, i] = rpois(n, 2)
#   }
#   
#   Y = matrix(0, nrow = n, ncol = q)
#   
#    # Y[,1] = exp(X[, 1] +X[,3] + 2*X[,5])
#    # Y[,2] = exp(X[, 1] -1.4*X[,3] +  X[,5])
#    # Y[,3] = exp(3*X[, 1] + X[,3] +X[,5])
#    # Y[,4] = exp(-X[, 1] -1*X[,3] +X[,5])
#    # Y[,5] = exp(-2*X[, 1] -1*X[,3] +X[,5])
#    # Y[,6] = exp(X[, 1] -1.5*X[,3] +2.5*X[,5])
#    # 
#   #
#   Y[,1] = X[, 1] +X[,3] + 3*X[,5]
#   Y[,2] = X[, 1] +0*X[,3] +  X[,5]
#   Y[,3] = 3*X[, 1] + X[,3] - X[,5]
#   Y[,4] = -X[, 1] -1*X[,3] +X[,5]
#   Y[,5] = -2*X[, 1] -1*X[,3] +X[,5]
#   Y[,6] = X[, 1] -1.5*X[,3]# -2.5*X[,5]
#   
#   ##Y = scale(Y)
#   
#   #q = 1
#   #Y = X[, 1] +X[,3] + X[,5]
#   
#   testData = cbind(Y, X)
#   
#   #testData = matrix(rnorm(300, 0,1), ncol = 30, nrow = 10)
#   #phi = CovarProd_MV_main(data = testData, q = q)
#   phi_num2 = CorrMat_MV_main(data = testData, q = q)
#   
#   # timestamp()
#   # omega = zllz(data = testData, q=q)
#   # timestamp()
#   
#   timestamp()
#   dc = DCSIS_MV(data = testData, q=q)
#   timestamp()
#   # 
#   # head(phi)
#   # sorted_phi = sort.int(phi, decreasing  = TRUE, index.return = TRUE)
#   # head(sorted_phi$ix, 10)
#   # phiMainScores = which(sorted_phi$ix %in% c(1,3,5))
#   
#   #head(phi_num2)
#   sorted_phi_num2 = sort.int(phi_num2, decreasing  = TRUE, index.return = TRUE)
#   #head(sorted_phi_num2$ix, 10)
#   phi_num2MainScores = which(sorted_phi_num2$ix %in% c(1,3,5))
#   
#   # head(omega)
#   # sorted_omeg = sort.int(omega, decreasing  = TRUE, index.return = TRUE)
#   # head(sorted_omeg$ix, 30)
#   # omegaMainScores = which(sorted_omeg$ix %in% c(1,3,5))
#   
#   #head(dc)
#   sorted_dc = sort.int(dc, decreasing  = TRUE, index.return = TRUE)
#   #head(sorted_dc$ix, 30)
#   dcMainScores = which(sorted_dc$ix %in% c(1,3,5))
#   
#   #phiMainScores
#   #print(omegaMainScores)
#   print(dcMainScores)
#   print(phi_num2MainScores)
#   
#   
#   
#   if(mean(dcMainScores) > mean(phi_num2MainScores))
#   {
#     score[3] = score[3] + 1 ####small phi LOST to big phi. Or omega lost 
#     
#     #print(phiMainScores)
#     #print(phi_num2MainScores)
#     
#   }else
#     
#   if(mean(dcMainScores) == mean(phi_num2MainScores))
#   {
#     score[2] = score[2] + 1
#   }else ####small phi beat big phi. 
#     
#   {
#     score[1] = score[1] + 1
#   }
# 
#   
#   if(r %% 5 == 0)
#   {
#     ##print(paste0("We are at " , r,". The score is Omega: ", score[1], " Tie: ", score[2], " Matrix phi: ", score[3]))
#     print(paste0("We are at " , r,". The score is DC: ", score[1], " Tie: ", score[2], " Matrix phi: ", score[3]))
#  
#   }
# }
# 
# 
# 
# 
# 
# 
# 