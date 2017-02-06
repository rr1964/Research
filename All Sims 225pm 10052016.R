##This is where all the side by side simulations will be performed. 

#### CU039543EYRS-ZZBZ-SZX2

#install.packages("coin")
library(coin)
########
########
########
###Data Creation for CA Trend. Y binary, X_j trinary. 


makeYBinary <- function(n, percent0 = 0.5)
{
  sample(c(0,1),n,replace = TRUE, prob = c(percent0,1-percent0))###These are the Y's

}



####Should we generate the levels of X_j based on a uniform sample, or a binomial sample? sample() versus rbinom() 
#### It seems that it will be much easier to get a trend-like pattern for the levels of X_j if we use rbinom.
#### Use probabilities such as (0.45, 0.55) for Y = 0,1. This will cause more "successes" to correspond with higher levels of X_j. 

makeX_ijTrinary <- function(n,p, trueSize = 10, Y) 
{
  
  probability = c(0.3, 0.4, 0.6, 0.7, 0.2, 0.4, 0.3, 0.8, 0.4, 0.2,
                  0.6, 0.1, 0.1, 0.4, 0.8, 0.7, 0.9, 0.2, 0.7, 0.6)####Table 4.1
  
  table4_1 = matrix(data =probability , 2,10, byrow = TRUE)
  
  
  X_ij = matrix(rep(-1,n*10), nrow = n, ncol = p)##The X_js are stored here for all n-many replicates.
  ####Initialized to -1 for ease of locating missed assignments.
  
  for(j in 1:trueSize)
  {
   
    table4_1Vals = table4_1[Y+1,j]###If Y = m, the (m+1)th row of the jth column is the probability pi_mj given Y = m. 
    ###This collects the requisite table values and puts them in a vector. 
    
    
    X_ij[,j] = sapply(table4_1Vals, rbinom, n = 1, size = 2) 
    ###We define a whole column here! Each entry takes on the value of 0, 1, or 2. 
  }
  
  
  
  for(j in (trueSize+1):p)
  {
    X_ij[,j] = sample(c(0,1,2), n, replace= TRUE, prob = rep(0.333333,3))
  }
  
  return(X_ij)
}

########################################################################
########################################################################
########################################################################
##### Create the simulation data  from Huang et al.
##### For standardness sake, Y = 0,1,2,3. In Huang, it its 1-4, but I am abandoning that in my code. 
makeYHuang <- function(n)
{
  sample(c(1,2,3,0),n,replace = TRUE, prob = rep(0.25,4))###These are the Y's
}




probability = c(0.2, 0.8, 0.7, 0.2, 0.2, 0.9, 0.1,0.1,0.7,0.7,
                0.9,0.3,0.3,0.7,0.8, 0.4, 0.7, 0.6, 0.4, 0.1,
                0.7,0.2,0.1,0.6,0.7,0.6,0.8,0.9,0.1,0.8, 
                0.1, 0.9, 0.6, 0.1, 0.3, 0.1, 0.4, 0.3, 0.6, 0.4)####Table 1

table1 = matrix(data =probability , 4,10, byrow = TRUE)

makeX_ijHuang <- function(n,p, trueSize, Y) 
{
  X_ij = matrix(rep(-1,n*10), nrow = n, ncol = p)##The X_js are stored here for all n-many replicates.
  ####Initialized to -1 for ease of locating missed assignments.
  for(i in 1:n)
  {
    for(j in 1:trueSize)
    {
      
      k = Y[i]
      
      table1Val = table1[k+1,j]###the (k+1)th row of the jth column is the probability of X_j = 1 given Y = k.
      #####Remember that I am taking Y = 0,1,2,3. 
      
      X_ij[i,j] = rbinom(1, 1, table1Val)  
    }
    
  }  
  
  for(j in (trueSize+1):p)
  {
    X_ij[,j] = rbinom(n, 1, 0.5)
  }
  
  return(X_ij)
}

Y = makeYBinary(200, percent0 = 0.35)
covarsX = makeX_ijHuang(n = 200, p = 1000, trueSize = 10, Y = Y)

dta <- data.frame(
  y = gl(3, 2),
  x = sample(gl(3, 2))
)
## Asymptotic Cochran-Mantel-Haenszel Test
ct <- cmh_test(y ~ x, data = dta)

ct######Not the CA trend test we are looking for. 

#####cmh_test()

######Testing correlation of Y and X. 
  Y = makeYBinary(500)
  X_ij = makeX_ijTrinary(n = 500, p =3000, Y = Y)
  
  for(j in 1:20)
  print(cor(x =X_ij[,j], y = Y ))



##############################################################################################
##############################################################################################
##############################################################################################

###Distance Correlation

DCSIS = function(u, v, n){
  u = as.numeric(u)
  v = as.numeric(v)
  m.u = matrix(u, n, n, byrow = TRUE)
  m.v = matrix(v, n, n, byrow = TRUE)
  s.u = abs(m.u - t(m.u))
  s.v = abs(m.v - t(m.v))
  s1 = sum((s.u) * (s.v)) / (n * n)
  s2 = sum(s.u) * sum(s.v) / (n * n * n * n)
  s3 = sum(rowSums(s.u) * (s.v)) / (n * n * n)
  
  dcov <- sqrt(abs(s1 + s2 -2 * s3))
  return(dcov)
}

Y = makeYEx1(4)
Xij = makeX_ijEx1(n = 4, p =20, Y = Y)

for(j in 1:20)
{
  print(DCSIS(Xij[,j], Y, n = 4))
}

DCSIS(Xij[,2], Y, n = 4)

Xij[,2]
Y

Y = makeYHuang(200)
Xij = makeX_ijHuang(n = 200, p =1000, Y = Y)
p = dim(Xij)[2] 
N = dim(Xij)[1] 


rank.DCSIS = rep(0, p)
omega = matrix(0, 1, p)

dcov.y = DCSIS(Y,Y, n = N)
for(j in 1:p){
  X.j = matrix(Xij[, j])
  dcov.xy = DCSIS(X.j, Y, n = N)
  dcov.xx = DCSIS(X.j, X.j, n = N)
  dcorr = dcov.xy / sqrt(dcov.xx * dcov.y)
  omega[, j] = dcorr * dcorr      
} 
rank.DCSIS = p +1- rank(omega, ties.method = 'random')
rank.DCSIS

in.TrueModel = (rank.DCSIS <= 10)
max(which(in.TrueModel))#####This finds the smallest model size required to include the true model. 


###################################################################################################
###################################################################################################
####### CA-Trend specific code.


calculatep_lk <- function(X_ijMat, numYlevels = 2, capK_j, vscores = c(0,1,2), p, n, Y)
{
  maxK = max(capK_j)
  p_lk = array(dim = c(numYlevels,maxK, p))
  
  
  for(j in 1:p)
    for(k in 1:capK_j[j])
    {
      for(m in 0:(numYlevels-1))
      {
        p_lk[m+1,k,j] =  1/n *sum((Y == m)*(X_ijMat[,j] == vscores[k]))
        ###The total proportion of X_ij=v_k when Y = m
        
      }
      
    }
  
  return(p_lk)
}



calculate_rSquared <- function(v_kj,  p_lk, capK_j, p, Y, numYlevels = 2)
{####v_kj is a (perhaps ragged) K_j x p matrix of categorical scores for each level k of X_j.
  ####In practice, v_kj is the same for every column. Above I just 
  ###p_lk is a (perhaps ragged) m x K_j x p array 
  ### with the total proportion (out of n) of the X_ij=k associated with response l.  
  
  ###capK_j is a vector of all of the K_j associated with the X_j
  
  ###p is p, the number of full model covariates. 
  
  numSummands = matrix(nrow = max(capK_j), ncol = p)###
  XvarSummands = matrix(nrow = max(capK_j), ncol = p)###
  YvarSummands = vector(length = numYlevels)
  ##print(numSummands)
  
  yBar = mean(Y)
  v_jBar = apply(v_kj, MARGIN = 2, mean)###v_jBar is the average score for the levels of X_j.
  
  numerator = vector(length = p)
  denominator = vector(length = p)
  
  
  
  #print((1-yBar)*(v_kj[1,1] - v_jBar[1])*p_lk[2,1,1] - yBar*(v_kj[1,1] - v_jBar[1])*p_lk[1,1,1])
  
  for(j in 1:p)
  {  
    for(k in 1:capK_j[j])
    {
      iterationOverM = vector(length = numYlevels)
      for(m in 0:(numYlevels-1))
      {
        iterationOverM[m+1] = (m-yBar)*(v_kj[k,j] - v_jBar[j])*p_lk[m+1,k,j] 
        YvarSummands[m+1] = (m-yBar)^2*sum(p_lk[m+1,,j])
      }
      
      numSummands[k,j] = sum(iterationOverM)
      XvarSummands[k,j] = sum(p_lk[,k,j])*(v_kj[k,j] - v_jBar[j])^2
      
    }
    
    numerator[j] =  sum(numSummands[,j])^2
    denominator[j] = sum(YvarSummands) * sum(XvarSummands[,j])
  }
  
  rSquared = numerator/denominator
  
  return(list(rSquared, numerator))####This is a vector of length p. So we have each r^2_j in a vector.
  
}


findCutoff <- function(sorted_r)##The rSquareds should be sorted in descending order.
{
  #####I am going to ignore the tail values (j = 1, and the very very small rSquareds )
  #### The tail values are throwing it off. We do not want the empty model or the (near) full model.
  #### The issue is that some of the rSquared_j for large j are so small that their ratio is huge.
  
  pShortened = length(sorted_r)###Accounting for removing the really tiny r^2 values. 
  ratios = vector(length = pShortened)### 
  ratios[1] = sorted_r[1]/sorted_r[2]
  for(j in 2:pShortened)###Ignoring the tail values on each end
  {
    ratios[j] = sorted_r[j]/sorted_r[j+1]
  }
  
  dHat =which.max(ratios) ####This which.max function is equivalnet to argmax (which is not a function in R)
  
  #print(ratios)
  
  return(dHat)
}

runSimulationCATrend_findCutoff <- function(n, p)
{
  set.seed(7919)
  #Y = makeYHuang(n)
  Y = makeYBinary(n)
  #print(head(Y,20))
  
  #scoresV = matrix(data = c(0,1), nrow = 2, ncol = p)
  #print(scoresV)
  scoresV = matrix(data = c(0,1,2), nrow = 3, ncol = p)
  
  # K_jVect = rep(2, p)
  K_jVect = rep(3, p)
  #print(K_jVect)
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijTrinary(n = n, p = p, trueSize = 10, Y = Y)
  #print(head.matrix(covarsX_ij, 3))
  
  p_lkMat = calculatep_lk(X_ijMat = covarsX_ij,numYlevels = 2, capK_j = K_jVect, p = p,n = n, Y = Y)
  
  #print(p_lkMat[,,1:15])
  
  rSquared_j  = calculate_rSquared(v_kj = scoresV,p_lk = p_lkMat, capK_j = K_jVect, p= p, Y = Y , numYlevels = 2)[[1]] 
  rSquared_j = rSquared_j[rSquared_j > 5e-6] 
  
  #print(rSquared_j)
  
  sorted_r = sort(rSquared_j, index.return = TRUE, decreasing = TRUE)
  
  #print(head(sorted_r$ix,10))
  if(sum(head(sorted_r$ix, 10))>sum(1:10))
  {warning("The largest rSquareds were not the first ten!")}
  
  dHat = findCutoff(sorted_r$x[1:(length(sorted_r$x)-(p*(30/n)))])
  
  #print(sorted_r$x[dHat])
  #print(sorted_r$x[dHat+1])
  
  return(dHat)
}

runSimulationCATrend_findCutoff(n = 200, p = 1000)


##########################################################
##########################################################
#### MMLE METHOD
#### I USE THE BULIT IN glm() function. It performs mle.  

extractBeta_j_2ndMethod <- function(p, X_ij, Y)
{
  Beta_jHatVect = vector(length = p)####A vector of all the beta_j hats taken in MAGNITUDE.
  
  for(j in 1:p)
  {
    Beta_jHatVect[j] = glm(Y ~ X_ij[,j], family = "binomial")$coefficients[[2]]
  }
  
  Beta_jHatVect = abs(Beta_jHatVect)###We just want magnitude.
}

##########################################################
##########################################################
#### Huang's METHOD Huang's METHOD Huang's METHOD
#### I ADAPT THE BULIT IN chisq.test() function. 

makeAllDelta_j_Prefab <- function(X_ij, Y, n)
{
  #####The returned value from the built in method chisq.test is n*Delta.
  
  Deltas = apply(X_ij, MARGIN = 2, function(X_jColumn, y){return(1/n * as.numeric(chisq.test(x = X_jColumn, y = Y)$statistic))}, y = Y)
  #apply(covarsX, MARGIN = 2, chisq.test, y = Y)
  return(Deltas)
  
  
}


#####################################################################################################
#####################################################################################################
########################             THE ACTUAL SIMULATIONS                    ######################
#####################################################################################################
#####################################################################################################

########Doing CA-trend on Huang's data is proving very difficult for some reason. 
########Either CA-doesn't work on Huang data....or something is off in the code. Look at the packages perhaps.
######## Although none of the packages seem to be quickly implementable or even what we want.
######## coin for cmh_test() is not what we want. It is a different method.

######A function to find p_j as she calls it (p_s in DCSIS)

prepForp_j <-function(sortedIndicies, d)
{
  firstDfeatures = sortedIndicies[1:d]
  containsEach = 1:10 %in% firstDfeatures
  return(containsEach)
}

#### We report the minimum model size required to contain all true predictors.

######-------------------------------------------------------------------------------------------------------------------------------
#########CA Trend CA Trend CA Trend CA Trend CA Trend CA Trend
######-------------------------------------------------------------------------------------------------------------------------------
runSimCATrend_minModSize <- function(n, p, preseed = 7919)
{
  set.seed(preseed)
  #Y = makeYHuang(n)
  Y = makeYBinary(n, percent0 = 0.5)
  #print(head(Y,20))
  
  #scoresV = matrix(data = c(0,1), nrow = 2, ncol = p)
  #print(scoresV)
  scoresV = matrix(data = c(0,1,2), nrow = 3, ncol = p)
  
  # K_jVect = rep(2, p)
  K_jVect = rep(3, p)
  #print(K_jVect)
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijTrinary(n = n, p = p, trueSize = 10, Y = Y)
  #print(head.matrix(covarsX_ij, 3))
  
  #print(covarsX_ij[1:10, 1:20])
  
  p_lkMat = calculatep_lk(X_ijMat = covarsX_ij,numYlevels = 2, capK_j = K_jVect, p = p,n = n, Y = Y)
  
  #print(p_lkMat[,,1:15])
  
  rSquared_j  = calculate_rSquared(v_kj = scoresV,p_lk = p_lkMat, capK_j = K_jVect, p= p, Y = Y , numYlevels = 2)[[1]] 
  rSquared_j = rSquared_j[rSquared_j > 5e-6] 
  
  #print(rSquared_j)
  
  sorted_r = sort(rSquared_j, index.return = TRUE, decreasing = TRUE)
  first10 = prepForp_j(sorted_r$ix, 10)
  first11 = prepForp_j(sorted_r$ix, 11)
  first15 = prepForp_j(sorted_r$ix, 15)
  
  in.TrueModel = (sorted_r$ix <= 10)
  minModelSize = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model. 
  
  return(list(minModelSize,first10, first11, first15))
}

runSimCATrend_minModSize(n = 100, p = 3000)

reps = 500
minModelSizes = vector(length = reps)
p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)

timestamp()##------ Wed Aug 17 11:37:34 2016 ------##
for(w in 1:reps)
{
  if(w %% 100 == 0)
  {print("Benchmark" )}
  newSeed = 7919 +20*w + w^2 - w^3
  
  simList = runSimCATrend_minModSize(n = 200, p = 5000, preseed = newSeed)
  
  minModelSizes[w] =  simList[[1]]
  p_j_forD10[w,] =  simList[[2]]
  p_j_forD11[w,] =  simList[[3]]
  p_j_forD15[w,] =  simList[[4]]
}
timestamp()##------ Wed Aug 17 11:40:53 2016 ------##
mean(minModelSizes)## 10.302#####The listed menas are all for percent0 = 0.5
head(minModelSizes)
CAminModelSizes = c(11,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,12,10,10,10,10,11,10,10,10,10,
                  10,19,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                  10,10,10,11,10,10,10,10,10,10,10,54,10,10,10,10,10,10,10,11,11,10,10,10,10,10,10,
                  10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,11,10,10,
                  10,10,10,10,10,10,10,10,10,10,10,10,32,10,10,10,12,10,11,10,10,10,10,10,10,10,
                  11,11,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,14,
                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,
                  10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,
                  10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,11,10,10,10,10,10,10,10,10,10,10,10,10,
                  10,10,10,10,10,10,10,12,10,11,10,10,10,10,10,10,10,10,10,10,11,10,11,10,10,10,10,11,10,
                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,11,10,10,10,10,10,10,
                  10,10,10,10,10,10,10,11,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,12,10,10,
                  10,12,10,10,10,10,10,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,17,10,16,
                  10,10,10,10,10,11,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,12,10,10,10,10,
                  10,10,10,10,10,14,10,10,11,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,11,10,10,10,
                  10,10,10,10,10,10,10,10,11,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,
                  10,10,10,10,10,10,15,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10)

quantile(minModelSizes, probs =c(0.25,0.75, 0.9,0.95, 0.99))

get_p_a <-function(minModSzs,d)
{
  fullTrueModelNum = sum((minModSzs <= d))
  
  return(fullTrueModelNum/length(minModSzs))
}

get_p_j <-function(first10, first11, first15)
{
  p_j_10 = apply(first10, MARGIN = 2, sum)/reps
  p_j_11 = apply(first11, MARGIN = 2, sum)/reps
  p_j_15 = apply(first15, MARGIN = 2, sum)/reps
  return(list(p_j_10, p_j_11, p_j_15))
}

get_p_a(minModSzs = CAminModelSizes, d = 10)
get_p_a(minModSzs = CAminModelSizes, d = 11)
get_p_a(minModSzs = CAminModelSizes, d = 12)
get_p_a(minModSzs = CAminModelSizes, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)


######-------------------------------------------------------------------------------------------------------------------------------
######### MMLE MMLE MMLE MMLE MMLE
######-------------------------------------------------------------------------------------------------------------------------------
runSimMMLE_minModSize <- function(n, p, preseed = 7919)
{
  set.seed(preseed)
  
  Y = makeYBinary(n, percent0 = 0.5)
  covarsX_ij = makeX_ijTrinary(n = n, p = p, trueSize = 10, Y = Y)
  
  
  theBetas = extractBeta_j_2ndMethod(p,covarsX_ij, Y)###Faster than my method
  
  sorted_beta = sort(theBetas, index.return = TRUE, decreasing = TRUE)
  first10 = prepForp_j(sorted_beta$ix, 10)
  first11 = prepForp_j(sorted_beta$ix, 11)
  first15 = prepForp_j(sorted_beta$ix, 15)
  
  in.TrueModel = (sorted_beta$ix <= 10)
  minModSz = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model. 
  
  return(list(minModSz, first10, first11, first15))
}

runSimMMLE_minModSize(n = 200, p = 3000)

reps = 500

minModelSizes = vector(length = reps)

p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)


timestamp()#####Rather slow. Can this be vectorized?
for(w in 1:reps)
{
  newSeed = 7919 +20*w + w^2 - w^3
  simList = runSimMMLE_minModSize(n = 200, p = 3000, preseed = newSeed)
  
  if(w %% 100 == 0)	
 {print("Benchmark" )}
  
  minModelSizes[w] =  simList[[1]]
  p_j_forD10[w,] =  simList[[2]]
  p_j_forD11[w,] =  simList[[3]]
  p_j_forD15[w,] =  simList[[4]]
 
}
timestamp()
mean(minModelSizes)####10.042#####The listed menas are all for percent0 = 0.5
head(minModelSizes)
MMLEminModelSizes = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,20,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,15,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10)

mean(MMLEminModelSizes)
quantile(MMLEminModelSizes, probs =c(0.25,0.5,0.9,0.95, 0.99))

get_p_a(minModSzs = MMLEminModelSizes, d = 10)
get_p_a(minModSzs = MMLEminModelSizes, d = 11)
get_p_a(minModSzs = MMLEminModelSizes, d = 12)
get_p_a(minModSzs = MMLEminModelSizes, d = 15)


get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)


######-------------------------------------------------------------------------------------------------------------------------------
#########DCSIS DCSIS DCSIS DCSIS DCSIS DCSIS DCSIS
######-------------------------------------------------------------------------------------------------------------------------------

runSimDCSIS_minModSize <- function(n, p, preseed = 7919)
{
  set.seed(preseed)
  
  Y = makeYBinary(n)
  covarsX_ij = makeX_ijTrinary(n = n, p = p, trueSize = 10, Y = Y)
  #p = dim(covarsX_ij)[2] 
  #N = dim(covarsX_ij)[1] 


  rank.DCSIS = rep(0, p)
  omega = matrix(0, 1, p)

  dcov.y = DCSIS(Y,Y, n = n)
  for(j in 1:p)
  {
    X.j = matrix(covarsX_ij[, j])
    dcov.xy = DCSIS(X.j, Y, n = n)
    dcov.xx = DCSIS(X.j, X.j, n = n)
    dcorr = dcov.xy / sqrt(dcov.xx * dcov.y)
    omega[, j] = dcorr * dcorr      
  } 
    
  #sorted.DCSIS = p +1- rank(omega, ties.method = 'random')
  sorted.DCSIS = sort(omega, index.return = TRUE, decreasing = TRUE)
  
  
  first10 = prepForp_j(sorted.DCSIS$ix, 10)
  first11 = prepForp_j(sorted.DCSIS$ix, 11)
  first15 = prepForp_j(sorted.DCSIS$ix, 15)
  
  
  in.TrueModel = (sorted.DCSIS$ix <= 10)
  minModSz = max(which(in.TrueModel))
  
  return(list(minModSz, first10, first11, first15))
}

reps = 500
minModelSizes = vector(length = reps)
p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)

timestamp()##------ Wed Aug 17 11:46:22 2016 ------####------ Mon Aug 29 12:50:34 2016 ------##
for(w in 1:reps)
{
  if(w %% 100 == 0)
  {print("Benchmark" )}
  
  newSeed = 7919 +20*w + w^2 - w^3
  
  simList = runSimDCSIS_minModSize(n = 200, p = 5000, preseed = newSeed)	
  	  			
  	  minModelSizes[w] =  simList[[1]]			
  	  p_j_forD10[w,] =  simList[[2]]			
  	  p_j_forD11[w,] =  simList[[3]]			
  	  p_j_forD15[w,] =  simList[[4]]			
  	  
}
timestamp()##------ Wed Aug 17 13:12:45 2016 ------####------ Mon Aug 29 14:11:51 2016 ------##

mean(minModelSizes)#10.324#####The listed menas are all for percent0 = 0.5 
head(minModelSizes)
### 
DCSISminModelSizes = c(12,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,17,10,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,59,10,10,10,10,10,10,10,11,11,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,13,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,36,10,11,10,10,10,11,10,10,10,10,10,10,10,11,12,10,10,10,10,10,
                       10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,15,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,14,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,10,10,12,10,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,11,10,10,10,10,10,10,10,10,10,10,
                       11,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,11,10,10,10,10,10,10,10,10,10,11,10,10,10,10,
                       10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,12,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,20,10,19,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,
                       10,10,10,10,10,10,10,10,10,10,10,10,19,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10)
mean(DCSISminModelSizes)

quantile(DCSISminModelSizes, probs =c(0.25,0.5, 0.9,0.95, 0.99))

get_p_a(minModSzs = minModelSizes, d = 10)
get_p_a(minModSzs = minModelSizes, d = 11)
get_p_a(minModSzs = minModelSizes, d = 12)
get_p_a(minModSzs = minModelSizes, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)


######-------------------------------------------------------------------------------------------------------------------------------
#########HUANG HUANG HUANG HUANG HUANG
######-------------------------------------------------------------------------------------------------------------------------------

runSimHuang_minModSize <- function(n, p, preseed = 7919)
{
  set.seed(preseed)
  
  Y = makeYBinary(n, percent0 = 0.5)
  covarsX_ij = makeX_ijTrinary(n = n, p = p, trueSize = 10, Y = Y)


  theDeltas = makeAllDelta_j_Prefab(X_ij = covarsX_ij, Y = Y, n = n)
  
  
  sorted_Delta = sort(theDeltas, index.return = TRUE, decreasing = TRUE)
  
  
  first10 = prepForp_j(sorted_Delta$ix, 10)	
  first11 = prepForp_j(sorted_Delta$ix, 11)			
  first15 = prepForp_j(sorted_Delta$ix, 15)			
  	  
  	in.TrueModel = (sorted_Delta$ix <= 10)	
  
 	  minModSz = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model.		
 	  
   return(list(minModSz, first10, first11, first15))	
}

runSimHuang_minModSize(n = 200, p = 3000)			
			
	reps = 500	
	minModelSizes = vector(length = reps)		
	p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)	
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)			
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)			
	
timestamp()##------ Thu Aug 18 15:27:57 2016 ------##
for(w in 1:reps)
{
  if(w %% 100 == 0)
  {print("Benchmark" )}
  
  newSeed = 7919 +20*w + w^2 - w^3
  simList = runSimHuang_minModSize(n = 200, p = 5000, preseed = newSeed)
  
  minModelSizes[w] =  simList[[1]]			
    p_j_forD10[w,] =  simList[[2]]			
 	  p_j_forD11[w,] =  simList[[3]]			
 	  p_j_forD15[w,] =  simList[[4]]
}
timestamp()##------ Thu Aug 18 15:49:23 2016 ------##

mean(minModelSizes)#10.674  #####The listed menas are all for percent0 = 0.5
head(minModelSizes)#13 10 10 10 10 10
HuangminModelSizes = c(13,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,14,10,10,10,10,10,10,10,10,10,10,20,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,13,10,10,10,10,10,10,10,103,10,10,10,10,10,10,10,11,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,
10,10,10,10,11,11,10,10,13,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,14,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,49,10,12,10,11,10,16,10,10,10,10,10,10,10,11,13,10,10,10,10,10,10,10,10,10,10,10,11,10,10,12,10,10,10,10,10,10,10,10,10,10,10,24,10,10,10,10,
11,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,19,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,11,10,10,10,10,10,10,10,
11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,13,11,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,22,10,13,10,10,10,10,10,10,10,10,
10,10,11,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,12,10,10,10,11,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,
10,10,11,11,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,12,10,10,10,11,10,11,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,35,10,37,10,11,11,10,
10,10,10,10,11,10,10,10,15,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,11,10,
10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,28,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,11,10,10,10,10,10,10,10)

mean(HuangminModelSizes)


quantile(minModelSizes, probs =c(0.25,0.75, 0.9, 0.95, 0.99))


get_p_a(minModSzs = minModelSizes, d = 10)
get_p_a(minModSzs = minModelSizes, d = 11)
get_p_a(minModSzs = minModelSizes, d = 12)
get_p_a(minModSzs = minModelSizes, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)

##############  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ##############
##############      Making LINEARLY Correlated Data.        ##############
##############  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ##############


########Some preliminary tests.
Ytest = makeYBinary(200)
X_ij = sapply(Ytest, function(y){rnorm(1, mean =y, sd = 1)})
plot(X_ij)
X_ij[X_ij>1.6912] = 2
X_ij[X_ij>0.3088 & X_ij<1.6912] = 1
X_ij[X_ij < 0.3088] = 0
table(X_ij, Ytest)


Ytest = makeYBinary(200)
X_ij = sapply(Ytest, function(y){rnorm(1, mean =y, sd = 1)})
plot(X_ij)
X_ij[X_ij>= 1] = 2
X_ij[X_ij>0.25 & X_ij<1] = 1
X_ij[X_ij <= 0.25] = 0
table(X_ij, Ytest)

findCutoff <- function(kappa, largerMean = 2)
{
  return(0.5*(pnorm(kappa, mean = largerMean) + pnorm(kappa)))
}




##################################################################
##### Generating the trend data.
##################################################################

##upperMeanScalers= c(2,2,2,2,2,1,1,1,1,1)

upperMeanScalers= c(1,1,1,1,1,1,1,1,1,1)

#vectorCutoffs = c(0, 2, 0.3088, 1.6912, 1, 1.832, 1, 2.283, 0.5866, 1.4134,
 #                 0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)

vectorCutoffs = c(0, 0.75, 0, 1, 0.2, 0.8, 0, 0.9, -0.213, 1.213,
                 0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)

#vectorCutoffs = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
 #                 0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)

cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)

cutoffs

###Generate Y as before.
###Then generate X_ij using the following function.

makeX_ijLinear <- function(n,p, trueSize = 10, Y, cutoffs, upperMeanScalers, vscores)
{
  X_ij = matrix(-1, nrow = n, ncol = p)##The X_js are stored here for all n-many replicates.
  ####Initialized to -1 for ease of locating missed assignments.'
  #print(X_ij)
  for(j in 1:trueSize)
  {
     column_j = sapply(Y, function(y){rnorm(1, mean =upperMeanScalers[j]*y, sd = 1)})
    #Define a whole column.
     
     
     kappa_L = cutoffs[j,1]
     kappa_U = cutoffs[j,2]
     
    
    column_j[(column_j>kappa_U)] = 222 ###This is done to prevent reassignment when the kappa_U is > 2.
    column_j[(column_j>=kappa_L & column_j<=kappa_U)] = vscores[2]
    column_j[(column_j < kappa_L)] = vscores[1]
    column_j[column_j ==222] = vscores[3] #Switch to the correct value after assignment of other levels.
    X_ij[,j] = column_j
  }
  
  
  
  for(j in (trueSize+1):p)
  {
    X_ij[,j] = sample(vscores, n, replace= TRUE, prob = rep(0.333333,3))
  }
  
  return(X_ij)
  
}


Y = makeYBinary(200)
covarsX = makeX_ijLinear(n = 200, p = 30, Y = Y, cutoffs = cutoffs, upperMeanScalers = upperMeanScalers, vscores = c(0,2,4))
covarsX

####Look at Spearman's Rho test. Maybe use as another screening method.

table(covarsX[,2], Y)

cor(x = covarsX[,6], y = Y)
cor(x = covarsX[,12], y = Y)

for(j in 1:30)
{
  print(cor(x = covarsX[,j], y = Y))
}


##########################################################################
############# SIMULATIONS ON Linearly correlated Data. 


#### We report the minimum model size required to contain all true predictors.

######-------------------------------------------------------------------------------------------------------------------------------
#########CA Trend CA Trend CA Trend CA Trend CA Trend CA Trend
######-------------------------------------------------------------------------------------------------------------------------------

runSimCATrend_Linear <- function(n, p, preseed = 7919, scores)
{
  set.seed(preseed)
  #Y = makeYHuang(n)
  Y = makeYBinary(n, percent0 = 0.5)
  #print(head(Y,20))
  
  #scoresV = matrix(data = c(0,1), nrow = 2, ncol = p)
  #print(scoresV)
  scoresV = matrix(data = scores, nrow = 3, ncol = p)
  
  # K_jVect = rep(2, p)
  K_jVect = rep(3, p)
  #print(K_jVect)
  
  ums= c(1,1,1,1,1,1,1,1,1,1)
  
  vectorCutoffs = c(0, 0.75, 0, 1, 0.2, 0.8, 0, 0.9, -0.213, 1.213,
                    0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)
  
  cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)
  
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijLinear(n = n, p = p, Y = Y, cutoffs = cutoffs, 
                              upperMeanScalers = ums, vscores = scores)
  #print(head.matrix(covarsX_ij, 3))
  
  #print(covarsX_ij[1:10, 1:20])
  
  p_lkMat = calculatep_lk(X_ijMat = covarsX_ij,numYlevels = 2, capK_j = K_jVect,
                          p = p,n = n, Y = Y, vscores = scores)
  
  #print(p_lkMat[,,1:15])
  
  rSquared_j  = calculate_rSquared(v_kj = scoresV,p_lk = p_lkMat,
                                   capK_j = K_jVect, p= p, Y = Y , numYlevels = 2) 
  #rSquared_j = rSquared_j[rSquared_j > 5e-6] 
  
  #print(rSquared_j)
  
  sorted_r = sort(rSquared_j, index.return = TRUE, decreasing = TRUE)
  first10 = prepForp_j(sorted_r$ix, 10)
  first11 = prepForp_j(sorted_r$ix, 11)
  first15 = prepForp_j(sorted_r$ix, 15)
  
  in.TrueModel = (sorted_r$ix <= 10)
  minModelSize = max(which(in.TrueModel))
  #####This finds the smallest model size required to include the true model. 
  
  return(list(minModelSize,first10, first11, first15))
}

runSimCATrend_Linear(n = 200, p = 5000, preseed = round(runif(1)*100000), scores= c(0,2,5))

reps = 500
minModelSizes = vector(length = reps)
p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)

timestamp()##------ Tue Aug 30 11:28:02 2016 ------##
for(w in 1:reps)
{
  if(w %% 100 == 0)
  {print("Benchmark" )}
  newSeed = 7919 +20*w + w^2 - w^3
  
  simList = runSimCATrend_Linear(n = 125, p = 3000, preseed = newSeed, scores = c(0,2,6))
  
  minModelSizes[w] =  simList[[1]]
  p_j_forD10[w,] =  simList[[2]]
  p_j_forD11[w,] =  simList[[3]]
  p_j_forD15[w,] =  simList[[4]]
}
timestamp()##------ Tue Aug 30 11:33:11 2016 ------##
mean(minModelSizes)####10.15
head(minModelSizes)
CAminModelLinear = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,15,10,10,10,10,10,10,10,10,16,10,10,10,10,10,10,13,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,11,10,10,10,10,11,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,
                     10,10,10,10,10,10,13,10,11,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,16,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,19,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,14,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                     10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,12,10,17,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,
                     10,10,13,10,13,10)
mean(CAminModelLinear)

quantile(minModelSizes, probs =c(0.25,0.75, 0.9,0.95, 0.99))

get_p_a <-function(minModSzs,d)
{
  fullTrueModelNum = sum((minModSzs <= d))
  
  return(fullTrueModelNum/length(minModSzs))
}

get_p_j <-function(first10, first11, first15)
{
  p_j_10 = apply(first10, MARGIN = 2, sum)/reps
  p_j_11 = apply(first11, MARGIN = 2, sum)/reps
  p_j_15 = apply(first15, MARGIN = 2, sum)/reps
  return(list(p_j_10, p_j_11, p_j_15))
}

get_p_a(minModSzs = minModelSizes, d = 10)
get_p_a(minModSzs = minModelSizes, d = 11)
get_p_a(minModSzs = minModelSizes, d = 12)
get_p_a(minModSzs = minModelSizes, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)

##############################################################################
##############################################################################
##### MMLE MMLE MMLE MMLE MMLE MMLE MMLE
##############################################################################
##############################################################################

runSimMMLE_Linear <- function(n, p, preseed = 7919, scores = c(0,1,2))
{
  set.seed(preseed)
  
  Y = makeYBinary(n, percent0 = 0.5)
 
  
  ums= c(1,1,1,1,1,1,1,1,1,1)
  
  vectorCutoffs = c(0, 0.75, 0, 1, 0.2, 0.8, 0, 0.9, -0.213, 1.213,
                    0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)
  
  cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)
  
  cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)
  
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijLinear(n = n, p = p, Y = Y, cutoffs = cutoffs,
                              upperMeanScalers = ums, vscores = scores)
  
  
  theBetas = extractBeta_j_2ndMethod(p,covarsX_ij, Y)###Faster than my method
  
  sorted_beta = sort(theBetas, index.return = TRUE, decreasing = TRUE)
  first10 = prepForp_j(sorted_beta$ix, 10)
  first11 = prepForp_j(sorted_beta$ix, 11)
  first15 = prepForp_j(sorted_beta$ix, 15)
  
  in.TrueModel = (sorted_beta$ix <= 10)
  minModSz = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model. 
  
  return(list(minModSz, first10, first11, first15))
}

runSimMMLE_Linear(n = 200, p = 3000, scores = c(0,2,5))

reps = 500

minModelSizes = vector(length = reps)

p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)


timestamp()##------ Tue Aug 30 14:54:48 2016 ------##
for(w in 1:reps)
{
  newSeed = 7919 +20*w + w^2 - w^3
  simList = runSimMMLE_Linear(n = 125, p = 3000, preseed = newSeed, scores = c(0,2,6))
  
  if(w %% 100 == 0)  
  {print("Benchmark" )}
  
  minModelSizes[w] =  simList[[1]]
  p_j_forD10[w,] =  simList[[2]]
  p_j_forD11[w,] =  simList[[3]]
  p_j_forD15[w,] =  simList[[4]]
  
}
timestamp()##------ Tue Aug 30 15:50:11 2016 ------##

mean(minModelSizes)####10.192
head(minModelSizes)

quantile(minModelSizes, probs =c(0.25,0.5, 0.9,0.95, 0.99))

MMLEMinModelLinear = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,11,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,15,10,10,10,11,10,
  10,10,10,17,10,10,11,10,10,10,14,10,10,10,17,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,12,10,10,10,10,10,10,10,10,10,11,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,15,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,12,10,10,11,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,23,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,11,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,
  10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,15,10,17,10,10,10,10,10,10,10,10,10,13,
  10,10,10,10,10,10,10,10,10,10,10,10,17,10,12,10)

mean(MMLEMinModelLinear)
quantile(MMLEMinModelLinear, probs =c(0.25,0.5, 0.9,0.95, 0.99))

get_p_a(minModSzs = minModelSizes, d = 10)
get_p_a(minModSzs = minModelSizes, d = 11)
get_p_a(minModSzs = minModelSizes, d = 12)
get_p_a(minModSzs = minModelSizes, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)
  
##############################################################################
##############################################################################
##### DC-SIS DC-SIS DC-SIS
##############################################################################
##############################################################################

runSimDCSIS_Linear <- function(n, p, preseed = 7919, scores = c(0,1,2))
{
  set.seed(preseed)
  
  Y = makeYBinary(n, percent0 = 0.5)
  
  
  ums= c(2,2,2,2,2,1,1,1,1,1)
  
  vectorCutoffs = c(0, 2, 0.3088, 1.6912, 1, 1.832, 1, 2.283, 0.5866, 1.4134,
                    0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)
  
  cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)
  
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijLinear(n = n, p = p, Y = Y, cutoffs = cutoffs, 
                              upperMeanScalers = ums, vscores = scores )
  
  rank.DCSIS = rep(0, p)
  omega = matrix(0, 1, p)
  
  dcov.y = DCSIS(Y,Y, n = n)
  for(j in 1:p)
  {
    X.j = matrix(covarsX_ij[, j])
    dcov.xy = DCSIS(X.j, Y, n = n)
    dcov.xx = DCSIS(X.j, X.j, n = n)
    dcorr = dcov.xy / sqrt(dcov.xx * dcov.y)
    omega[, j] = dcorr * dcorr      
  } 
  
  #sorted.DCSIS = p +1- rank(omega, ties.method = 'random')
  sorted.DCSIS = sort(omega, index.return = TRUE, decreasing = TRUE)
  
  
  first10 = prepForp_j(sorted.DCSIS$ix, 10)
  first11 = prepForp_j(sorted.DCSIS$ix, 11)
  first15 = prepForp_j(sorted.DCSIS$ix, 15)
  
  
  in.TrueModel = (sorted.DCSIS$ix <= 10)
  minModSz = max(which(in.TrueModel))
  
  return(list(minModSz, first10, first11, first15))
}

runSimDCSIS_Linear(n = 200, p = 3000, scores = c(0,2,6))

reps = 500
minModelSizes = vector(length = reps)
p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)

timestamp()##------ Tue Aug 30 15:13:01 2016 ------##
##------ Wed Aug 31 13:44:08 2016 ------##
for(w in 1:reps)
 {
    if(w %% 100 == 0)
      {print("Benchmark" )}
       
     newSeed = 7919 +20*w + w^2 - w^3
     
       simList = runSimDCSIS_Linear(n = 125, p = 3000, preseed = newSeed, scores =c(0,1,2))  
     
       minModelSizes[w] =  simList[[1]]  		
     p_j_forD10[w,] =  simList[[2]]			
     p_j_forD11[w,] =  simList[[3]]			
     p_j_forD15[w,] =  simList[[4]]			
     
  }
timestamp()

mean(minModelSizes)

DCSISminModelLinear = c(10, 10,10,10,10,10,10,10,10,10,11,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,14,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,
                        10,12,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,113,10,10,11,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,11,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,19,10,10,10,11,10,10,10,10,10,10,10,
                        10,11,10,12,10,10,10,10,10,10,10,10,10,10,12,10,10,10,10,10,10,10,10,10,10,42,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,14,10,10,10,10,10,10,10,10,10,10,10,10,16,10,
                        10,10,10,10,10)
mean(DCSISminModelLinear)

quantile(DCSISminModelLinear, probs =c(0.25,0.5, 0.9,0.95, 0.99))

get_p_a(minModSzs = DCSISminModelLinear, d = 10)
get_p_a(minModSzs = DCSISminModelLinear, d = 11)
get_p_a(minModSzs = DCSISminModelLinear, d = 12)
get_p_a(minModSzs = DCSISminModelLinear, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)


######-------------------------------------------------------------------------------------------------------------------------------
#########HUANG HUANG HUANG HUANG HUANG
######-------------------------------------------------------------------------------------------------------------------------------

runSimHuang_Linear <- function(n, p, preseed = 7919)
{
  set.seed(preseed)
  
  Y = makeYBinary(n, percent0 = 0.5)
  
  ums= c(2,2,2,2,2,1,1,1,1,1)
  
  vectorCutoffs = c(0, 2, 0.3088, 1.6912, 1, 1.832, 1, 2.283, 0.5866, 1.4134,
                    0.25,1,0,1,0.1,1,-0.2,1.2,0.213, 0.787)
  
  cutoffs = matrix(vectorCutoffs, nrow = 10,ncol = 2, byrow = TRUE)
  
  
  #covarsX_ij = makeX_ijHuang(n = n, p = p, trueSize = 10, Y = Y)
  covarsX_ij = makeX_ijLinear(n = n, p = p, Y = Y, cutoffs = cutoffs, upperMeanScalers = ums)
  
  
  
  theDeltas = makeAllDelta_j_Prefab(X_ij = covarsX_ij, Y = Y, n = n)
  
  
  sorted_Delta = sort(theDeltas, index.return = TRUE, decreasing = TRUE)
  
  
  first10 = prepForp_j(sorted_Delta$ix, 10)  
  first11 = prepForp_j(sorted_Delta$ix, 11)			
  first15 = prepForp_j(sorted_Delta$ix, 15)			
  
  in.TrueModel = (sorted_Delta$ix <= 10)			 
  
  minModSz = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model.		
  
  return(list(minModSz, first10, first11, first15))	
}

runSimHuang_Linear(n = 200, p = 3000)			


reps = 500	
minModelSizes = vector(length = reps)		
p_j_forD10 = matrix(data = -1, nrow = reps, ncol = 10)	
p_j_forD11 = matrix(data = -1, nrow = reps, ncol = 10)			
p_j_forD15 = matrix(data = -1, nrow = reps, ncol = 10)			

timestamp()##------ Tue Aug 30 18:18:23 2016 ------##
for(w in 1:reps)
{
  if(w %% 100 == 0)
  {print("Benchmark" )}
  
  newSeed = 7919 +20*w + w^2 - w^3
  simList = runSimHuang_Linear(n = 200, p = 3000, preseed = newSeed)
  
  minModelSizes[w] =  simList[[1]]			
  p_j_forD10[w,] =  simList[[2]]			
  p_j_forD11[w,] =  simList[[3]]			
  p_j_forD15[w,] =  simList[[4]]
}
timestamp()##------ Tue Aug 30 18:44:28 2016 ------##

mean(minModelSizes)#10.474
head(minModelSizes)
HuangminModelLinear = c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,11,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,11,10,19,10,10,10,10,10,10,10,10,10,10,10,10,10,12,13,10,10,10,10,10,10,10,12,10,10,10,27,10,10,10,11,10,10,10,10,31,10,10,10,10,
                        10,10,17,10,10,10,15,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,11,10,10,10,10,11,10,10,14,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,18,10,10,10,10,10,10,10,10,14,10,15,10,10,10,10,10,10,10,17,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,13,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,35,10,10,10,10,10,13,10,10,11,10,
                        10,10,10,10,10,10,10,11,10,10,10,10,10,14,11,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,12,11,12,10,10,10,10,10,10,
                        10,10,10,10,10,10,10,10,10,10,10,10,10,12,10,10,11,10,11,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,
                        10,34,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,10,10,17,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,14,10,
                        10,10,10,10,10,10,10,10,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,10,10,11,10,10,10,10,10,
                        11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,12,10,10,10,10,10,10,10,10,10,10,10,10,10,14,10,14,11,26,10,10,10,10,10,10,
                        10,10,10,14,10,10,10,10,10,10,10,10,11,10,10,10,10,10,16,10)

mean(HuangminModelLinear)


quantile(HuangminModelLinear, probs =c(0.25,0.75, 0.9, 0.95, 0.99))


get_p_a(minModSzs = HuangminModelLinear, d = 10)
get_p_a(minModSzs = HuangminModelLinear, d = 11)
get_p_a(minModSzs = HuangminModelLinear, d = 12)
get_p_a(minModSzs = HuangminModelLinear, d = 15)

get_p_j(p_j_forD10, p_j_forD11, p_j_forD15)

