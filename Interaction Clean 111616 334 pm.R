##############################
##############################
####Interaction Screening CA Trend. 

###Data Creation for CA Trend. Y binary, X_j trinary. 
library(boot)


makeYBinary <- function(n, percent0 = 0.5)
{
  sample(c(0,1),n,replace = TRUE, prob = c(percent0,1-percent0))###These are the Y's
  
}



############################################################################################
############################################################################################
# Here we are going to generate X_ij data that is very similar to that found in simulation 3 of Huang.
# In their simulation, Y has 4 levels, but instead I am going to only let Y have two levels and use theta_kj
# from the final two lines of Table 3 of Huang. (So for k = 3, 4). 

makeX_ijForInteraction <- function(n,p, numInteract = 4, Y) 
{
  
  probability = c(0.7, 0.9, 0.1, 0.1,
                  0.2, 0.1, 0.9, 0.7)####Table 3
  
  table3 = matrix(data =probability , 2,4, byrow = TRUE)
  
  
  X_ij = matrix(nrow = n, ncol = p)
  ##The X_js are stored here for all n-many replicates.
  ####Initialized to -1 for ease of locating missed assignments.
  
  for(j in 1:numInteract)
  {
    
    table3Vals = table3[Y+1,j]###If Y = m, the (m+1)th row of the jth column is the probability theta_mj given Y = m. 
    ###This collects the requisite table values and puts them in a vector. 
    X_ij[,2*j-1] = sapply(table3Vals, rbinom, n = 1, size = 2) 
    ###We define a whole column here! Each entry takes on the value of 0, 1, or 2. 
    #print(table3Vals)
    
    
    #####This next part is a bit tricky. We need to create a table of probabilities that are conditional
    ##### on the value of X_{2j-1} AND on the probability theta_mj (which corresonds to the value of Y_i).
    ##### This makes our probabilities for generating X_2j. 
    conditionalProbs = (0.05*(table3Vals >= 0.5) + 0.8*(table3Vals < 0.5))*(X_ij[,2*j-1] == 0) +
      (0.95*(table3Vals >= 0.5) + 0.8*(table3Vals < 0.5))*(X_ij[,2*j-1] == 1) +
      (0.2*(table3Vals >= 0.5) + 0.65*(table3Vals < 0.5))*(X_ij[,2*j-1] == 2)#
    
    ######
    #print(j)
    #print(conditionalProbs)
    ####We have a 1 by n vector where each row represents the probability of "success" in the random binomial trial process 
    ####   used to determine the value of X_i(2j). So index i holds the probability of 'success' for the ith trial.  
    
    X_ij[,2*j] = sapply(conditionalProbs, rbinom, n = 1, size = 2) 
    
    ###We define a whole column here! Each entry takes on the value of 0, 1, or 2. 
    
  }
  
  
  for(j in (2*numInteract+1):p)
  {
    X_ij[,j] = sample(c(0,1,2), n, replace= TRUE, prob = rep(0.333333,3))
  }
  
  return(X_ij)
}

set.seed(79117)
Ysamp = makeYBinary(n = 10)

testX = makeX_ijForInteraction(n = 10, p = 10, numInteract = 4, Y = Ysamp)
Ysamp
testX




#####################################################################################
#####################################################################################
### Find Numerators and denominators. 
#####################################################################################
#####################################################################################

makeNumerators <- function(X_ij, Y, p )
{
  ######In so far as I can see right now, this cannot be vectorized. 
  
  
  numerators = matrix(nrow= p, ncol = p)
  for(j_1 in 1:p)
    for(j_2 in 1:p)
      if(j_1 < j_2)
      {
        numerators[j_1, j_2] = cum3(a = X_ij[,j_1], b = X_ij[,j_2], c = Y, unbiased = FALSE)
      }
  
  return(numerators)
}

Ysamp = makeYBinary(n = 200, percent0 = 0.5)
#Ysamp

Xcovars =makeX_ijForInteraction(n = 200, p = 2000, numInteract = 4, Y = Ysamp)
#Xcovars

timestamp()##------ Wed Nov 16 15:54:04 2016 ------##
numerators = makeNumerators(X_ij = Xcovars, Y = Ysamp, p = 2000)
timestamp()##------ Wed Nov 16 15:55:15 2016 ------##
###Making the numerators takes about 1 minute. (p = 2000) 
###
numerators[1:13, 1:13]



#####A very simple method for getting the covariance approximation used by Agresti (in an equivalent form). 
#####In the proof of consitency of the estimator given in Agresti for the covariance, I show that (n-1)/n*cov(X, Y)
##### is equivalent to that estimator given in Agresti. 
covarApprox<-function(X1, X2 = X1, n)
{
  
  biasedCovar = (n-1)/n * cov(x = X1, y = X2, use = "all.obs", method = "pearson")
  return(biasedCovar)
  
}

sdApprox<-function(x, n)
{
  
  biasedSD = (n-1)/n * sd(x, na.rm = TRUE)
  return(biasedSD)
  
}

# make_COVCOVDemominators <- function(X_ij, Y, p)
# {#####MUCH TOO SLOW. 
#   denoms = matrix(nrow= p, ncol = p)
#   for(j_1 in 1:p)
#     for(j_2 in 1:p)
#       if(j_1 < j_2)
#       {
#         denoms[j_1, j_2] = covarApprox(X1 = X_ij[,j_1], X2 = Y)*covarApprox( X1 = X_ij[,j_2], X2 = Y)
#       }
# 
#   return(denoms)####This is a matrix of dimension p by p. We have each cov(X_j, Y)*cov(X_k, Y) in an entry
# 
# }

make_COVCOVDemominators <- function(X_ij, Y, p, n)
{###This vectorized method is MUCH faster. 
  #denoms = matrix(nrow= p, ncol = p)
  
  ####Think of some way to do a matrix computation here to maybe speed this up. 
  covars = apply(X_ij, MARGIN = 2, covarApprox, X2 = Y, n = n )
  covarMat = matrix(covars, nrow = p, ncol = p)
  denoms = covarMat * t(covarMat)
  
  
  return(denoms)####This is a matrix of dimension p by p. We have each cov(X_j, Y)*cov(X_k, Y) in an entry
  
}

make_SDSDSDDemominators <- function(X_ij, Y, p, n)
{
  stdevs = apply(X_ij, MARGIN = 2, sdApprox, n= n)
  sdMat = matrix(stdevs, nrow = p, ncol = p)
  denoms = covarMat * t(sdMat)
  
  return(denoms)####This is a matrix of dimension p by p. We have each sd(X_i)*sd(X_j)*sd(Y) in an entry
  
}

Ysamp = makeYBinary(n = 200, percent0 = 0.5)
#Ysamp

Xcovars =makeX_ijForInteraction(n = 200, p = 2000, numInteract = 4, Y = Ysamp)

timestamp()##------ Wed Nov 16 15:54:04 2016 ------##
numerators = makeNumerators(X_ij = Xcovars, Y = Ysamp, p = 2000)
timestamp()##------ Wed Nov 16 15:55:15 2016 ------##
####A little over a minute (p = 2000)

#covars = apply(Xcovars, MARGIN = 2, covarApprox, X2 = Ysamp)
#covarMat = matrix(covars,nrow = 2000, ncol = 2000)
#covarMat

#denoms = covarMat * t(covarMat)

timestamp()
denoms = make_COVCOVDemominators(X_ij = Xcovars, Y = Ysamp, p = 2000, n = 200)
timestamp()


denoms[1:13, 1:13]


calculateR_j_1j_2 <- function(denom, cummulant_3_way, d)
{###d is the fixed number of X_j that we retained from the main effect test. 
  ###This preassumes that we have already found the covariances.
  ###The computationual requirements are such that we can perhaps only look at the top 40 or so most significant main effects
  ### for a significant interaction. 
  ### The denominators and numerators need to be the top d significant main effects. These already need to be selected.
  
  denom[denom < 0.00001] = 0.01
  
  
  R_j_1j_2 = abs(cummulant_3_way/denom)
  
  return(R_j_1j_2)####A d by d matrix. All values ar non-negative. We are only looking at magnitude. 
  
}

bigR = calculateR_j_1j_2(denom = denoms, cummulant_3_way = numerators, d = 2000)




calculateNEWR_j_1j_2 <- function(oldNumerators, cummulant_3_way, d)
{###d is the fixed number of X_j that we retained from the main effect test. 
  ###This preassumes that we have already found the covariances.
  ###The computationual requirements are such that we can only look at the top 40 or so most significant main effects
  ### for a significant interaction. 
  ### The old Numerators need to be the top d significant main effects. These already need to be selected.
  
  ######THIS IS WHERE WE HAVE TO CHANGE. MAKE THE DENOMINATORS DIFFERENT THAN THE ORIGINAL IDEA.
  ###THREE WAY CUMULANT OR VARIANCE.The Covariance is a bunch of head cheese. 
  covarProdMat = matrix(oldNumerators, nrow = d, ncol = d)###Filled by column
  newDenom = covarProdMat*t(covarProdMat)
  
  
  
  R_j_1j_2 = abs(cummulant_3_way/newDenom)
  R_j_1j_2[is.na(R_j_1j_2)] = 0 ####Set the NAs all to zero. This smooths out sorting problems. 
  #####The sort() function cannot return indices AND remove NAs for some reason. 
  
  return(R_j_1j_2)####A d by d matrix. All values ar non-negative. We are only looking at magnitude. 
  
}


bigR = calculateR_j_1j_2(denom = denoms, cummulant_3_way = numerators, d = 2000)

sortedR = sort(bigR, index.return = TRUE, decreasing = TRUE)####The sorted array is COLUMNWISE.?????? 

columnNums = (head(sortedR$ix, n = 10)-1) %/%2000 + 1
rowNums = (head(sortedR$ix, n = 10)-1) %%2000 + 1
columnNums
rowNums
rbind(rowNums, columnNums)



#####This extracts the values in the Rj_1j_2 matrix at the specified indices.
#####We use the fact that bigR is symmetric. 
##### For some reason the sort function returns indices that are row-wise. ???? That is the only thing I can figure. 
for(index in 1:10)
{
  if(rowNums[index]<= columnNums[index])
  {
    print(bigR[rowNums[index], columnNums[index]])
  }
  else
  {
    print(bigR[columnNums[index],rowNums[index]])
    print("Flipped index")
  }
}


prepForp_j <-function(sortedIndicies, d)
{
  firstDfeatures = sortedIndicies[1:d]
  containsEach = 1:8 %in% firstDfeatures
  return(containsEach)
}

#### We report the minimum model size required to contain all true predictors.
###################################################################################################
###################################################################################################
# This is where we screen for interaction. The run time on this could be long. 

#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__
# \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / _
#  \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ / 
#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__
# \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / _
#  \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ / 
#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__
# \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / _
#  \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ / 
#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__
# \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / _
#  \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ / 
#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__
# \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / _
#  \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ / 
#  / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \__/ / __ \ \_
# / /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \____/ /  \ \__


############################################################
###This is where we screen for interaction. 
############################################################

findTrueModelIndices <- function(row_col_mat = matrix(1:8, nrow =2, ncol = 4) , p)
{
  indices = apply(row_col_mat, 2, function(rowCol){return(c(rowCol[1] + p*(rowCol[2]-1)))})#,rowCol[2] + p*(rowCol[1]-1)))})
  ###This bit commented out is if the sort function is not able to ignore NA.
  
  
  return(indices)#####This is currently a vector. Assuming that the sort function does what I want, this will spit out the 
                ####### VECTOR indices of the true interactions. 
}

rawIndex = matrix(1:8, nrow =2, ncol = 4)
rawIndex

trueindex = findTrueModelIndices(p = 500)
c(503, 501, 1005) %in% trueindex
###########################


runSimCATrend_Interaction <- function(n, p, preseed = 7919, trueMod_Indices)
{
  set.seed(preseed)
  
  #print(trueMod_Indices)
  
  Y = makeYBinary(n, percent0 = 0.5)
  
  
  covarsX_ij = makeX_ijForInteraction(n = n, p = p, numInteract =  4, Y = Y)
  
 
  
  numerators = makeNumerators(X_ij = covarsX_ij, Y = Y, p = p)
  
  denominators  = make_COVCOVDemominators(p= p, Y = Y, X_ij = covarsX_ij , n = n) 
  
  
  bigRmatrix = calculateR_j_1j_2(denom = denominators, cummulant_3_way = numerators, d = p) ###Throw no possibilities out
  
  #####I might/should also run some simulations that drop some covariates. (Like only keep the top 100 or something)
  
  sortedR = sort(bigRmatrix, index.return = TRUE, decreasing = TRUE)####The sorted array is COLUMNWISE.?????? Yes. I believe so. 
  
  #sortedR = sort(numerators, index.return = TRUE, decreasing = TRUE)
  
  #print(c("Larget value", max(abs(bigRmatrix), na.rm = TRUE)))
  
  
  in.TrueModel = (sortedR$ix %in% trueMod_Indices)#####True mod indices is passed in as a prameter. 
  
  minModelSize = max(which(in.TrueModel))#####This finds the smallest model size required to include the true model. 
  
  return(minModelSize)
}

runSimCATrend_Interaction(n = 50, p = 300, preseed = round(100*runif(n = 1, min = 0, max = 20)), trueMod_Indices =  findTrueModelIndices(p = 300))

reps = 100
minModelSizes = vector(length = reps)

trueIndex = findTrueModelIndices(p = 3000)

timestamp()##------ Wed Nov 16 17:24:38 2016 ------##
for(w in 1:reps)
{
  if(w %% 25 == 0)
  {print("Benchmark" )}
  newSeed = 7919 +20*w + w^2 - w^3
  
  simList = runSimCATrend_Interaction(n = 200, p = 3000, preseed = newSeed, trueMod_Indices = trueIndex)
  
  minModelSizes[w] =  simList
  #p_j_forD8[w,] =  simList[[2]]
  #p_j_forD10[w,] =  simList[[3]]
  #p_j_forD12[w,] =  simList[[4]]
}
timestamp()##------ Thu Nov 17 11:19:01 2016 ------##

mean(minModelSizes)#3729128
#3000^2-3729128 = 5270872, so a little less than half way in is the final true effect. 




