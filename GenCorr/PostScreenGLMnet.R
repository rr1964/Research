

library(data.table)
library(dplyr)
library(glmnet)
library(readr)


Y = fread("M:/GCorr/Real Data GenCorr/pheno_MiceImputed.csv",  sep = ",", header = TRUE)
flippedSNPs = fread("M:/GCorr/Real Data GenCorr/flippedSNPs.csv",  sep = ",", header = TRUE)


Y = fread("H:/GCorr/Real Data GenCorr/pheno_MiceImputed.csv",  sep = ",", header = TRUE)
flippedSNPs = fread("H:/GCorr/Real Data GenCorr/flippedSNPs.csv",  sep = ",", header = TRUE)

flippedSNPs[1:10, 1:5]

Y= as.matrix(Y)

jumboCSV = read_csv("H:/GCorr/Real Data GenCorr/jumboCSV.csv")
finalMargEffects = read_csv("H:/GCorr/Real Data GenCorr/finalMargEffects.csv")

jumboCSV = read_csv("M:/GCorr/Real Data GenCorr/jumboCSV.csv")
finalMargEffects = read_csv("M:/GCorr/Real Data GenCorr/finalMargEffects.csv")


n = 288
q = 7

d = 2*round(n/log(n), digits = 0)

topInteract = head(arrange(jumboCSV,desc(Phi)), n = 102)
#We just take the top 102 interactions. 
#There is no precendent for how to really run an interative approach here.

table(topInteract$SNP_1) #77 Unique SNPs
length(table(topInteract$SNP_1)) #77 unique SNPs.

table(topInteract$SNP_2) #13 Unique SNPs
length(table(topInteract$SNP_2)) #13 unique SNPs.

table(finalMargEffects$SNP)
length(table(finalMargEffects$SNP)) #102 unique SNPs.


which(topInteract$SNP_1 %in% finalMargEffects$SNP) ## None

which(topInteract$SNP_2 %in% finalMargEffects$SNP) ## None

length(table(c(topInteract$SNP_2, finalMargEffects$SNP))) ## 115. 
##Confirms the above results.

## So we have no marginal effects that appear in the interactive effects.
## oh Well. And no need to mention this. 

length(table(c(topInteract$SNP_2, topInteract$SNP_1))) ## 83. 
## So 83 total SNPs account for 102 interactions. 
## We could have as many as 204 unique SNPs if every interaction had two 
##  previously unseen SNPs.

interactProduct = matrix(-1, nrow = n, ncol = d)

for(i in 1:d)
{
  ##print(paste(topInteract$SNP_1[i], topInteract$SNP_2[i]))
  
  SNP_One_Index = which(colnames(flippedSNPs) == topInteract$SNP_1[i])
  
  SNP_Two_Index = which(colnames(flippedSNPs) == topInteract$SNP_2[i])
  
  interactProduct[,i] = 
    as.matrix(dplyr::select(flippedSNPs, SNP_One_Index)
              *dplyr::select(flippedSNPs, SNP_Two_Index))
}

marginals = matrix(-1, nrow = n, ncol = d)

for(i in 1:d)
{

  SNP_One_Index = which(colnames(flippedSNPs) == topInteract$SNP_1[i])
  
  
  marginals[,i] = 
    as.matrix(dplyr::select(flippedSNPs, SNP_One_Index))
}



fullData = cbind(marginals, interactProduct)

countNonZeroCoef <- function(coefVect, d = 102)
{
  #Remember to account for the placeholder for the intercept.
  marginalCount = sum(abs(coefVect[2:(d+1)]) > 0)
  interactCount = sum(abs(coefVect[(d+2):(2*d+1)]) > 0)
  
  print(paste("Marginal Features:", marginalCount))
  print(paste("Interact Features:", interactCount))
  print(paste("TotalNum Features:", marginalCount + interactCount))
  print("----------------------------------------------------------")
  
  return(c(marginalCount, interactCount, marginalCount+interactCount))
  
}

testVect = c(1, 0,2,3, 1, 9,11,0,1)

countNonZeroCoef(testVect, d = 4)

#*******************************************************************************
## alpha = 0.40 ################################################################
#*******************************************************************************
harold =  cv.glmnet(x = fullData, y = Y, family = "mgaussian",
                    alpha = 0.40, type.measure = "mse", nfolds = 10,
                    standardize.response = TRUE, intercept = FALSE)

minIndex = which(harold$lambda == harold$lambda.min)
harold$cvm[minIndex] ##This is to get the mean 10-fold CV MSE. 

#plot(harold)
#harold$lambda.min

coefficients0_40 = coef(harold, s = "lambda.min")

roundedCoeff = sapply(coefficients0_40,round, digits = 8)

sapply(roundedCoeff, countNonZeroCoef, d = 102)

sum((roundedCoeff$SBP != 0)*(roundedCoeff$DBP != 0)*(roundedCoeff$MAP != 0)*(roundedCoeff$HDL != 0)*
  (roundedCoeff$CHL != 0)*(roundedCoeff$TRI != 0)*(roundedCoeff$GLU != 0))


###So we need to select a candidate model. I feel that lambda.min will be the simplest. 

fitHarold = harold$glmnet.fit

tLL = fitHarold$nulldev - deviance(fitHarold)
### deviance() returns the deviance value for each of the 100 lambdas used. 

k = fitHarold$df
n = 288
q = 7
correction = 2*n*(q*k + q*(q+1)/2)/(n - (k + q+1))
sAICc = -tLL+ correction ###A vector of the AICc values. 
##We then just need to select which model (lambda) we are using and pull out the associated sAICc

##which(sAICc == min(sAICc))
chosenModel = which(fitHarold$lambda == harold$lambda.min)

sAICc[chosenModel]/1000

#*******************************************************************************
## alpha = 0.80 ################################################################
#*******************************************************************************
harold =  cv.glmnet(x = fullData, y = Y, family = "mgaussian",
                    alpha = 0.8, type.measure = "mse", nfolds = 10,
                    standardize.response = TRUE, intercept = FALSE)

minIndex = which(harold$lambda == harold$lambda.min)
harold$cvm[minIndex] ##This is to get the mean 10-fold CV MSE. 

#plot(harold)
#harold$lambda.min

coefficients0_80 = coef(harold, s = "lambda.min")

roundedCoeff = sapply(coefficients0_80,round, digits = 8)

sapply(roundedCoeff, countNonZeroCoef, d = 102)

sum((roundedCoeff$SBP != 0)*(roundedCoeff$DBP != 0)*(roundedCoeff$MAP != 0)*(roundedCoeff$HDL != 0)*
      (roundedCoeff$CHL != 0)*(roundedCoeff$TRI != 0)*(roundedCoeff$GLU != 0))

fitHarold = harold$glmnet.fit

tLL = fitHarold$nulldev - deviance(fitHarold)
### deviance() returns the deviance value for each of the 100 lambdas used. 

k = fitHarold$df
n = 288
q = 7
correction = 2*n*(q*k + q*(q+1)/2)/(n - (k + q+1))
sAICc = -tLL+ correction ###A vector of the AICc values. 
##We then just need to select which model (lambda) we are using and pull out the associated sAICc

##which(sAICc == min(sAICc))
chosenModel = which(fitHarold$lambda == harold$lambda.min)

sAICc[chosenModel]/1000


#*******************************************************************************
## alpha = 1 LASSO #############################################################
#*******************************************************************************
harold =  cv.glmnet(x = fullData, y = Y, family = "mgaussian",
                    alpha = 1, type.measure = "mse", nfolds = 10,
                    standardize.response = TRUE, intercept = FALSE)

minIndex = which(harold$lambda == harold$lambda.min)
harold$cvm[minIndex] ##This is to get the mean 10-fold CV MSE. 


coefficientsLASSO = coef(harold, s = "lambda.min")

roundedCoeff = sapply(coefficientsLASSO,round, digits = 8)

sapply(roundedCoeff, countNonZeroCoef, d = 102)

sum((roundedCoeff$SBP != 0)*(roundedCoeff$DBP != 0)*(roundedCoeff$MAP != 0)*(roundedCoeff$HDL != 0)*
      (roundedCoeff$CHL != 0)*(roundedCoeff$TRI != 0)*(roundedCoeff$GLU != 0))

fitHarold = harold$glmnet.fit

tLL = fitHarold$nulldev - deviance(fitHarold)
### deviance() returns the deviance value for each of the 100 lambdas used. 

k = fitHarold$df
n = 288
q = 7
correction = 2*n*(q*k + q*(q+1)/2)/(n - (k + q+1))
sAICc = -tLL+ correction ###A vector of the AICc values. 
##We then just need to select which model (lambda) we are using and pull out the associated sAICc

##which(sAICc == min(sAICc))
chosenModel = which(fitHarold$lambda == harold$lambda.min)

sAICc[chosenModel]/1000


