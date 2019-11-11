

###Long Form interaction printing.

###Going one pair at a time and printing it out if it is above a certain cutoff.

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(R.utils)

library(mice)

source("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/GenCorrFunct.R")

#arg = list(1, 3)

arg = cmdArgs()


pheno_raw <- fread("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/pheno_raw.csv")


###ACR will be thrown out in the end due to almost all the values being 0. 

sum(pheno_raw$ACR == 0, na.rm = TRUE)/288 ##89.93056%. So essentially 90% of the mice have ACR == 0. 

#snps_dat_collapsed = as_tibble(snps_dat_collapsed)
pheno_raw = as_tibble(pheno_raw[,c("SBP", "DBP", "MAP", "HDL", "CHL", "TRI", "GLU")])
## Omit ACR and PedNum. Neither is important in the numerical analysis. 


tempData <- mice(pheno_raw,m=5,maxit=5,meth='pmm',seed=500)


pheno_raw = complete(tempData,1)###Impute with the first imputation. 


#### Summarizing the response data. 

#summarise_all(pheno_raw, funs(mean))

Y = pheno_raw[,c("SBP", "DBP", "MAP", "HDL", "CHL", "TRI", "GLU")] ###Create The Y Part of the Data.
head(Y,3)


flippedSNPs = fread(file = '/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/flippedSNPs.csv', 
                    sep = ",", header = TRUE)



############################################

q = 7

#splitTable = matrix(c(1,11107,11108, 22214, 22215,33321,33322, 44428), nrow = 4, ncol = 2, byrow = TRUE)
#splitTable = matrix(c(1,200,201, 400, 401,600,33322, 44428), nrow = 4, ncol = 2, byrow = TRUE)
splitTable = matrix(c(11108, 16660, 16661, 22214, 22215,27767, 27768, 33321,33322,38874, 38875, 44428), nrow = 6, ncol = 2, byrow = TRUE)
# splitONE = flippedSNPs[,1:11107]
# splitTWO = flippedSNPs[,11108:22214]
# splitTHREE = flippedSNPs[,22215:33321]
# splitFOUR = flippedSNPs[,33322:44428]

lower1 = splitTable[arg[[1]],1]
upper1 = splitTable[arg[[1]],2]

lower2 = splitTable[arg[[2]],1]
upper2 = splitTable[arg[[2]],2]

X1 = flippedSNPs[,lower1:upper1]
X2 = flippedSNPs[,lower2:upper2]

if(arg[[1]] == arg[[2]])
{
  realData = cbind(Y, X1)
  Phi = CorrMat_MV_Interact_WithinGroup(data = realData, q = q, normToUse = 2,
                                        names = colnames(X1))
  
  fileName = paste0("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/Real_S"
                    ,arg[[3]], "_S", arg[[4]], ".csv")
  
  fwrite(as.data.frame(Phi), file = fileName, sep = ',', col.names = TRUE)
  
} else
{
  data = cbind(Y, X1, X2)
  Phi = CorrMat_MV_Interact_SeparateGroup(data = data, q = q, normToUse = 2,
                                        namesX1 =  colnames(X1), namesX2 = colnames(X2))
  
  fileName = paste0("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/Real_S"
                    ,arg[[3]], "_S", arg[[4]], "X.csv") #The X is prevent accidental overwrite. 
  
  fwrite(as.data.frame(Phi), file = fileName, sep = ',', col.names = TRUE)
}





