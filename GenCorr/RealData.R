

## Real Data Analysis. 

### This sure seems to be the correct data. 228 mice. 44,428 unique SNP genotypes. 
### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3284324/
### I'm about 99% sure that this is the dataset. everything matches exactly. 
### http://cgd.jax.org/datasets/phenotype/nmri.shtml

library(data.table)
library(dplyr)
library(tidyr)

#if (!require("tibble")){
#  install.packages("tibble")
#} 

library(tibble)


# if (!require("VIM")){
#   install.packages("VIM")
# } 
# library(VIM)

# if (!require("mice")){
#   install.packages("mice")
#  } 

library(mice)

source("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/GenCorrFunct.R")

#pheno_raw <- fread("h:/GCorr/Real Data GenCorr/pheno_raw.csv") ###Pheno_raw is the responses.
#snps_dat_collapsed <- fread("h:/GCorr/Real Data GenCorr/snps.dat.collapsed.csv") ##snps are the predictors. 


### pheno_raw has the following names:
# PedNum is the unique identifier for a subject (i.e. one mouse)
# SBP  systolic blood pressure
# DBP  diastolic blood pressure
# MAP  mean arterial pressure
# ACR  urinary albumin-to-creatinine ratio
# HDL  high-density lipoprotein cholesterol levels
# CHL  total cholesterol levels 
# TRI  triglyceride
# GLU  glucose levels

#snps_dat_collapsed <- fread("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Real Data GenCorr/snps.dat.collapsed.csv")
#pheno_raw <- fread("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Real Data GenCorr/pheno_raw.csv")

#snps_dat_collapsed <- fread("M:/GCorr/Real Data GenCorr/snps.dat.collapsed.csv")
#pheno_raw <- fread("M:/GCorr/Real Data GenCorr/pheno_raw.csv")


#snps_dat_collapsed <- fread("~/GenCorr/snps.dat.collapsed.csv")
#pheno_raw <- fread("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/pheno_raw.csv")
pheno_raw <- fread("M:/GCorr/Real Data GenCorr/pheno_raw.csv")


###ACR will be thrown out in the end due to almost all the values being 0. 

sum(pheno_raw$ACR == 0, na.rm = TRUE)/288 ##89.93056%. So essentially 90% of the mice have ACR == 0. 

#snps_dat_collapsed = as_tibble(snps_dat_collapsed)
pheno_raw = as_tibble(pheno_raw[,c("SBP", "DBP", "MAP", "HDL", "CHL", "TRI", "GLU")])
## Omit ACR and PedNum. Neither is important in the numerical analysis. 

#head(pheno_raw) 

#proportionMissing <- function(x){sum(is.na(x))/length(x)}
#apply(pheno_raw, MARGIN = 2, proportionMissing) ####
#sum(apply(snps_dat_collapsed, MARGIN = 2, proportionMissing) > 0) 
###Nothing is missing in the SNPs data. 

###Observing missingness using the mice package. 

#md.pattern(pheno_raw)

#aggr_plot <- aggr(pheno_raw, col=c('navyblue','red'),
 #                 numbers=TRUE, sortVars=TRUE,
  #                labels=names(data), cex.axis=.7,
   #               gap=3, ylab=c("Histogram of missing data","Pattern"))

#marginplot(pheno_raw[c(3,2)])

tempData <- mice(pheno_raw,m=5,maxit=5,meth='pmm',seed=500)

#stripplot(tempData, pch = 20, cex = 1.2) ###Shows what the data with imputations looks like. 
### 0 is the original data set. 1,2,3,4,5 are the five data sets WITH the imputations. 

#tempData$imp$SBP
#tempData$imp$DBP

pheno_raw = complete(tempData,1)###Impute with the first imputation. 


#### Summarizing the response data. 

#summarise_all(pheno_raw, funs(mean))

Y = pheno_raw[,c("SBP", "DBP", "MAP", "HDL", "CHL", "TRI", "GLU")] ###Create The Y Part of the Data.
head(Y)

fwrite(as.data.table(Y), file = "M:/GCorr/Real Data GenCorr/pheno_MiceImputed.csv")
Y = fread("M:/GCorr/Real Data GenCorr/pheno_MiceImputed.csv")

###The SNPs data needs to be transposed so that each individual is housed in a row, and the SNPs are the columns. 
#head(snps_dat_collapsed)

#length(snps_dat_collapsed$probeset_id)

#flippedSNPs = t(snps_dat_collapsed[,5:292])

#colnames(flippedSNPs) = snps_dat_collapsed$probeset_id

#fwrite(as.data.table(flippedSNPs), file = '~/GenCorr/flippedSNPs.csv', sep = ",", col.names = TRUE)

#flippedSNPs = fread(file = '/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/flippedSNPs.csv', sep = ",", header = TRUE)

flippedSNPs = fread("M:/GCorr/Real Data GenCorr/flippedSNPs.csv",  sep = ",", header = TRUE)

flippedSNPs[1:10, 1:5]

############################################

q = 7

realData = cbind(Y, flippedSNPs)

phi_num2 = CorrMat_MV_main(data = realData, q = q, normToUse = 2)

Phi = CorrMat_MV_Interact(data = realData, q = q, normToUse = 2)
## We'll need to figure out a way to record just the top 10,000 or so. 
## Too big to store all of this perhaps.

mainEff = cbind(colnames(flippedSNPs), phi_num2)

colnames(mainEff) = c("SNP", "phi_j")

fwrite(as.data.table(mainEff), file = "/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/mainEffectsRealDOS.csv", sep = ",")

colnames(Phi) = colnames(flippedSNPs)
rownames(Phi) = colnames(flippedSNPs)

fwrite(as.data.table(Phi), file = "/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/interactEffectsRealDOS.csv",
       sep = ",", row.names = TRUE, col.names = TRUE)

