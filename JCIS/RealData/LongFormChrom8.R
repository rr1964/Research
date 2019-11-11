#source("https://bioconductor.org/biocLite.R")

####install.packages(c("binomTools","feather", "readr"),lib=c("/uufs/chpc.utah.edu/common/home/u6011224/software/pkg/RLibs/3.3.2i"),
    ####              repos=c("http://cran.us.r-project.org"),verbose=TRUE)

source("Interact Functions.R")
library(snpStats)
library(R.utils)
library(base)
library(boot)
library(binomTools)
library(data.table)
library(dplyr)
library(magrittr)
library(feather) # v0.0.0.9000
library(readr) # v0.2.2

###Although this BIM table is only assciated with the Y = 1 group, the BIM table for the control (Y = 0) group is identical. 
###Because of this, I just use the BIM table from the Y = 1 group as the main BIM table. 
BIMTable = fread("NICHD_PolycysticOvary_c1_PCOS.bim", 
                 col.names = c("Chromosome", "ID","GeneticDist", "Position", "Allele1", "Allele2"))

BIMTable = BIMTable[,-3] ###Remove the GeneticDistance column. Has no information.

#dim(filter(BIMTable, Chromosome %in% 1:23, Position>0))###The "cleansed" table we want. 728286 by 5

cleanBIMTable = filter(BIMTable, Chromosome %in% 1:23, Position>0)

filter(cleanBIMTable, Chromosome == 8) %>%
  select(ID) ->
  name8

# genotypes
########Clean bed 1
bed1 = read.plink("NICHD_PolycysticOvary_c1_PCOS.bed", select.snps = name8[,1])
bed1 = bed1$genotypes

bed0 = read.plink("NICHD_PolycysticOvary_c2_NUGENE.bed", select.snps = name8[,1])
bed0 = bed0$genotypes

genotype = rbind(bed1, bed0) 


snpsum.col <- col.summary(genotype)
call <- 0.95 ###Why remove low call rate?
use <- with(snpsum.col, (!is.na(Call.rate) & Call.rate >= call))
use[is.na(use)] <- FALSE              
cat(ncol(genotype)-sum(use),"SNPs will be removed due to low call rate.\n") 
genotype <- genotype[,use]
snpsum.col <- snpsum.col[use,]

minor <- 0.1
use1 <- with(snpsum.col, (!is.na(MAF) & MAF > minor) )
use1[is.na(use1)] <- FALSE               
cat(ncol(genotype)-sum(use1),"SNPs will be removed due to low MAF .\n"  ) 
genotype <- genotype[,use1]
snpsum.col <- snpsum.col[use1,]

name8 = rownames(snpsum.col)###Update the chromsomes we are looking at. 


class(genotype) = "matrix"



###################################################################
###################################################################
###################################################################
###################################################################

###################################################################
###################################################################
###################################################################
###################################################################



###print("Line 39 executed")

X = apply(genotype, 2, as.numeric)

Y = c(rep(1, 1043), rep(0, 3056))



data = cbind(Y, X)

##rm(X)####delete X, it does not need to be reference again.

#smallData = data[,c("Y", name8[1:4000,1])]###Dimensions are off here. 
#dim(smallData)

# Iterative CA
timestamp()
R3LongPrint(data, names = name8, resultFileName = "chrom8LEAN.csv")
timestamp()


###which(result != 0, arr.ind = TRUE)
##fwrite(as.data.frame(result), file = "chrom8Short.csv", row.names = TRUE, col.names = TRUE)


