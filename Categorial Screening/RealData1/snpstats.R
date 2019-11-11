
source("https://bioconductor.org/biocLite.R")
biocLite("snpStats")

source("functions.R")
library(snpStats)

# genotypes
bed1 = read.plink("36417/NICHD_PolycysticOvary_c1_PCOS.bed")
bed1 = bed1$genotypes
class(bed1) = "matrix"

bed2 = read.plink("36419/NICHD_PolycysticOvary_c2_NUGENE.bed")
bed2 = bed2$genotypes
class(bed2) = "matrix"

genotype = rbind(bed1, bed2)
X = apply(genotype, 2, as.numeric)
Y = c(rep(1, 1043), rep(0, 3056))
data = cbind(Y, X)

#CAres = CATrend(data)
#result = matrix(NA, 731442, 1)
#result[, 1] = CAres
#names(result) = "Omega(CA-Trend)"
#write.csv(result, "result.csv", row.names = FALSE)

# Choose p1 (191)
set.seed(10000000)
P1 = chooseP1(data) #191

# Iterative CA
result = CAISIS(data, p1 = P1) ##P1 = 191
