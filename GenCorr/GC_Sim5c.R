
### SIMULATION SIX

#Sim 6
#
# Interactive settings. With main effects. Pretty much SIM 5,, but now with main effects. 

#setwd("M:/GCorr")
source("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/GenCorrFunct.R")
#source("M:/GCorr/GenCorr Functions.R")
#print("pig")

library(data.table)
library(R.utils)
library(MASS)


#arg = list("FIRST", 132)
arg = cmdArgs()

n = 100
p = 1000
q = 4
d = 5

score = matrix(-1, nrow = 100, ncol = 8) ###These are the scores for each method.
#### Best rank, worst rank. Perfect score is 1,2.
###Looking at percentiles may be our best bet. The mean is ~not as good as we'd maybe want. 

colnames(score) = c("GCorr_Best", "GCorr_Worst", "X_1X_2", "X_3X_4",  "X_1", "X_2", "X_3", "X_4")

set.seed(arg[[2]])

for(r in 1:100)
{
  X = matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) 
  {
    X[, i] = rnorm(n, 0, 2)
    #X[, i] = rpois(n, 2)
  }
  
  Y = matrix(0, nrow = n, ncol = q)
  
  
  rho = 0.5
  Sigma = diag(q)
  Sigma = rho ^ abs(row(Sigma) - col(Sigma)) 
  
  scalarMat = t(mvrnorm(6, rep(3, q), Sigma))
  ##scalarMat = matrix(rnorm(18, 0,2.5), nrow = q, ncol = 3)
  
  for(m in 1:q)
  {
    
    Y[,m] = cbind(X[,c(1,2,3,4)], 3*X[,1]*X[,2], 3*X[,3]*X[,4]) %*% scalarMat[m,]
  }
  
  
  # Y[,1] = X[, 1]*X[,3] + 2*X[,4]*3*X[,5]
  #  Y[,2] = X[, 1]*X[,3] +  1.2*X[,4]*X[,5]
  #   Y[,3] = 3*X[, 1]*X[,3] - 2*X[,4]*X[,5]
  #   Y[,4] = 2*X[, 1]*-1*X[,3] +X[,4]*X[,5]
  #  # Y[,5] = -2*X[, 1] *-1*X[,3]# +X[,5]
  #  # Y[,6] = X[, 1]* -1.5*X[,3]# -2.5*X[,5]
  #  # 
  testData = cbind(Y, X)
  
  phi_num = CorrMat_MV_main(data = testData, q = q, normToUse = arg[[3]])
  phi_num2 = CorrMat_MV_Interact(data = testData, q = q, normToUse = arg[[3]])
  
  #timestamp()
  #omega = zllz(data = testData, q=q)
  #timestamp()
  
  #timestamp()
  #dc = DCSIS_MV(data = testData, q=q)
  timestamp()
  
  sorted_phi = sort.int(phi_num, decreasing  = TRUE, index.return = TRUE)
  
  sorted_phi_num2 = sort.int(phi_num2, decreasing  = TRUE, index.return = TRUE)
  

  ###Using a temp cutoff of d=5
  # selectedPairs = head(sorted_phi_num2$ix, d)
  # for(k in 1:d)
  # {
  #   j1 = (selectedPairs[k]-1) %% p + 1
  #   j2 = (selectedPairs[k]-1) %/% p + 1
  #   
  #   print(paste0("(", j1, ",", j2, ")"))
  #   print(selectedPairs[k])
  #   
  # }
  
  phi_num2MainScores = which(sorted_phi_num2$ix %in% c(1001,3003))
  X1X2Score = which(sorted_phi_num2$ix == 1001)
  X3X4Score = which(sorted_phi_num2$ix == 3003)  
  
  X1Score = which(sorted_phi$ix == 1)
  X3Score = which(sorted_phi$ix == 3)
  X2Score = which(sorted_phi$ix == 2)
  X4Score = which(sorted_phi$ix == 4)


  score[r, ] = c(phi_num2MainScores, X1X2Score, X3X4Score, X1Score, X2Score, X3Score, X4Score)
  
  
   if(r %% 25 == 0)
   {
     ##timestamp()
     print(paste0("We are at replicate ", r, " for Sim 6 run ", arg[[1]]))
     print("The column scores are ")
     print(colMeans(score[1:r,]))
     
     
   }
  
  
}

fileName = paste0("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/", "Sim6Results_", arg[[1]],".csv")

fwrite(as.data.frame(score), 
       file = fileName,
       showProgress = TRUE,
       col.names=TRUE)



