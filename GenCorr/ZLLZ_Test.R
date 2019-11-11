
### SIMULATION ZLLZ TEST

#Sim ZLLZ test. 


source("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/GenCorrFunct.R")
#setwd("M:/GCorr")
#source("GenCorr Functions.R")
#print("pig")

library(data.table)
library(R.utils)
library(MASS)

arg = cmdArgs()
arg = list("FIRST", 890)

n = 200
p = 2000
q = 1

score = matrix(-1, nrow = 200, ncol = 10) ###These are the scores for each method.
#### Best rank, mid rank, worst rank. Perfect score is 1,2,3.
colnames(score) = c("GCorr_Best","Dos", "GCorr_Mid","Cuatro", "GCorr_Worst", 
                    "ZLLZ_Best","Dos", "ZLLZ_Mid","Cuatro", "ZLLZ_Worst")#,
                    #"DC_Best", "DC_Mid", "DC_Worst")

set.seed(arg[[2]])

for(r in 1:200)
{
  ##X = matrix(NA, nrow = n, ncol = p)
  # for (i in 1:p) 
  # {
  #   X[, i] = rnorm(n, 2, 5)
  #   #X[, i] = rpois(n, 2)
  # }
  # 
  
  rho = 0.8
  Sigma = diag(p)
  Sigma = rho ^ abs(row(Sigma) - col(Sigma)) 
  
  X = mvrnorm(n, rep(0,p), Sigma = Sigma)
  Y = 0.5* X[,1:5] %*% as.matrix(c(1,0.8,0.6,0.4,0.2)) + sqrt(6.83)*rnorm(n, 0,1)
  
  
  testData = cbind(Y, X)
  
  #testData = matrix(rnorm(300, 0,1), ncol = 30, nrow = 10)
  #phi = CovarProd_MV_main(data = testData, q = q)
  #phi_num2 = CorrMat_MV_main(data = testData, q = q)
  
  timestamp()
  omega = zllz(data = testData, q=q)
  timestamp()
  
  #timestamp()
  #dc = DCSIS_MV(data = testData, q=q)
  ##timestamp()
  # 
  # head(phi)
  # sorted_phi = sort.int(phi, decreasing  = TRUE, index.return = TRUE)
  # head(sorted_phi$ix, 10)
  # phiMainScores = which(sorted_phi$ix %in% c(1,3,5))
  
  #head(phi_num2)
  #sorted_phi_num2 = sort.int(phi_num2, decreasing  = TRUE, index.return = TRUE)
  #head(sorted_phi_num2$ix, 10)
  #phi_num2MainScores = which(sorted_phi_num2$ix %in% c(1,2,3,4,5))
  
  # head(omega)
  sorted_omeg = sort.int(omega, decreasing  = TRUE, index.return = TRUE)
  # head(sorted_omeg$ix, 30)
  omegaMainScores = which(sorted_omeg$ix %in% c(1,2,3,4,5))
  
  #head(dc)
  #sorted_dc = sort.int(dc, decreasing  = TRUE, index.return = TRUE)
  #head(sorted_dc$ix, 30)
  #dcMainScores = which(sorted_dc$ix %in% c(1,3,5))
  
  #score[r, 1:5] = phi_num2MainScores
  score[r, 6:10] = omegaMainScores
  #score[r, 7:9] = dcMainScores
  
  if(r %% 5 == 0)
  {
    timestamp()
    print(paste0("We are at replicate ", r))
    print("The column scores are ")
    print(colMeans(score[1:r,]))
    
  }
  
  
}

fileName = paste0("M:/GCorr/ZLLZ_output.csv")
#fileName = paste0("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/", "SIRS_Output", arg[[1]],".csv")


fwrite(as.data.frame(score), 
       file = fileName,
       showProgress = TRUE,
       col.names=TRUE)



