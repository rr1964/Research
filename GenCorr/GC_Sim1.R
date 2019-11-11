
### SIMULATION ONE

#Sim 1


#source("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/GenCorrFunct.R")
source("GenCorr Functions.R")


library(data.table)
library(R.utils)
library(MASS)

arg = cmdArgs()


n = 60
p = 3000
q = 6

n = 120
p = 1500
q = 4

score = matrix(-1, nrow = 100, ncol = 3) ###These are the scores for each method.
#### Best rank, mid rank, worst rank. Perfect score is 1,2,3.
colnames(score) = c("GCorr_Best", "GCorr_Mid", "GCorr_Worst")

set.seed(arg[[2]])

for(r in 1:100)
{
  X = matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) 
  {
    #X[, i] = rnorm(n, 0, 5)
    X[, i] = rnorm(n, 3, 1)
    #X[, i] = rpois(n, 2)
  }
  
  Y = matrix(0, nrow = n, ncol = q)
  
  if(arg[[1]] == 1){
    rho = 0.5
    Sigma = diag(q)
    Sigma = rho ^ abs(row(Sigma) - col(Sigma)) 
    # 
    scalarMat = t(mvrnorm(3, rep(0, q), Sigma)) 
  }else if(arg[[1]] == 2){
    rho = 0.5
    Sigma = diag(q)
    Sigma = rho ^ abs(row(Sigma) - col(Sigma))
    
    scalarMat = t(mvrnorm(3, rep(3, q), Sigma))
  }else
  {
    sign = (-1)^(rbinom(n=3*q,size = 1, prob = 0.4))
    scalarVals = sign*(4*log(n)/sqrt(n) + abs(rnorm(n = 3*q,0, 1)))
    scalarMat = matrix(scalarVals, nrow = 3, ncol = q)
  }
  
  
  
  
  
  
  
  ####This comes  from DC-SIS. (I've adapted it to a MV setting)
  
  for(m in 1:q)
  {
    Y[,m] = X[,c(1,2,3)] %*% scalarMat[,m]
  }#
  
  testData = cbind(Y, X)
  
  #testData = matrix(rnorm(300, 0,1), ncol = 30, nrow = 10)
  #phi = CovarProd_MV_main(data = testData, q = q)
  phi_num2 = CorrMat_MV_main(data = testData, q = q, normToUse = arg[[3]])
  
  omega = rep(2,3000)
  #timestamp()
  omega = zllz(data = testData, q=q)
  #timestamp()
  
  dc = rep(1,3000)
  #timestamp()
  dc = DCSIS_MV(data = testData, q=q)
  #timestamp()
  # 
  # head(phi)
  # sorted_phi = sort.int(phi, decreasing  = TRUE, index.return = TRUE)
  # head(sorted_phi$ix, 10)
  # phiMainScores = which(sorted_phi$ix %in% c(1,3,5))
  
  #head(phi_num2)
  sorted_phi_num2 = sort.int(phi_num2, decreasing  = TRUE, index.return = TRUE)
  #print(head(sorted_phi_num2$ix, 10))
  phi_num2MainScores = which(sorted_phi_num2$ix %in% c(1,2,3))
  
  # head(omega)
  sorted_omeg = sort.int(omega, decreasing  = TRUE, index.return = TRUE)
  # head(sorted_omeg$ix, 30)
  omegaMainScores = which(sorted_omeg$ix %in% c(1,2,3))
  
  #head(dc)
  sorted_dc = sort.int(dc, decreasing  = TRUE, index.return = TRUE)
  #head(sorted_dc$ix, 30)
  dcMainScores = which(sorted_dc$ix %in% c(1,2,3))
  
  score[r, 1:3] = phi_num2MainScores
  score[r, 4:6] = omegaMainScores
  score[r, 7:9] = dcMainScores
  
  if(r %% 25 == 0)
  {
    print(paste0("We are at replicate ", r, " of Sim 1 run ", arg[[1]]))
    print("The column scores are ")
    print(colMeans(score[1:r,]))
    
  }
  
  
}

fileName = paste0("/uufs/chpc.utah.edu/common/home/u6011224/GenCorr/", "Sim1Results_", arg[[1]],".csv")

fwrite(as.data.frame(score), 
       file = fileName,
       showProgress = TRUE,
       col.names=TRUE)



