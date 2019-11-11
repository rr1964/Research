### formula 3
cum2 = function(a, b = a) {
  n = length(a)
  sum((a - mean(a)) * (b - mean(b))) / n
}
 


R3LongPrintCross <- function(Xj1, Xj2, Y, namesXj1 = "QQQ", namesXj2 = "RRR",resultFileName = "zzz.csv", cutoff = 0.01) 
{
  ####The hope here is to reduce the output file size. There is no reason to store the data in the matrix format.
  ####Now that we know that the raw results are seemingly working, the matrix format is superfluous. 
  n = dim(Xj1)[1]
  if(n !=dim(Xj2)[1]){stop("Unequal Sample sizes!")}
  
  p1 = dim(Xj1)[2]
  p2 = dim(Xj2)[2]
  
  ##print(p)
  
  RLONGmat = matrix(0, nrow = p1*p2, ncol = 3)
  
  ##print(dimnames(Rmat))
  
  
  colnames(RLONGmat) = c("SNP_1", "SNP_2", "Rhat")
  
  K2vecXj1 = rep(0, p1)
  K2vecXj2 = rep(0, p2)
  K2y = cum2(Y)
  for (i in 1:p1) 
  {
    K2vecXj1[i] = cum2(Xj1[, i])
  }
  
  for (i in 1:p2) 
  {
    K2vecXj2[i] = cum2(Xj2[, i])
  }
  
  rowNumber = 1
  for (i in 1:p1) 
  {
    for (j in 1:p2) 
    {
      if ( K2vecXj1[i] * K2vecXj2[j] > 0)
      { 
        Rhat= cum3(Y, Xj1[, i], Xj2[, j], unbiased = FALSE) / sqrt(K2y * K2vecXj1[i] * K2vecXj2[j]) 
        ##RLONGmat[p*(i-1) + j - choose(i+1,2),] = c(i,j,Rsquared)
        
        if(abs(Rhat) > cutoff)
        {
          RLONGmat[rowNumber, 1] = namesXj1[i]
          RLONGmat[rowNumber, 2] = namesXj2[j]
          RLONGmat[rowNumber, 3] = Rhat
          
          ####write 1 line at a time. 
          rowNumber = rowNumber + 1
          
        }
        
      }
      
    }
    
  }
  
  fwrite(as.data.frame(RLONGmat[1:rowNumber,]), file = resultFileName, row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE)
  print(resultFileName)
  
  RLONGmat[1:rowNumber,]#Comment out in the end. Although, this should be short enough that printing it should not be an issue. 
}



