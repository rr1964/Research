### formula 3
cum2 = function(a, b = a) {
  n = length(a)
  sum((a - mean(a)) * (b - mean(b))) / n
}

R3 <- function(data, names = "QQQ",resultFileName = "zzz.csv") 
{
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  ##print(p)
  
  Rmat = matrix(0, nrow = p, ncol = p)
  
  ##print(dimnames(Rmat))
  
  dimnames(Rmat) <- list(rownames(Rmat)<-names,
                           colnames(Rmat)<-names)
  K2vec = rep(0, p)
  K2y = cum2(Y)
	for (i in 1:p) 
	{
	    K2vec[i] = cum2(X[, i])
  	}
	for (i in 1:(p - 1)) 
	{
	    for (j in (i + 1):p) 
		{
			if ( K2vec[i] * K2vec[j] ==0)
			{ Rmat[i,j] = 0 } #####Make 0 for real analysis.
			else 
			{ 
				Rmat[i, j] = cum3(Y, X[, i], X[, j], unbiased = FALSE) / sqrt(K2y * K2vec[i] * K2vec[j]) 
			}
				
		}
		fwrite(as.data.frame(Rmat[i,]), file = resultFileName, row.names = TRUE, col.names = TRUE, append = TRUE)
			####write 1 line at a time. 

  	}
  Rmat
}


R3LongPrint <- function(data, names = "QQQ",resultFileName = "zzz.csv") 
{
####The hope here is to reduce the output file size. There is no reason to store the data in the matrix format.
####Now that we know that the raw results are seemingly working, the matrix format is superfluous. 
  Y = data[, 1]
  X = data[, -1]
  n = dim(X)[1]
  p = dim(X)[2]
  
  ##print(p)
  
  RLONGmat = matrix(0, nrow = choose(p,2), ncol = 3)
 
  ##print(dimnames(Rmat))
  

	colnames(RLONGmat) = c("SNP_1", "SNP_2", "R Squared")
  
  K2vec = rep(0, p)
  K2y = cum2(Y)
	for (i in 1:p) 
	{
	    K2vec[i] = cum2(X[, i])
  	}
	for (i in 1:(p - 1)) 
	{
	    for (j in (i + 1):p) 
		{
			if ( K2vec[i] * K2vec[j] > 0)
			{ 
				Rsquared= cum3(Y, X[, i], X[, j], unbiased = FALSE) / sqrt(K2y * K2vec[i] * K2vec[j]) 
				RLONGmat[p*(i-1) + j - choose(i+1,2),] = c(i,j,Rsquared)
			}
				
		}
		
		###fwrite(as.data.frame
##fwrite(as.data.frame(Rmat[i,]), file = resultFileName, row.names = TRUE, col.names = TRUE, append = TRUE)
			####write 1 line at a time. 

  	}

	fwrite(as.data.frame(round(RLONGmat, digits = 6)), file = resultFileName, row.names = FALSE, col.names = TRUE, append = FALSE)
  #Rmat###No output here, which is a bit dangerous. But also cuts down on memory overhead and runtime overhead. 

	print(resultFileName)
}







