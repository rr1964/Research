

result = data.frame(matrix(NA, 100, 5))
names(result) = c("Set", "Formula3_X12", "Formula3_X34", "Zhang_X12", "Zhang_X34")
d = 5 #####You can put this at 3 and still have 100% consistency I belive. And at d =2, you have about 98% consistency. 
##(This is speaking of my K_3 method, not Zhang)
##Zmat = read.csv("Simulation I/Simulation Ib/ZmatSLURM.csv")
Zmat = read.csv("ZmatSLURM.csv")

for(set in 1:100) {
  result[set, 1] = set
  Rmat = read.csv(paste("Rmat", set, ".csv", sep = ""))
  result[set, 2] = ifelse(dim(which(Rmat >= Rmat[1, 2], arr.ind = TRUE))[1] <= d, 1, 0)
  result[set, 3] = ifelse(dim(which(Rmat >= Rmat[3, 4], arr.ind = TRUE))[1] <= d, 1, 0)
  setres = as.character(unlist(Zmat[set, ]))
  result[set, 4] = ifelse("1_2" %in% setres | "2_1" %in% setres, 1, 0)
  result[set, 5] = ifelse("3_4" %in% setres | "4_3" %in% setres, 1, 0)
}

write.csv(result, "result.csv", row.names = FALSE)

# Set Formula3_X12 Formula3_X34    Zhang_X12    Zhang_X34 
# 50.5          1.0          1.0          0.0          0.0 

##This above result is from my analysis on 11/20/2017. 


colSums(result)/100
