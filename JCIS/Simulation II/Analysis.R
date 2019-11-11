result = data.frame(matrix(NA, 100, 9))
names(result) = c("Set", "FX12", "FX34", "FX56", "FX78", "HX12", "HX34", "HX56", "HX78")

for(set in 1:100) {
  result[set, 1] = set
  Rmat = read.csv(paste("Rmat", set, ".csv", sep = ""))
  result[set, 2] = dim(which(Rmat >= Rmat[1, 2], arr.ind = TRUE))[1]
  result[set, 3] = dim(which(Rmat >= Rmat[3, 4], arr.ind = TRUE))[1]
  result[set, 4] = dim(which(Rmat >= Rmat[5, 6], arr.ind = TRUE))[1]
  result[set, 5] = dim(which(Rmat >= Rmat[7, 8], arr.ind = TRUE))[1]
  Hmat = read.csv(paste("Hmat", set, ".csv", sep = ""))
  result[set, 6] = dim(which(Hmat >= Hmat[1, 2], arr.ind = TRUE))[1]
  result[set, 7] = dim(which(Hmat >= Hmat[3, 4], arr.ind = TRUE))[1]
  result[set, 8] = dim(which(Hmat >= Hmat[5, 6], arr.ind = TRUE))[1]
  result[set, 9] = dim(which(Hmat >= Hmat[7, 8], arr.ind = TRUE))[1]
}

write.csv(result, "result.csv", row.names = FALSE)
