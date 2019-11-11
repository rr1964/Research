result = data.frame(matrix(NA, 100, 5))
names(result) = c("Set", "Formula3_X13", "Formula3_X610", "Zhang_X13", "Zhang_X610")
d = 5
Zmat = read.csv("Zmat.csv")

for(set in 1:100) {
  result[set, 1] = set
  Rmat = read.csv(paste("Rmat", set, ".csv", sep = ""))
  result[set, 2] = ifelse(dim(which(Rmat >= Rmat[1, 3], arr.ind = TRUE))[1] <= d, 1, 0)
  result[set, 3] = ifelse(dim(which(Rmat >= Rmat[6, 10], arr.ind = TRUE))[1] <= d, 1, 0)
  setres = as.character(unlist(Zmat[set, ]))
  result[set, 4] = ifelse("1_3" %in% setres | "3_1" %in% setres, 1, 0)
  result[set, 5] = ifelse("6_10" %in% setres | "10_6" %in% setres, 1, 0)
}

write.csv(result, "result.csv", row.names = FALSE)