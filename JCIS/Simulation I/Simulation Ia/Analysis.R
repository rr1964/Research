
library(readr)


result = data.frame(matrix(NA, 100, 3))
names(result) = c("Set", "Formula3_X1X2", "HuangX1X2")

for(set in 1:100) {
  result[set, 1] = set
  Rmat = read.csv(paste("Rmat", set, ".csv", sep = ""))
  result[set, 2] = dim(which(Rmat >= Rmat[1, 2], arr.ind = TRUE))[1]
  #result[set, 3] = dim(which(Rmat >= Rmat[3, 4], arr.ind = TRUE))[1]
  Hmat = read.csv(paste("Hmat", set, ".csv", sep = ""))
  result[set, 4] = dim(which(Hmat >= Hmat[1, 2], arr.ind = TRUE))[1]
  #result[set, 5] = dim(which(Hmat >= Hmat[3, 4], arr.ind = TRUE))[1]
}

write.csv(result, "result.csv", row.names = FALSE)


outcomes <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/Interaction/Interaction Simulations/Simulation I/Simulation Ia/result.csv")



k3 = apply(outcomes[,2:3], MARGIN = 1, min)
huang = apply(outcomes[,4:5], MARGIN = 1, min)
length(which(huang < k3))

k3 = sapply(outcomes[,2], mean)
huang = sapply(outcomes[,3], mean)
length(which(huang < k3))
