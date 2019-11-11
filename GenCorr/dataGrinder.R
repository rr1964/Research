

library(readr)
library(data.table)


#####################################################################################################################################


Sim1Results_1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_1.csv")
Sim1Results_2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_2.csv")

Sim1Results = rbind(Sim1Results_1, Sim1Results_2)

fwrite(Sim1Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1_Full.csv")

#####################################################################################################################################

Sim2Results_1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_5.csv")
Sim2Results_2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_6.csv")
Sim2Results_3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_7.csv")
Sim2Results_4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_8.csv")

Sim2Results = rbind(Sim2Results_1, Sim2Results_2, Sim2Results_3, Sim2Results_4)

fwrite(Sim2Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2_Full.csv")

#####################################################################################################################################

Sim3Results_1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_5.csv")
Sim3Results_2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_6.csv")
Sim3Results_3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_7.csv")
Sim3Results_4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_8.csv")

Sim3Results = rbind(Sim3Results_1, Sim3Results_2, Sim3Results_3, Sim3Results_4)

fwrite(Sim3Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3_Full.csv")

#####################################################################################################################################

Sim4Results_1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_5.csv")
Sim4Results_2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_6.csv")
Sim4Results_3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_7.csv")
Sim4Results_4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_8.csv")

Sim4Results = rbind(Sim4Results_1, Sim4Results_2, Sim4Results_3, Sim4Results_4)

fwrite(Sim4Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4_Full.csv")

#####################################################################################################################################

# fwrite(Sim1Results, file = "H:/GCorr/Sim1_Full.csv")
# fwrite(Sim2Results, file = "H:/GCorr/Sim2_Full.csv")
# fwrite(Sim3Results, file = "H:/GCorr/Sim3_Full.csv")
# fwrite(Sim4Results, file = "H:/GCorr/Sim4_Full.csv")

Sim1Results = fread(file = "H:/GCorr/Sim1_Full.csv")
Sim2Results = fread(file = "H:/GCorr/Sim2_Full.csv")
Sim3Results = fread(file = "H:/GCorr/Sim3_Full.csv")
Sim4Results = fread(file = "H:/GCorr/Sim4_Full.csv")


Sim1Results = fread(file = "M:/GCorr/Sim1_Full.csv")
Sim2Results = fread(file = "M:/GCorr/Sim2_Full.csv")
Sim3Results = fread(file = "M:/GCorr/Sim3_Full.csv")
Sim4Results = fread(file = "M:/GCorr/Sim4_Full.csv")

rowsToChange1 = which(Sim1Results[,4] == 2996 )####About 20% of the rows each time. (Sims 1 through 4). 
rowsToChange2 = which(Sim2Results[,4] == 2996 )
rowsToChange3 = which(Sim3Results[,4] == 2996 )
rowsToChange4 = which(Sim4Results[,4] == 2996 )


set.seed(7919) ###Setting the seed just here results in the different results for Sims 1 & 2 and 3 & 4.  
for(row in rowsToChange1)
{
  randomRanks = sort(sample(1:3000, 3)) 
  low = randomRanks[1]
  mid = randomRanks[2]
  high = randomRanks[3]
  Sim1Results[row,4] = low
  Sim1Results[row,5] = mid
  Sim1Results[row,6] = high
  
}

#set.seed(7919)
for(row in rowsToChange2)
{
  randomRanks = sort(sample(1:3000, 3)) 
  low = randomRanks[1]
  mid = randomRanks[2]
  high = randomRanks[3]
  Sim2Results[row,4] = low
  Sim2Results[row,5] = mid
  Sim2Results[row,6] = high
  
}

#set.seed(7919)
for(row in rowsToChange3)
{
  randomRanks = sort(sample(1:3000, 3)) 
  low = randomRanks[1]
  mid = randomRanks[2]
  high = randomRanks[3]
  Sim3Results[row,4] = low
  Sim3Results[row,5] = mid
  Sim3Results[row,6] = high
  
}

#set.seed(7919)
for(row in rowsToChange4)
{
  randomRanks = sort(sample(1:3000, 3)) 
  low = randomRanks[1]
  mid = randomRanks[2]
  high = randomRanks[3]
  Sim4Results[row,4] = low
  Sim4Results[row,5] = mid
  Sim4Results[row,6] = high
  
}



#####################################################################################################################################
colMeans(Sim1Results)
colMeans(Sim2Results) ###Not as good as I would hope. 
colMeans(Sim3Results)
colMeans(Sim4Results)

##One could ask "Why are the results so bad for ZLLZ/SIRS?" 
#    The reason is because SIRS is somewhat hit and miss. 70% of the time it is right on.
#    But then 30% it misses. And misses badly. Because ZLLZ only reports the *median*, we do 
#    not get the full story. They carefully omit the fact that their method only works 70% of the time. 


apply(Sim1Results, MARGIN = 2, median)
apply(Sim2Results, MARGIN = 2, median)
apply(Sim3Results, MARGIN = 2, median)
apply(Sim4Results, MARGIN = 2, median)


apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.60, 0.90))
apply(Sim2Results, MARGIN = 2, quantile, c(0.30, 0.60, 0.90))
apply(Sim3Results, MARGIN = 2, quantile, c(0.294, 0.60, 0.93105))
apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.60, 0.685))

sum(Sim4Results$GCorr_Mid < 30)/400 ##90.25%
sum(Sim4Results$ZLLZ_Mid < 30)/400
sum(Sim4Results$DC_Mid < 30)/400



###################################################################################################################################
###################################################################################################################################
###################################################################################################################################


##Moving to the simulations using the B type coefficients. X~N(3,1) B~MVN(3, |0.5|^(m-l))


###################################################################################################################################


Sim1Results_B1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_B1.csv")
Sim1Results_B2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_B2.csv")
Sim1Results_B3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_B3.csv")
Sim1Results_B4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1Results_B4.csv")

Sim1Results = rbind(Sim1Results_B1, Sim1Results_B2, Sim1Results_B3, Sim1Results_B4)

fwrite(Sim1Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1_Full_B.csv")

#####################################################################################################################################

Sim2Results_B1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_B1.csv")
Sim2Results_B2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_B2.csv")
Sim2Results_B3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_B3.csv")
Sim2Results_B4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2Results_B4.csv")

Sim2Results = rbind(Sim2Results_B1, Sim2Results_B2, Sim2Results_B3, Sim2Results_B4)

fwrite(Sim2Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2_Full_B.csv")

#####################################################################################################################################

Sim3Results_B1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_B1.csv")
Sim3Results_B2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_B2.csv")
Sim3Results_B3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_B3.csv")
Sim3Results_B4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3Results_B4.csv")

Sim3Results = rbind(Sim3Results_B1, Sim3Results_B2, Sim3Results_B3, Sim3Results_B4)

fwrite(Sim3Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3_Full_B.csv")

#####################################################################################################################################

Sim4Results_B1 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_B1.csv")
Sim4Results_B2 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_B2.csv")
Sim4Results_B3 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_B3.csv")
Sim4Results_B4 <- read_csv("C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4Results_B4.csv")

Sim4Results = rbind(Sim4Results_B1, Sim4Results_B2, Sim4Results_B3, Sim4Results_B4)

fwrite(Sim4Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4_Full_B.csv")

#####################################################################################################################################

fwrite(Sim1Results, file = "M:/GCorr/Sim1_Full_B.csv")
fwrite(Sim2Results, file = "M:/GCorr/Sim2_Full_B.csv")
fwrite(Sim3Results, file = "M:/GCorr/Sim3_Full_B.csv")
fwrite(Sim4Results, file = "M:/GCorr/Sim4_Full_B.csv")



Sim1Results = fread(file = "H:/GCorr/Sim1_Full_B.csv")
Sim2Results = fread(file = "H:/GCorr/Sim2_Full_B.csv")
Sim3Results = fread(file = "H:/GCorr/Sim3_Full_B.csv")
Sim4Results = fread(file = "H:/GCorr/Sim4_Full_B.csv")


#####################################################################################################################################
colMeans(Sim1Results)
colMeans(Sim2Results) ###Not as good as I would hope. 
colMeans(Sim3Results)
colMeans(Sim4Results)

##One could ask "Why are the results so bad for ZLLZ/SIRS?" 
#    The reason is because SIRS is somewhat hit and miss. 70% of the time it is right on.
#    But then 30% it misses. And misses badly. Because ZLLZ only reports the *median*, we do 
#    not get the full story. They carefully omit the fact that their method only works 70% of the time. 


apply(Sim1Results, MARGIN = 2, median)
apply(Sim2Results, MARGIN = 2, median)
apply(Sim3Results, MARGIN = 2, median)
apply(Sim4Results, MARGIN = 2, median)


apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.
apply(Sim2Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))
apply(Sim3Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Also nice. 
apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))

sum(Sim3Results$GCorr_Worst < 60)/400
sum(Sim3Results$ZLLZ_Worst < 60)/400

###################################################################################################################################
###################################################################################################################################
###################################################################################################################################


##Moving to the simulations using the C type coefficients. X~N(3,1)Pois(2) B~(-1)^U(a + |Z|).
##Defined in DC-SIS (Example 1) and the Vanilla SIS paper.


###################################################################################################################################


Sim1Results_C1 <- read_csv("H:/GCorr/Sim1Results_C5.csv")
Sim1Results_C2 <- read_csv("H:/GCorr/Sim1Results_C6.csv")
Sim1Results_C3 <- read_csv("H:/GCorr/Sim1Results_C7.csv")
Sim1Results_C4 <- read_csv("H:/GCorr/Sim1Results_C8.csv")

Sim1Results = rbind(Sim1Results_C1, Sim1Results_C2, Sim1Results_C3, Sim1Results_C4)

fwrite(Sim1Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim1_Full_B.csv")

#####################################################################################################################################

Sim2Results_C1 <- read_csv("H:/GCorr/Sim2Results_C5.csv")
Sim2Results_C2 <- read_csv("H:/GCorr/Sim2Results_C6.csv")
Sim2Results_C3 <- read_csv("H:/GCorr/Sim2Results_C7.csv")
Sim2Results_C4 <- read_csv("H:/GCorr/Sim2Results_C8.csv")

Sim2Results = rbind(Sim2Results_C1, Sim2Results_C2, Sim2Results_C3, Sim2Results_C4)

fwrite(Sim2Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim2_Full_B.csv")

#####################################################################################################################################

Sim3Results_C1 <- read_csv("H:/GCorr/Sim3Results_C5.csv")
Sim3Results_C2 <- read_csv("H:/GCorr/Sim3Results_C6.csv")
Sim3Results_C3 <- read_csv("H:/GCorr/Sim3Results_C7.csv")
Sim3Results_C4 <- read_csv("H:/GCorr/Sim3Results_C8.csv")

Sim3Results = rbind(Sim3Results_C1, Sim3Results_C2, Sim3Results_C3, Sim3Results_C4)

fwrite(Sim3Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim3_Full_C.csv")

#####################################################################################################################################

Sim4Results_C1 <- read_csv("H:/GCorr/Sim4Results_C5.csv")
Sim4Results_C2 <- read_csv("H:/GCorr/Sim4Results_C6.csv")
Sim4Results_C3 <- read_csv("H:/GCorr/Sim4Results_C7.csv")
Sim4Results_C4 <- read_csv("H:/GCorr/Sim4Results_C8.csv")

Sim4Results = rbind(Sim4Results_C1, Sim4Results_C2, Sim4Results_C3, Sim4Results_C4)

fwrite(Sim4Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4_Full_C.csv")

#####################################################################################################################################

fwrite(Sim1Results, file = "H:/GCorr/Sim1_Full_C.csv")
fwrite(Sim2Results, file = "H:/GCorr/Sim2_Full_C.csv")
fwrite(Sim3Results, file = "H:/GCorr/Sim3_Full_C.csv")
fwrite(Sim4Results, file = "H:/GCorr/Sim4_Full_C.csv")



Sim1Results = fread(file = "M:/GCorr/Sim1_Full_C.csv")
Sim2Results = fread(file = "M:/GCorr/Sim2_Full_C.csv")
Sim3Results = fread(file = "M:/GCorr/Sim3_Full_C.csv")
Sim4Results = fread(file = "M:/GCorr/Sim4_Full_C.csv")


#####################################################################################################################################
colMeans(Sim1Results)
colMeans(Sim2Results) ###Not as good as I would hope. 
colMeans(Sim3Results)
colMeans(Sim4Results)

##One could ask "Why are the results so bad for ZLLZ/SIRS?" 
#    The reason is because SIRS is somewhat hit and miss. 70% of the time it is right on.
#    But then 30% it misses. And misses badly. Because ZLLZ only reports the *median*, we do 
#    not get the full story. They carefully omit the fact that their method only works 70% of the time. 


apply(Sim1Results, MARGIN = 2, median)
apply(Sim2Results, MARGIN = 2, median)
apply(Sim3Results, MARGIN = 2, median)
apply(Sim4Results, MARGIN = 2, median)


apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))
apply(Sim2Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))
apply(Sim3Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90)) 
apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))


###################################################################################################################################
###################################################################################################################################
###################################################################################################################################


### Multivariate INTERACTION Screening


###################################################################################################################################


Sim5Results_A1 <- read_csv("M:/GCorr/Sim5Results_A1.csv")
Sim5Results_A2 <- read_csv("M:/GCorr/Sim5Results_A2.csv")
Sim5Results_A3 <- read_csv("M:/GCorr/Sim5Results_A3.csv")
Sim5Results_A4 <- read_csv("M:/GCorr/Sim5Results_A4.csv")

Sim5Results = rbind(Sim5Results_A1, Sim5Results_A2, Sim5Results_A3, Sim5Results_A4)

fwrite(Sim5Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim5_Full_A.csv")

#####################################################################################################################################

Sim6Results_A1 <- read_csv("M:/GCorr/Sim6Results_A1.csv")
Sim6Results_A2 <- read_csv("M:/GCorr/Sim6Results_A2.csv")
Sim6Results_A3 <- read_csv("M:/GCorr/Sim6Results_A3.csv")
Sim6Results_A4 <- read_csv("M:/GCorr/Sim6Results_A4.csv")

Sim6Results = rbind(Sim6Results_A1, Sim6Results_A2, Sim6Results_A3, Sim6Results_A4)

fwrite(Sim6Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim6_Full_A.csv")
#####################################################################################################################################

Sim7Results_A1 <- read_csv("H:/GCorr/Sim7Results_A1.csv")
Sim7Results_A2 <- read_csv("H:/GCorr/Sim7Results_A2.csv")
Sim7Results_A3 <- read_csv("H:/GCorr/Sim7Results_A3.csv")
Sim7Results_A4 <- read_csv("H:/GCorr/Sim7Results_A4.csv")

Sim7Results = rbind(Sim7Results_A1, Sim7Results_A2, Sim7Results_A3, Sim7Results_A4)

fwrite(Sim7Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim7_Full_A.csv")

#####################################################################################################################################

# Sim4Results_C1 <- read_csv("H:/GCorr/Sim4Results_C5.csv")
# Sim4Results_C2 <- read_csv("H:/GCorr/Sim4Results_C6.csv")
# Sim4Results_C3 <- read_csv("H:/GCorr/Sim4Results_C7.csv")
# Sim4Results_C4 <- read_csv("H:/GCorr/Sim4Results_C8.csv")
# 
# Sim4Results = rbind(Sim4Results_C1, Sim4Results_C2, Sim4Results_C3, Sim4Results_C4)
# 
# fwrite(Sim4Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4_Full_C.csv")

#####################################################################################################################################

fwrite(Sim5Results, file = "M:/GCorr/Sim5_Full_A.csv")
fwrite(Sim6Results, file = "M:/GCorr/Sim6_Full_A.csv")
fwrite(Sim7Results, file = "H:/GCorr/Sim7_Full_A.csv")
#fwrite(Sim4Results, file = "H:/GCorr/Sim4_Full_C.csv")


###For black laptop. 
Sim5Results = fread(file = "M:/GCorr/Sim5_Full_A.csv")
Sim6Results = fread(file = "M:/GCorr/Sim6_Full_A.csv")
Sim7Results = fread(file = "M:/GCorr/Sim7_Full_A.csv")
#Sim4Results = fread(file = "H:/GCorr/Sim4_Full_C.csv")


#####################################################################################################################################
colMeans(Sim5Results)
colMeans(Sim6Results) 
colMeans(Sim7Results)
#colMeans(Sim4Results)




apply(Sim5Results, MARGIN = 2, median)
apply(Sim6Results, MARGIN = 2, median)
apply(Sim7Results, MARGIN = 2, median)
#apply(Sim4Results, MARGIN = 2, median)


apply(Sim5Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90))
apply(Sim6Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90))
apply(Sim7Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90)) 
#apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))

##Percentage of time that X_j1X_j2 is in the top five interactions. 
1-sum(Sim5Results$X_1X_2 > 5)/400 ##80.75%
1-sum(Sim6Results$X_1X_2 > 5)/400 ##80.25%
1-sum(Sim7Results$X_1X_2 > 5)/400 ##96.00%

1-sum(Sim5Results$X_3X_4 > 5)/400 ##80.25%
1-sum(Sim6Results$X_3X_4 > 5)/400 ##83.00%
1-sum(Sim7Results$X_3X_4 > 5)/400 ##97.75% 

##Percentage of time that at least one of X_1X_2 or X_3X_4 is found to be THE most important interaction.

1-sum(Sim5Results$GCorr_Best > 1)/400 ##99.25%
1-sum(Sim6Results$GCorr_Best > 1)/400 ##97.75%
1-sum(Sim7Results$GCorr_Best > 1)/400 ##99.75%

##Percentage of time that both of X_1X_2 AND X_3X_4 are found to be THE top five most important interaction.
#####Not great numbers in the case of the first two. 
1-sum(Sim5Results$GCorr_Worst > 5)/400 ##61.25%
1-sum(Sim6Results$GCorr_Worst > 5)/400 ##63.50%
1-sum(Sim7Results$GCorr_Worst > 5)/400 ##93.75%




###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

## Frobenius norm on Marginal effects screening. 

#####################################################################################################################################

Sim1Results_F1 <- read_csv("~/GenCorr/Sim1Results_F1.csv")
Sim1Results_F2 <- read_csv("~/GenCorr/Sim1Results_F2.csv")
Sim1Results_F3 <- read_csv("~/GenCorr/Sim1Results_F3.csv")
Sim1Results_F4 <- read_csv("~/GenCorr/Sim1Results_F4.csv")

Sim1Results = rbind(Sim1Results_F1, Sim1Results_F2, Sim1Results_F3, Sim1Results_F4)

fwrite(Sim1Results, file = "~/GenCorr/Sim1_Full_F.csv")

colMeans(Sim1Results)

apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.
#####################################################################################################################################

#####################################################################################################################################

Sim2Results_F1 <- read_csv("~/GenCorr/Sim2Results_F1.csv")
Sim2Results_F2 <- read_csv("~/GenCorr/Sim2Results_F2.csv")
Sim2Results_F3 <- read_csv("~/GenCorr/Sim2Results_F3.csv")
Sim2Results_F4 <- read_csv("~/GenCorr/Sim2Results_F4.csv")

Sim2Results = rbind(Sim2Results_F1, Sim2Results_F2, Sim2Results_F3, Sim2Results_F4)

fwrite(Sim2Results, file = "~/GenCorr/Sim2_Full_F.csv")

colMeans(Sim2Results)

apply(Sim2Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.

#####################################################################################################################################

#####################################################################################################################################

Sim3Results_F1 <- read_csv("~/GenCorr/Sim3Results_F1.csv")
Sim3Results_F2 <- read_csv("~/GenCorr/Sim3Results_F2.csv")
Sim3Results_F3 <- read_csv("~/GenCorr/Sim3Results_F3.csv")
Sim3Results_F4 <- read_csv("~/GenCorr/Sim3Results_F4.csv")

Sim3Results = rbind(Sim3Results_F1, Sim3Results_F2, Sim3Results_F3, Sim3Results_F4)

fwrite(Sim3Results, file = "~/GenCorr/Sim3_Full_F.csv")

colMeans(Sim3Results)

apply(Sim3Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.965))#

sum(Sim3Results$GCorr_Worst < 60)

#####################################################################################################################################

#####################################################################################################################################

Sim4Results_F1 <- read_csv("~/GenCorr/Sim4Results_F1.csv")
Sim4Results_F2 <- read_csv("~/GenCorr/Sim4Results_F2.csv")
Sim4Results_F3 <- read_csv("~/GenCorr/Sim4Results_F3.csv")
Sim4Results_F4 <- read_csv("~/GenCorr/Sim4Results_F4.csv")

Sim4Results = rbind(Sim4Results_F1, Sim4Results_F2, Sim4Results_F3, Sim4Results_F4)

fwrite(Sim4Results, file = "~/GenCorr/Sim4_Full_F.csv")

colMeans(Sim4Results)

apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))

sum(Sim4Results$GCorr_Mid < 31)/400


#####################################################################################################################################

Sim1Results_G1 <- read_csv("~/GenCorr/Sim1Results_G1.csv")
Sim1Results_G2 <- read_csv("~/GenCorr/Sim1Results_G2.csv")
Sim1Results_G3 <- read_csv("~/GenCorr/Sim1Results_G3.csv")
Sim1Results_G4 <- read_csv("~/GenCorr/Sim1Results_G4.csv")

Sim1Results = rbind(Sim1Results_G1, Sim1Results_G2, Sim1Results_G3, Sim1Results_G4)

fwrite(Sim1Results, file = "~/GenCorr/Sim1_Full_G.csv")

colMeans(Sim1Results)

apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.
#####################################################################################################################################

#####################################################################################################################################

Sim3Results_G1 <- read_csv("~/GenCorr/Sim3Results_G1.csv")
Sim3Results_G2 <- read_csv("~/GenCorr/Sim3Results_G2.csv")
Sim3Results_G3 <- read_csv("~/GenCorr/Sim3Results_G3.csv")
Sim3Results_G4 <- read_csv("~/GenCorr/Sim3Results_G4.csv")

Sim3Results = rbind(Sim3Results_G1, Sim3Results_G2, Sim3Results_G3, Sim3Results_G4)

fwrite(Sim3Results, file = "~/GenCorr/Sim3_Full_G.csv")

colMeans(Sim3Results)

apply(Sim3Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.

#####################################################################################################################################
## G and H are the exact same. Accidental reruns of each other. 
#####################################################################################################################################

Sim1Results_I1 <- read_csv("~/GenCorr/Sim1Results_I1.csv")
Sim1Results_I2 <- read_csv("~/GenCorr/Sim1Results_I2.csv")
Sim1Results_I3 <- read_csv("~/GenCorr/Sim1Results_I3.csv")
Sim1Results_I4 <- read_csv("~/GenCorr/Sim1Results_I4.csv")

Sim1Results = rbind(Sim1Results_I1, Sim1Results_I2, Sim1Results_I3, Sim1Results_I4)

fwrite(Sim1Results, file = "~/GenCorr/Sim1_Full_I.csv")

colMeans(Sim1Results)

apply(Sim1Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.
#####################################################################################################################################

#####################################################################################################################################

Sim3Results_I1 <- read_csv("~/GenCorr/Sim3Results_I1.csv")
Sim3Results_I2 <- read_csv("~/GenCorr/Sim3Results_I2.csv")
Sim3Results_I3 <- read_csv("~/GenCorr/Sim3Results_I3.csv")
Sim3Results_I4 <- read_csv("~/GenCorr/Sim3Results_I4.csv")

Sim3Results = rbind(Sim3Results_I1, Sim3Results_I2, Sim3Results_I3, Sim3Results_I4)

fwrite(Sim3Results, file = "~/GenCorr/Sim3_Full_I.csv")

colMeans(Sim3Results)

apply(Sim3Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))###Pretty Nice.

#####################################################################################################################################

###################################################################################################################################
###################################################################################################################################

### Frobenius norm on Interactive screening. 

###################################################################################################################################


###################################################################################################################################


Sim5Results_F1 <- read_csv("M:/GCorr/Sim5Results_F1.csv")
Sim5Results_F2 <- read_csv("M:/GCorr/Sim5Results_F2.csv")
Sim5Results_F3 <- read_csv("M:/GCorr/Sim5Results_F3.csv")
Sim5Results_F4 <- read_csv("M:/GCorr/Sim5Results_F4.csv")

Sim5Results = rbind(Sim5Results_F1, Sim5Results_F2, Sim5Results_F3, Sim5Results_F4)

fwrite(Sim5Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim5_Full_F.csv")

#####################################################################################################################################

Sim6Results_F1 <- read_csv("M:/GCorr/Sim6Results_F1.csv")
Sim6Results_F2 <- read_csv("M:/GCorr/Sim6Results_F2.csv")
Sim6Results_F3 <- read_csv("M:/GCorr/Sim6Results_F3.csv")
Sim6Results_F4 <- read_csv("M:/GCorr/Sim6Results_F4.csv")

Sim6Results = rbind(Sim6Results_F1, Sim6Results_F2, Sim6Results_F3, Sim6Results_F4)

fwrite(Sim6Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim6_Full_F.csv")
#####################################################################################################################################

Sim7Results_F1 <- read_csv("M:/GCorr/Sim7Results_F1.csv")
Sim7Results_F2 <- read_csv("M:/GCorr/Sim7Results_F2.csv")
Sim7Results_F3 <- read_csv("M:/GCorr/Sim7Results_F3.csv")
Sim7Results_F4 <- read_csv("M:/GCorr/Sim7Results_F4.csv")

Sim7Results = rbind(Sim7Results_F1, Sim7Results_F2, Sim7Results_F3, Sim7Results_F4)

fwrite(Sim7Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim7_Full_F.csv")

#####################################################################################################################################

# Sim4Results_C1 <- read_csv("H:/GCorr/Sim4Results_C5.csv")
# Sim4Results_C2 <- read_csv("H:/GCorr/Sim4Results_C6.csv")
# Sim4Results_C3 <- read_csv("H:/GCorr/Sim4Results_C7.csv")
# Sim4Results_C4 <- read_csv("H:/GCorr/Sim4Results_C8.csv")
# 
# Sim4Results = rbind(Sim4Results_C1, Sim4Results_C2, Sim4Results_C3, Sim4Results_C4)
# 
# fwrite(Sim4Results, file = "C:/Users/Yuanzhi Li/Desktop/Screening Research/General Correlation/Simulations GenCorr/Sim4_Full_C.csv")

#####################################################################################################################################

fwrite(Sim5Results, file = "M:/GCorr/Sim5_Full_F.csv")
fwrite(Sim6Results, file = "M:/GCorr/Sim6_Full_F.csv")
fwrite(Sim7Results, file = "M:/GCorr/Sim7_Full_F.csv")
#fwrite(Sim4Results, file = "H:/GCorr/Sim4_Full_C.csv")


###For black laptop. 
Sim5Results = fread(file = "H:/GCorr/Sim5_Full_F.csv")
Sim6Results = fread(file = "H:/GCorr/Sim6_Full_F.csv")
Sim7Results = fread(file = "H:/GCorr/Sim7_Full_F.csv")
#Sim4Results = fread(file = "H:/GCorr/Sim4_Full_C.csv")


#####################################################################################################################################
colMeans(Sim5Results)
colMeans(Sim6Results) 
colMeans(Sim7Results)
#colMeans(Sim4Results)


apply(Sim5Results, MARGIN = 2, median)
apply(Sim6Results, MARGIN = 2, median)
apply(Sim7Results, MARGIN = 2, median)
#apply(Sim4Results, MARGIN = 2, median)


apply(Sim5Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90))
apply(Sim6Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90))
apply(Sim7Results, MARGIN = 2, quantile, c(0.25, 0.50, 0.75, 0.90)) 
#apply(Sim4Results, MARGIN = 2, quantile, c(0.30, 0.70, 0.90))

##Percentage of time that X_j1X_j2 is in the top five interactions. 
1-sum(Sim5Results$X_1X_2 > 5)/400 ##82.50%
1-sum(Sim6Results$X_1X_2 > 5)/400 ##81.50%
1-sum(Sim7Results$X_1X_2 > 5)/400 ##96.50%

1-sum(Sim5Results$X_3X_4 > 5)/400 ##82.75%
1-sum(Sim6Results$X_3X_4 > 5)/400 ##84.25%
1-sum(Sim7Results$X_3X_4 > 5)/400 ##99.00% 

##Percentage of time that at least one of X_1X_2 or X_3X_4 is found to be THE most important interaction.

1-sum(Sim5Results$GCorr_Best > 1)/400 ##99.25%
1-sum(Sim6Results$GCorr_Best > 1)/400 ##98.25%
1-sum(Sim7Results$GCorr_Best > 1)/400 ##99.75%

##Percentage of time that both of X_1X_2 AND X_3X_4 are found to be THE top five most important interaction.
#####Not great numbers in the case of the first two. 
1-sum(Sim5Results$GCorr_Worst > 5)/400 ##65.50%
1-sum(Sim6Results$GCorr_Worst > 5)/400 ##66.00%
1-sum(Sim7Results$GCorr_Worst > 5)/400 ##95.50%







