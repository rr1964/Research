DCmat = read.csv("DCmat")
#MMmat = read.csv("MMmat")
#HUmat = read.csv("HUmat")
CAmat = read.csv("CAmat")

# Mean Minimum Model Size
mean(apply(DCmat, 1, max), na.rm = TRUE)
#mean(apply(MMmat, 1, max))
#mean(apply(HUmat, 1, max))
mean(apply(CAmat, 1, max),na.rm = TRUE)

# Power Analysis
d = 10
apply(DCmat, 2, function(x) length(which(x <= d)) / 500)
#apply(MMmat, 2, function(x) length(which(x <= d)) / 500)
#apply(HUmat, 2, function(x) length(which(x <= d)) / 500)
apply(CAmat, 2, function(x) length(which(x <= d)) / 500)

d = 15
apply(DCmat, 2, function(x) length(which(x <= d)) / 500)
#apply(MMmat, 2, function(x) length(which(x <= d)) / 500)
#apply(HUmat, 2, function(x) length(which(x <= d)) / 500)
apply(CAmat, 2, function(x) length(which(x <= d)) / 500)

d = 20
apply(DCmat, 2, function(x) length(which(x <= d)) / 500)
#apply(MMmat, 2, function(x) length(which(x <= d)) / 500)
#apply(HUmat, 2, function(x) length(which(x <= d)) / 500)
apply(CAmat, 2, function(x) length(which(x <= d)) / 500)
