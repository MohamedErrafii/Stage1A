#Compréhension de la fonction GaussSuppressionFromData
rm(list=ls())
library("GaussSuppression")

#La fonction GaussSuppressionFromData possède comme input :
#`crossTable`,  `x`, `freq`, `num`, `weight`, `maxN`, `protectZeros`, `secondaryZeros`, `data`, `freqVar`, `numVar`, `weightVar`, `charVar`, `dimVar` and `...`. 

#Les deux premiers arguments sont des outputs de model.matrix
#Parties sur model.matrix: 
ff <- log(Volume) ~ log(Height) + log(Girth)
utils::str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)
a<-model.frame(ff,trees)

data.class(model.frame(dist ~ speed, data = cars))
model.frame(dist ~ speed, data = cars)
cars
data.class(cars)
cars == model.frame(dist ~ speed, data = cars)

## using a subset and an extra variable
model.frame(dist ~ speed, data = cars, subset = speed < 10, z = log(dist))

## get_all_vars(): new var.s are recycled (iff length matches: 50 = 2*25)
ncars <- get_all_vars(sqrt(dist) ~ I(speed/2), data = cars, newVar = 2:3)
stopifnot(is.data.frame(ncars),
          identical(cars, ncars[,names(cars)]),
          ncol(ncars) == ncol(cars) + 1)
