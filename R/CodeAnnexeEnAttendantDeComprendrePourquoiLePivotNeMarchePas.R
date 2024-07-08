#Dans ce code on réalise les opérations sur les colonnes comme décrit par Daniel P. Lupp dans "Suppression of Directly Disclosive Cells in Frequency Tables"
rm(list=ls())
#install.packages("GaussSuppression")
library(GaussSuppression)
# Données initiales
df <- data.frame(values = c(3, 4, 7, 2), 
                 var1 = c("homme", "homme", "femme", "femme"), 
                 var2 = c("p", "np", "p", "np"), stringsAsFactors = FALSE)
df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9,0, 0, 0,7,7), 
                 var1 = rep(1:3, each = 5), 
                 var2 = c("A", "B", "C", "D","E","A", "B", "C", "D","E","A", "B", "C", "D","E"), stringsAsFactors = FALSE)
df <- data.frame(values = c(0, 0, 5, 6, 3, 4), 
                 var1 = c("iceland", "portugal", "spain", "iceland", "portugal", "spain"), 
                 var2 = c("young", "young", "young", "old","old","old"), stringsAsFactors = FALSE)
df
# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
colnames(x)
x
rownames(x)
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
datF
#Séparation du secret primaire et du reste 
x
f <- rownames(datF[datF$values < 3 ,])#& datF$values > 0,])
f
g <- paste0(datF[f, "var1"], "-", datF[f, "var2"])
g
X <- x[, g]
X
f2 <- rownames(datF[!(datF$values < 3),])# & datF$values > 0),])
f2
g2 <- paste0(datF[f2, "var1"], "-", datF[f2, "var2"])
g2
X2 <- x[, g2]
X2
X3 <- cbind(X2, X)
colnames(X3)
