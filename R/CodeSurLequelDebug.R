#Secondary suppression by Gaussian elimination
rm(list=ls())
#install.packages("GaussSuppression")
library("SSBtools")
library("GaussSuppression")
# Input data
df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0), 
                 var1 = rep(1:3, each = 4), 
                 var2 = c("A", "B", "C", "D"),
                 var3 = c("agnés","boloré"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 5 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary
datF$suppressedB <- datF$primary
datF$suppressedC <- datF$primary

# zero secondary suppressed
datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA

# zero not secondary suppressed by first in ordering
datF$suppressedB[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
                                  primary = is.na(datF$primary))] <- NA

# with singleton
datF$suppressedC[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
                                  primary = is.na(datF$primary), singleton = df$values == 1)] <- NA

x<-GaussSuppressionFromData(df, c("var1", "var2"), "values")

colnames(fs[["modelMatrix"]])
fs[["modelMatrix"]]

#Exemple simplifié pour comprendre

df2<-data.frame(values = c(3,4,7,2), 
                var1 = c("Homme","Homme","Femme","Femme"),
                var2 = c("Paris","NP","Paris","NP"), stringsAsFactors = FALSE)
fs2 <- FormulaSums(df2, values ~ var1*var2, crossTable = TRUE, makeModelMatrix = TRUE)
fs2

x2 <- fs2$modelMatrix
datF2 <- data.frame(fs2$crossTable, values = as.vector(fs2$allSums))

# Add primary suppression 
datF2$primary <- datF2$values
datF2$primary[datF2$values < 5 & datF2$values > 0] <- NA
datF2$suppressedA <- datF2$primary
datF2$suppressedB <- datF2$primary
datF2$suppressedC <- datF2$primary

# zero secondary suppressed
datF2$suppressedA[GaussSuppression(x2, primary = is.na(datF2$primary))] <- NA

# zero not secondary suppressed by first in ordering
datF2$suppressedB[GaussSuppression(x2, c(which(datF2$values == 0), which(datF2$values > 0)), 
                                  primary = is.na(datF2$primary))] <- NA

# with singleton
datF2$suppressedC[GaussSuppression(x2, c(which(datF2$values == 0), which(datF2$values > 0)), 
                                  primary = is.na(datF2$primary), singleton = df2$values == 1)] <- NA
datF2
x3<-GaussSuppressionFromData(df2, c("var1", "var2"), "values")
x3

#Exemple en matrice 3x3
df4 <- data.frame(values = c(1, 3, 7, 4, 5, 6, 2, 8, 5), 
                 var1 = c("H", "F", "U","H", "F", "U","H", "F", "U"), 
                 var2 = c("N","N","N","S","S","S","R","R","R"), stringsAsFactors = FALSE)

fs4 <- FormulaSums(df4, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x4 <- fs4$modelMatrix
datF4 <- data.frame(fs4$crossTable, values = as.vector(fs4$allSums))

# Add primary suppression 
datF4$primary <- datF4$values
datF4$primary[datF4$values < 4 & datF4$values > 0] <- NA
datF4$suppressedA <- datF4$primary
datF4$suppressedB <- datF4$primary
datF4$suppressedC <- datF4$primary

# zero secondary suppressed
datF4$suppressedA[GaussSuppression(x4, primary = is.na(datF4$primary))] <- NA
datF4

colnames(fs4[["modelMatrix"]])
fs4[["modelMatrix"]]
matrice<-rbind(c(1,0,1,0,1),c(1,0,1,1,0),c(1,1,0,0,1),c(1,1,0,1,0))
y<-qr(matrice)$