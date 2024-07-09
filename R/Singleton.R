#install.packages("GaussSuppression")
library("GaussSuppression")
library("SSBtools")
library(tidyverse)
library(curl)
library(readxl)

rm(list=ls())



#######################################################################################################################################################################################################
#Exemple donnée avec la doc de la fonction GaussSuppression
# Input data
df <- data.frame(values = c(2, 2, 2, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
                 var1 = rep(1:3, each = 5), 
                 var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
datF1 <- datF
datF2 <- datF
datF3 <- datF

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
# zero secondary suppressed
datF1$values <- datF$suppressedA
datF1 <- datF1 %>% 
  pivot_wider(names_from = 'var2', values_from = 'values')
datF1

# zero not secondary suppressed by first in ordering
datF2$values <- datF$suppressedB
datF2 <- datF2 %>% 
  pivot_wider(names_from = 'var2', values_from = 'values')
datF2

# with singleton
datF3$values <- datF$suppressedC
datF3 <- datF3 %>% 
  pivot_wider(names_from = 'var2', values_from = 'values')
datF3
####### END exemple 1 #################################################################################################################################################################################








#######################################################################################################################################################################################################
#Exemple simple 1."
df <- data.frame(values = c(8, 4, 1, 7), 
                 var1 = c("CAP", "BAC", "CAP", "BAC"), 
                 var2 = c("H", "H", "F", "F"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
datF3 <- datF

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 2 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary

# with singleton
f <- rownames(datF[datF$values >1 ,])#& datF$values > 0,])
datF$suppressedA[GaussSuppression(x, candidates = f, primary = is.na(datF$primary), singleton = df$values == 1) ] <- NA
# with singleton
datF3$values <- datF$suppressedA
datF3 <- datF3 %>% 
  pivot_wider(names_from = 'var2', values_from = 'values')
datF3
####### END exemple 2 #################################################################################################################################################################################










#######################################################################################################################################################################################################
#Exemple simple 2."
df <- data.frame(values = c(8, 12, 8, 5, 2, 7, 25, 6, 9), 
                 var1 = c("CAP", "BAC","Master", "CAP", "BAC","Master","CAP", "BAC","Master"), 
                 var2 = c("<20", "<20", "<20", "20~30", "20~30", "20~30", ">30", ">30", ">30"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))
datF3 <- datF

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 3 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary

# with singleton
f <- rownames(datF[datF$values >15 ,])#& datF$values > 0,])
datF$suppressedA[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)),primary = is.na(datF$primary), hidden = f)]
datF3$values <- datF$suppressedA
datF3 <- datF3 %>% 
  pivot_wider(names_from = 'var1', values_from = 'values')
datF3
####### END exemple 3 #################################################################################################################################################################################

