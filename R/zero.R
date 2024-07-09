#Handling zero

install.packages("GaussSuppression")
library("GaussSuppression")
library("SSBtools")
library(tidyverse)
library(curl)
library(readxl)

rm(list=ls())



#######################################################################################################################################################################################################
#Exemple donnée avec la doc de la fonction GaussSuppression #faire varier les valeurs
# Input data
df <- data.frame(values = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                 var1 = rep(1:3, each = 5), 
                 var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)


df <- data.frame(values = c(1, 1, 1, 1, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0), 
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
datF$primary[datF$values < 3 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary
datF$suppressedB <- datF$primary
datF$suppressedC <- datF$primary

# zero secondary suppressed
datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA

# zero not secondary suppressed by first in ordering  (faire un exemple où il est obligé de toucher à des zéros et voir si il le fait quand même )
datF$suppressedB[GaussSuppression(x, c(which(datF$values == 0), which(datF$values > 0)), 
                                  primary = is.na(datF$primary))] <- NA

# with singleton # (comprendre comment la méthode du singleton fonctionne  et revoir les paiers pour voir si ils en parlent) 
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

#il prefere toucher au marge que aux zeros 






