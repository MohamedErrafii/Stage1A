rm(list=ls())
install.packages("GaussSuppression")
library(GaussSuppression)
library(SSBtools)
# Données initiales
df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
                 var1 = rep(1:3, each = 5), 
                 var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)

# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 5 & datF$values > 0] <- NA
datF$suppressedA <- datF$primary

#Avec GaussSuppression : 
# zero secondary suppressed
datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA

datF

#Sans GaussSuppression 

gaussian_elimination <- function(A) {
  n <- nrow(A)
  
  cat("Matrice initiale:\n")
  print(A)
  cat("\n")
  
  # Étape de la triangulation
  for (i in 1:(n-1)) {
    # Recherche du maximum dans la colonne i
    max_row <- which.max(abs(A[i:n, i])) + (i-1)
    
    
    # Échange des lignes si nécessaire
    if (max_row != i) {
      cat("Échanger les lignes", i, "et", max_row, ":\n")
      A[c(i, max_row), ] <- A[c(max_row, i), ]
      print(A)
      cat("\n")
    }
    
    # Élimination des coefficients en dessous du pivot
    if (A[i,i] !=0){
      for (j in (i+1):n) {
        factor <- A[j, i] / A[i, i]
        A[j, ] <- A[j, ] - factor * A[i, ]
        cat("Éliminer les coefficients en dessous du pivot dans la colonne", i, ":\n")
        print(A)
        cat("\n")
      }
    }
    else {
      if(i>1){
        L<-A[1:(i-1),]
        print(L)
        M<-A[i:n,1:i]
        print(M)
        N<-A[i:n,(i+1):n]
        P<-gaussian_elimination(N)
        print(P)
        Q<-cbind(M,P)
        print(Q)
        R<-rbind(L,Q)
        R
      }
      else {
        M<-A[i:n,1:i]
        print(M)
        N<-A[i:n,(i+1):n]
        P<-gaussian_elimination(N)
        print(P)
        Q<-cbind(M,P)
        print(Q)
      }
    }
    
  }
  A
}



f<-rownames(datF[datF$values<3 & datF$values>0 ,])
f
g<-paste0(datF[f,"var1"],"-",datF[f,"var2"])
g
X<-x[,g]
X
f2<-rownames(datF[!(datF$values<3 & datF$values>0) ,])
f2
g2<-paste0(datF[f2,"var1"],"-",datF[f2,"var2"])
g2
X2<-x[,g2]
X2
X3<-cbind(X2,X)
X3
y<-gaussian_elimination(X3)
a<-nrow(y)
b<-y[a,]
b
#b[1]
#b[23]
a <- c()
for (i in 1:length(b)){
  if (b[i]!=0){
    a<-append(a,i)
  }
  a
}



y<-gaussian_elimination(x)
a<-nrow(y)
b<-y[a,]
?get0
