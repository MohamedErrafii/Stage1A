rm(list=ls())
#install.packages("GaussSuppression")
library(GaussSuppression)
library(SSBtools)
# Données initiales
df <- data.frame(values = c(3, 4, 7, 2), 
                 var1 = c("homme", "homme", "femme", "femme"), 
                 var2 = c("p", "np", "p", "np"), stringsAsFactors = FALSE)

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

echelonner_matrice <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  
  # Travailler sur une copie de la matrice originale pour ne pas la modifier directement et la préserver pour d'autres opérations
  matrice_echelonnee <- A
  
  # Indice pour suivre la colonne et la ligne
  pivot_col <- 1
  pivot_row <- 1
  
  while (pivot_row<n) {
    while (pivot_col<(m+1)){
      #recherche d'une colonne non nul
      non_zero_col<-c()
      for (i in pivot_row:n){
        if (matrice_echelonnee[i,pivot_col] !=0){
          non_zero_col<-c(non_zero_col,i)
        }
      }
      if (any(!is.na(non_zero_col))) {
        r<-non_zero_col[1]
        #échange de ligne si nécessaire
        if (pivot_row != r) {
          matrice_echelonnee[c(pivot_row, r), ] <- matrice_echelonnee[c(r, pivot_row), ]
        }
        # Diviser la ligne du pivot par le pivot(normalement inutile car la matrice de contribution est une "dummy matrix"(composée de zéro et un))
        pivot_value <- matrice_echelonnee[pivot_row, pivot_col]
        matrice_echelonnee[pivot_row, ] <- matrice_echelonnee[pivot_row, ] / pivot_value
        
        # Élimination des éléments en dessous du pivot
        for (elim_row in (pivot_row + 1):n) {
          elimination_factor <- matrice_echelonnee[elim_row, pivot_col]
          matrice_echelonnee[elim_row, ] <- matrice_echelonnee[elim_row, ] - elimination_factor * matrice_echelonnee[pivot_row, ]
          
        }
       # Passer à la ligne suivante
        pivot_row <- pivot_row + 1
        
       
      }
      pivot_col <- pivot_col + 1
    }
  }
  return(matrice_echelonnee)
}
    
  

# Exemple d'utilisation avec une matrice singulière
A <- matrix(c(1, 2, 3, 2, 4, 6), nrow = 2, byrow = TRUE)

# Échelonner la matrice
matrice_echelonnee <- echelonner_matrice(A)

print("Matrice originale :")
print(A)

print("Matrice échelonnée :")
print(matrice_echelonnee)



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
y<-echelonner_matrice(X3)
y
a<-nrow(y)
a
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



y<-echelonner_matrice(x)
a<-nrow(y)
b<-y[a,]
?get0

rm(list=ls())
#install.packages("GaussSuppression")
library(GaussSuppression)
library(SSBtools)

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
# Make output data frame and x 
fs <- FormulaSums(df, values ~ var1 * var2, crossTable = TRUE, makeModelMatrix = TRUE)
x <- fs$modelMatrix
colnames(x)
datF <- data.frame(fs$crossTable, values = as.vector(fs$allSums))

# Add primary suppression 
datF$primary <- datF$values
datF$primary[datF$values < 2 ] <- NA #& datF$values > 0 ] <- NA
datF$suppressedA <- datF$primary

# Avec GaussSuppression : 
# zero secondary suppressed
datF$suppressedA[GaussSuppression(x, primary = is.na(datF$primary))] <- NA

print(datF)

# Sans GaussSuppression 

echelonner_matrice <- function(A) {
  n <- nrow(A)
  m <- ncol(A)
  
  # Travailler sur une copie de la matrice originale pour ne pas la modifier directement et la préserver pour d'autres opérations
  matrice_echelonnee <- A
  
  # Indice pour suivre la colonne et la ligne
  pivot_col <- 1
  pivot_row <- 1
  
  while (pivot_row <= (n-1) && pivot_col <= m) {
    # Recherche d'une colonne non nulle
    non_zero_col <- c()
    for (i in pivot_row:n) {
      if (matrice_echelonnee[i, pivot_col] != 0) {
        non_zero_col <- c(non_zero_col, i)
      }
    }
    if (length(non_zero_col) > 0) {
      r <- non_zero_col[1]
      # Échange de ligne si nécessaire
      if (pivot_row != r) {
        matrice_echelonnee[c(pivot_row, r), ] <- matrice_echelonnee[c(r, pivot_row), ]
      }
      # Diviser la ligne du pivot par le pivot
      pivot_value <- matrice_echelonnee[pivot_row, pivot_col]
      if (pivot_value != 0) {  # Ajouter une vérification pour éviter la division par zéro
        matrice_echelonnee[pivot_row, ] <- matrice_echelonnee[pivot_row, ] / pivot_value
      }
      
      # Élimination des éléments en dessous du pivot
      for (elim_row in (pivot_row + 1):n) {
        elimination_factor <- matrice_echelonnee[elim_row, pivot_col]
        matrice_echelonnee[elim_row, ] <- matrice_echelonnee[elim_row, ] - elimination_factor * matrice_echelonnee[pivot_row, ]
      }
      # Passer à la ligne suivante
      pivot_row <- pivot_row + 1
    }
    pivot_col <- pivot_col + 1
  }
  return(matrice_echelonnee)
}

# Exemple d'utilisation avec une matrice singulière
A <- matrix(c(1, 2, 3, 2, 4, 6), nrow = 2, byrow = TRUE)

# Échelonner la matrice
matrice_echelonnee <- echelonner_matrice(A)

print("Matrice originale :")
print(A)

print("Matrice échelonnée :")
print(matrice_echelonnee)

#réordonner les colonnes pour mettre les colonnes du secret primaire en  dernier
f <- rownames(datF[datF$values < 2 ,])#& datF$values > 0,])
g <- paste0(datF[f, "var1"], "-", datF[f, "var2"])
X <- x[, g]
f2 <- rownames(datF[!(datF$values < 2),])# & datF$values > 0),])
g2 <- paste0(datF[f2, "var1"], "-", datF[f2, "var2"])
X2 <- x[, g2]
X3 <- cbind(X2, X)
colnames(X3)
y <- echelonner_matrice(X3)
y
a <- nrow(y)
b <- y[a,]
b

indices_non_zero <- which(b != 0)

print(indices_non_zero)

y_complet <- echelonner_matrice(x)
a_complet <- nrow(y_complet)
b_complet <- y_complet[a_complet,]

print(b_complet)




