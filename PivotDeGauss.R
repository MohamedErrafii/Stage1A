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
    
  }
}

# Exemple d'utilisation
A <- matrix(c(2, 0, -1, -3, 0, 2, -2, 0, 2), nrow = 3, byrow = TRUE)
gaussian_elimination(A)
