rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")

# ---------------------------------------------------------------------
# Mes premieres matrices
# ---------------------------------------------------------------------

m_34 <- matrix(seq(1:12),
             nrow = 3,
             ncol = 4)

m_34  # Affichage de la matrice


# Dimensions de la matrice
dim(m_34)
dim(m_34)[1]  # Lignes
dim(m_34)[2]  # Colonnes


matrix(data = 0,               
       nrow = 5,
       ncol = 3)

matrix(data = c(1,2,3),               
       nrow = 5,
       ncol = 3)

# ---------------------------------------------------------------------
# Creer une matrice 
# ---------------------------------------------------------------------

# Creer une matrice diagonale
m_diag <- diag(c(1, 2, 4, 9))  
m_diag


# Creer une matrice a partir de vecteurs
v1 <- c(1,2,3)
v2 <- c(2,2,2)
v3 <- c(3,0,1)

rbind(v1, v2)              # Associer des vecteurs en ligne
m_33 <- cbind(v1, v2, v3)  # Associer des vecteurs en colonne
m_33

# Creer une matrice a partir d un dataframe
df <- data.frame(v1, v2)
data.matrix(df)


# ---------------------------------------------------------------------
# Operations de base
# ---------------------------------------------------------------------

m_diag + m_diag      # Somme des termes ij
m_diag * m_diag      # Produit des termes ij
m_34 %*% m_diag      # Produit matriciel
t(m_34)              # Transposee
det(m_33)            # Determinant
solve(m_33)          # Inversion
eigen(m_33)          # Valeurs et vecteurs propres
eigen(m_33)$values
eigen(m_33)$vectors







