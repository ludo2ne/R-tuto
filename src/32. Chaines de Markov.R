#**************************************************************************
#* Chaines de Markov                                                      *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())


# ------------------------------------------------------------------------
# Création d'une chaine de Markov
# ------------------------------------------------------------------------

# etats
states <- c(1, 2, 3, 4)

# Matrice de transition
P <- rbind(c(  0,   1,   0,   0),
           c(1/2,   0, 1/2,   0),
           c(  0, 1/3, 1/6, 1/2),
           c(  0,   0, 3/8, 5/8))


# Initialisation
n <- 200
current_state <- 1
res <- rep(NA, n)
res[1] <- 1

# Génération des valeurs
for (i in 2:n) {
  new_state <- sample(states, prob = P[current_state,], size = 1)
  current_state <- new_state
  res[i] <- current_state
}

res

# Affichage sous forme de série temporelle
plot(ts(res), type = "o", pch = 20, col = "blue")

# Calcul des fréquences
#   tend vers 0.1 0.2 0.3 0.4 quand n tend vers l'infini
table(res)
prop.table(table(res))


# ------------------------------------------------------------------------
# Marche aléatoire en 2D
# ------------------------------------------------------------------------

marche_aleatoire_2d <- function(n = 1000){
  # Coordonnées des points
  x <- rep(0, n)
  y <- rep(0, n)
  
  # Test si l'on va en haut, en bas, à gauche, à droite
  for (i in 2:n) {
    val <- sample(c(1, 2, 3, 4), prob = c(0.25, 0.25, 0.25, 0.25), size = 1)
    if (val == 1){
      x[i] = x[i-1] + 1
      y[i] = y[i-1]
    }
    if (val == 2){
      x[i] = x[i-1] - 1
      y[i] = y[i-1]
    }
    if (val == 3){
      x[i] = x[i-1]
      y[i] = y[i-1] + 1
    }
    if (val == 4){
      x[i] = x[i-1]
      y[i] = y[i-1] - 1
    }
  }
  
  # Affichage
  plot(x, y, typ = "l")
  points(0, 0, pch = 3, col = "blue", lwd = 2)
  points(x[length(x)], y[length(y)], pch = 3, col = "green", lwd = 2)
}

marche_aleatoire_2d(50000)

# Centre
c(mean(x), mean(y))

