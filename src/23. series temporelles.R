#**************************************************************************
#* Séries Temporelles avec R                                              *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list = ls())

# -----------------------------------------------------------------------------
# Generer une serie temporelle
# -----------------------------------------------------------------------------


# Fonction pour generer une serie temporelle de n elements
#   avec une tendance saisonniere p periodique
generer_serie_temporelle <- function(n, p) {
  start = as.Date("2022-01-01")
  day <- seq(1,n)
  date <- start + day
  m_t <- 400 - 0.5 * day - 1/ (2*n) * day^2            # tendance polynomiale deterministe
  s_t <- 20 * sin(2 * pi * day / p)                  # tendance saisonniere p-periodique
  Z_t <- rnorm(n, 0, 10)                           # bruit
  
  X_t <- m_t + s_t + Z_t
  plot(x = date, y = X_t, type="l")
  
  return(data.frame(X_t, m_t, s_t, Z_t))
}


n <- 365
p <- 50


# Generation d une serie temporelle a partir de la fonction
df <- generer_serie_temporelle(n, p)

# Verification que le bruit est centre
mean(df$Z_t)

# Visualisation du bruit
plot(df$Z_t, type ="l")

# On verifie que la saisonnalite est de moyenne nulle sur une periode p
total_saisonnalite <- 0
for (i in seq(1, floor((n-p)/p) * p)) {
  total_saisonnalite <- total_saisonnalite + sum(df$s_t[i:i+p])
}
mean(total_saisonnalite)

# Visualisation de la saisonnalite
plot(df$s_t, type ="l")

# Backward : Operateur retard de p valeurs
B <- function(X_t, p){
  if (p > 0){
    X_t_moins_p <- c(rep(0,p), X_t[1:(length(X_t)-p)])
  }
  else{
    X_t_moins_p <- c(X_t[(-p+1):length(X_t)], rep(0,-p))
  }
  
  return(X_t_moins_p)
}

jours <- rep(c("L","M", "M", "J", "V", "S", "D"),2)
jours
B(jours, 3)

# Fonctionne egalement avec p negatif (Forward)
jours
B(jours, -2)




# Filtre lineaire M(B)
MB <- function(a_k, k, X_t){
  somme <- 0
  for (i in length(k)){
    somme <- somme + a_k[i] * B(X_t, k[i])
  }
  return (somme)
}

a_k <- c(1, 12, 20, -5)
k <- c(3, 5, -4, -10)

# A verifier...
# voir aussi fonction filter
MB(a_k, k, df$X_t)



#########################################################################################################
# TODO
#########################################################################################################









# Exo 9 a 11

# ---------------------------------------------------------------
# Identification des processus ARMA
# ---------------------------------------------------------------

n <- 1000
epsilon <- rnorm(n+1,mean=0,sd=1)

head(epsilon[-1])
head(epsilon[-(n+1)])

# Realisation d'un MA(1)
X <- 3 + epsilon[-1] + 2* epsilon[-(n+1)]
head(X)


eta <- rnorm(n+2,mean=0,sd=sqrt(2))

# Realisation d'un MA(2)
Y <- eta[-c(1,2)] -1/2* eta[-c(1,n+2)] + 1/3* eta[-c(n+1, n+2)]
head(Y)

Z <- X + Y
print(Z)

plot(X, type="l", col = "red")
plot(Y, type="l", col = "blue")
plot(Z, type="l", col = "darkgreen")

acf(X, type="correlation")
# index du dernier pic significatif : q=1 => MA(1)
pacf(X)
# decroissante exponentielle : AR : p=0
# Donc on a probablement ici un MA(1)

acf(Y, type="correlation")
pacf(Y)

acf(Z, type="correlation")
pacf(Z)
