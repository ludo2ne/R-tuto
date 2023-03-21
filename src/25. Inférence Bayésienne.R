#**************************************************************************
#* Inférence Bayésienne                                                   *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")


# ------------------------------------------------------------------------
# Générateur de nombres aléatoires
# ------------------------------------------------------------------------

# R utilise le générateur de Mersenne-Twister de période 2^19937-1

set.seed(0)
runif(4)

set.seed(1)
runif(3)

set.seed(0)

# ------------------------------------------------------------------------
# Test du chi2
# ------------------------------------------------------------------------

n <- 1000
p <- 5
u <- runif(n, 0, 1)

# Découpage en p classes
N <- trunc(p * u)
table(N)

# Test du chi2
#   H0 : Ui iid ~ U[0,1] 
chisq.test(table(N))


# ------------------------------------------------------------------------
# Test de Kolmogorov-Smirnov
# ------------------------------------------------------------------------

# Pour comparer les fonctions de répartition empiriques et théoriques
# H0 : Ui iid ~ U[0,1]

ks.test(u, "punif", 0 ,1)

# ECDF : Empirical Cumulative Distribution Function
plot(ecdf(u), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(punif(x), col = "red", add=TRUE)
legend("topleft", inset=.05, lty=c(1, 1), 
       c("Empirique", "Théorique"), 
       col=c("blue","red"))


# ------------------------------------------------------------------------
# Simulation de la loi exponentielle
# ------------------------------------------------------------------------

lambda <- 4
u <- runif(n, 0, 1)
e <- -log(u) / lambda

# Visualisation
plot(ecdf(e), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pexp(x, 4), col = "red", add=TRUE)

# Test de Kolmogorov-Smirnov
ks.test(e, "pexp", lambda)

# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet
#   Génération d'une loi Beta(2, 2)
# ------------------------------------------------------------------------

n <- 1000

plot(100, 100,
     xlim = c(0, 1), ylim = c(0, 6),
     xlab = "y", ylab = "f(y)",
     main = "Génération d'une loi Beta(2,2)")

x <- matrix(0, nr = 1, nc = n)

m = 6
i = 0
k = 0

while(i <= n){
  k <- k + 1
  y <- runif(1)
  u <- runif(1, 0, m)
  
  points(y, m, pch = 20, cex = 0.2, col = "red")
  points(y, u , pch = 20, cex = 0.2, col = "orange")
  
  if(u < 6 * y * (1 - y)){
    x[i] <- y
    i <- i + 1
    points(y, u, pch = 20, cex = 0.2, col = "green")
  }
}

matrix(c(k, k-n), ncol = 2, dimnames = list(" ", c("nb_iterations", "nb_rejets")))

curve(dbeta(x, shape1 = 2, shape2 = 2), bty="n", col = "purple", add = TRUE)
legend("left", inset=.05, lty=c(1, 1, NA, NA), pch = c(NA, NA, 20, 20), 
       c("Vraie densité", "m", "Points acceptés", "Points rejetés"), 
       col=c("purple", "red", "orange", "green"))

# Distribution générée
hist(x, freq = FALSE, col = "grey", breaks = 50, main = "Distribution générée")
curve(dbeta(x, 2, 2), 0, 1, add = TRUE, col = "red")

# Fonction de répartition
plot(ecdf(x), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pbeta(x, shape1 = 2, shape = 2), col = "red", add = TRUE)

# Test de Kolmogorov-Smirnov
ks.test(x, "pbeta", 2, 2)


# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet
#   Génération d'une loi Gamma(1.5, 1)
# ------------------------------------------------------------------------

# TODO fix or remove ?

n <- 1000
alpha <- 0.5
K <- exp(-0.5) / sqrt (2 * (1 - alpha))

x <- matrix(0, nr = 1, nc = n)

curve(dgamma(x, shape = 1.5, rate = 1), bty="n",
      from = 0, to = 10, ylim = c(0, 1),
      xlab = "y", ylab = "f(y)")

i = 0
k = 0

while(i <= n){
  k <- k + 1
  v <- runif(1)
  y <- -(1 / alpha) * log(v)          # y ~ loi exponentielle(alpha)
  u <- runif(1)
  
  points(y, sqrt(y) * exp(-(1 - alpha) * y) / K, pch = 20, cex = 0.2, col = "blue")
  # points(y, exp(-alpha * y) * K, pch = 20, cex = 0.2, col = "red")
  points(y, u, pch = 20, cex = 0.2, col = "orange")
  
  if(u <= sqrt(y) * exp(-(1 - alpha) * y) / K){
    x[i] <- y
    i <- i + 1
    points(y , u, pch = 20, cex = 0.2, col = "green")
  }
}

matrix(c(k, k-n), ncol = 2, dimnames = list(" ", c("iter", "rejet")))

# Fonction de répartition
plot(ecdf(x), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pgamma(x, shape = 1.5, rate = 1), col = "red", add = TRUE)

ks.test(x, "pgamma", 1.5, 1)




# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet                  BIS
#   Génération d'une loi Gamma(1.5, 1)
# ------------------------------------------------------------------------

# TODO fix or remove ?

n <- 1000
alpha <- 0.9
K <- exp(-0.5) / sqrt (2 * (1 - alpha))

x <- matrix(0, nr = 1, nc = n)

curve(dgamma(x, shape = 1.5, rate = 1), bty="n",
      from = 0, to = 10, ylim = c(0, 1),
      xlab = "y", ylab = "f(y)")

i = 0
k = 0

while(i <= n){
  k <- k + 1
  v <- runif(1)
  y <- -(1 / alpha) * log(v)          # y ~ loi exponentielle(alpha)
  m <- exp(-alpha * y) * K
  u <- runif(1, 0, m)
  
  points(y, m, pch = 20, cex = 0.2, col = "blue")
  # points(y, exp(-alpha * y) * K, pch = 20, cex = 0.2, col = "red")
  points(y, u, pch = 20, cex = 0.2, col = "orange")
  
  if(u <= sqrt(y) * exp(- y)){
    x[i] <- y
    i <- i + 1
    points(y , u, pch = 20, cex = 0.2, col = "green")
  }
}



# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet
#   Génération d'une loi Beta(3, 6)
# ------------------------------------------------------------------------

n <- 100000

sampled <- data.frame(proposal = runif(n, 0, 1))
sampled$targetDensity <- dbeta(sampled$proposal, 3, 6)

maxDens = max(sampled$targetDensity, na.rm = TRUE)
sampled$accepted = ifelse(runif(n, 0, 1) < sampled$targetDensity / maxDens, TRUE, FALSE)

hist(sampled$proposal[sampled$accepted], freq = FALSE, col = "grey", breaks = 100)
curve(dbeta(x, 3,6),0,1, add = TRUE, col = "red")



##########################################################################
# Méthode de Monte-Carlo
##########################################################################


# ------------------------------------------------------------------------
# Calcul de Pi
# ------------------------------------------------------------------------

set.seed(100)

n <- 10000

X <- runif(n)
Y <- runif(n)

# g renvoie 1 si le point (x, y) est dans le disque de centre (0, 0) et de rayon 1
g <- function(x, y){
  return (1 * (x^2 + y^2 <= 1))
}

I_hat_n <- sum(g(X, Y)) / n

# Estimation de Pi (Aire du disque de rayon 1)
#   on multiplie par 4 car X et Y sont à valeurs dans [0, 1] 
#   donc n'appartiennent qu'au quart de cercle supérieur droit
4 * I_hat_n

# Estimation de la variance par la méthode de Monte-Carlo
sigma_hat_n <- sum(g(X, Y)^2) / n - I_hat_n^2

# Comme g = g², la variance est directement égale à
I_hat_n - I_hat_n^2


# Intervalle de confiance à 95% de Pi
4 * c(I_hat_n - qnorm(0.975) * sqrt(sigma_hat_n) / sqrt(n),
      I_hat_n + qnorm(0.975) * sqrt(sigma_hat_n) / sqrt(n))


estimer_pi <- function(n){
  res <- rep(NA, n)
  for (i in 1:n) {
    for(j in 1:i){
      X <- runif(j)
      Y <- runif(j)
      res[j] <- 4 * sum(1 * (X^2 + Y^2 <= 1)) / j
    }
  }
  plot(ts(res))
  abline(h = pi, col = "red")
  return(res)
}

estimer_pi(200)

# ------------------------------------------------------------------------
# X ~ Binom(p)
# p ~ Beta(1/2, 1/2)
# ------------------------------------------------------------------------

set.seed(222)

n <- 100

P <- rbeta(n, 0.5, 0.5)

# On observe k réalisations de X et on cherche la loi à postériori de p
# et l'estimation de p par moyenne à postériori
k <- 10
sum_X_j <- 7

p_hat <- sum(P * P^sum_X_j * (1 - P)^(k - sum_X_j)) / sum(P^sum_X_j * (1 - P)^(k - sum_X_j)) 




##########################################################################
# Méthode de Monte-Carlo par chaine de Markov
##########################################################################

# Si l'on ne sait pas simuler la loi f, on ne peut pas appliquer la méthode de Monte-Carlo
# On va utiliser une chaine de Markov de loi stationnaire f

# ------------------------------------------------------------------------
# Algorithme de Metropolis Hasting
# ------------------------------------------------------------------------






