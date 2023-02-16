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

# R utilise le générateur de Mersenne-Twister de période 

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
