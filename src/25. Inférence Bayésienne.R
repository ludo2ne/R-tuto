#**************************************************************************
#* Inférence Bayésienne                                                   *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")


##########################################################################
# Mener une inférence bayésienne
#   inférence sur la moyenne
##########################################################################

# Nous cherchons à estimer la taille moyenne mu des élèves de 6e
# Notre à priori est que la taille suit une loi Normale N(140, 10)

mu0 <- 140
v0 <- 5

curve(dnorm(x, mean = mu0, sd = v0), 
      from = mu0 - 5 * v0, 
      to = mu0 + 5 * v0, 
      bty = "n", 
      main = "Densité de la loi N(140, 5)")

# Nous allons ensuite utiliser un échantillon de 15 valeurs de taille relevées
# pour affiner cette estimation

x <- c(133, 152, 170, 166, 155, 142, 139, 145, 149, 142, 165, 135, 150, 144, 148)
n <- length(x)
x_bar <- mean(x)

# Supposons que la vraisemblance suive une loi Normale N(mu, 15)

sigma0 <- 30

mu_post <- (mu0 * (sigma0 / n)  + x_bar * v0) / ((sigma0 / n) + v0)
v_post <- sigma0 * v0 / (sigma0 * n + v0) 

# La postériore est donc
curve(dnorm(x, mean = mu_post, sd = v_post), 
      col = "red",
      add = TRUE)

# Ainsi la taille moyenne après prise en compte des données vaut
mu_post

# Intervalle de crédibilité
c(qnorm(0.025, mu_post, v_post), 
  qnorm(0.975, mu_post, v_post))

# Dans l'exemple ci-dessous la priore était peu informative car la variance
# v0 était assez grande. Ainsi la prise en compte des données a fortement
# influé sur la valeur finale de mu. mu converge rapidement vers la taille
# moyenne de l'échantillon.

# Nous pouvons refaire l'exercice en choisissant une priore très informative
# c'est à dire avec un v0 très petit. Dans ce cas l'information apportée par
# la priore va prédominer par rapport aux données.
# Ainsi la valeur à postériori de mu va rester proche de la priore

v0_bis <- 1
mu_post_bis <- (mu0 * (sigma0 / n)  + x_bar * v0_bis) / ((sigma0 / n) + v0_bis)
mu_post_bis

curve(dnorm(x, mean = mu0, sd = v0),
      from = 120, to = 160, 
      ylim = c(0, 0.5),
      col = "purple",
      bty = "n",
      main = "Comparaison de lois à priori")

curve(dnorm(x, mean = mu0, sd = v0_bis),
      col = "red",
      add = TRUE)

rug(mu_post, col = "purple", lwd = 2, ticksize = 0.3, lty = "dashed")
rug(mu_post_bis, col = "red", lwd = 2, ticksize = 0.3, lty = "dashed")

legend("topright", inset = .05, lty = c(1, 1, 2), 
       c("Priore peu informative", "Priore informative", "Moyennes à postériori"), 
       col = c("purple", "red", "black"))


# Par la suite, nous allons utiliser une méthode de Monte-Carlo
# pour générer un échantillon iid de la postériore


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
# Test du chi2 d'adéquation
#   H0 : Vérifie si un échantillon suit bien une certaine variable aléatoire 
# ------------------------------------------------------------------------

n <- 1000
p <- 5
u <- runif(n, 0, 1)

# Découpage en p classes
N <- trunc(p * u)
table(N)

# Test du chi2 d'adéquation
#   H0 : Ui iid ~ U[0,1] 
#   La p-valeur est grande donc on ne rejette pas H0
chisq.test(table(N))


t <- chisq.test(table(N))
Zn <- sum((t$observed - n / p)^2) / (n / p)     # Stat de test
Zn > qchisq(0.95, df = p-1)                     # Rejet de H0 ?


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

# Afficher un graph sans rien
plot(100, 100,
     xlim = c(0, 1), ylim = c(0, 6),
     xlab = "y", ylab = "f(y)",
     main = "Génération d'une loi Beta(2,2)")

b22 <- matrix(0, nr = 1, nc = n)

m = 6     # majorant
i = 0
k = 0

while(i <= n){
  k <- k + 1
  y <- runif(1)
  u <- runif(1, 0, m)
  
  points(y, m, pch = 20, cex = 0.2, col = "red")
  points(y, u , pch = 20, cex = 0.2, col = "orange")
  
  if(u < 6 * y * (1 - y)){
    b22[i] <- y
    i <- i + 1
    points(y, u, pch = 20, cex = 0.2, col = "green")
  }
}

matrix(c(k, k-n), ncol = 2, dimnames = list(" ", c("nb_iterations", "nb_rejets")))

curve(dbeta(x, shape1 = 2, shape2 = 2), bty="n", col = "purple", add = TRUE)
legend("left", inset=.05, lty=c(1, 1, NA, NA), pch = c(NA, NA, 20, 20), 
       c("Vraie densité", "m", "Points acceptés", "Points rejetés"), 
       col=c("purple", "red", "orange", "green"))


# Densité à priori
curve(dbeta(x, 2, 2), col = 2, xlim = c(-1, 2), main = "Densité de la loi Beta(2,2)")
rug(b22)

# QQ-plot
qqplot(b22, rbeta(n, 2, 2), pch = 20)
abline(0, 1, col = "orange")

# Distribution générée
hist(x, freq = FALSE, col = "grey", breaks = 50, main = "Distribution générée")
curve(dbeta(x, 2, 2), 0, 1, add = TRUE, col = "red")

# Fonction de répartition
plot(ecdf(x), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pbeta(x, shape1 = 2, shape = 2), col = "red", add = TRUE)

# Test de Kolmogorov-Smirnov
ks.test(x, "pbeta", 2, 2)


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






