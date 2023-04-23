#**************************************************************************
#* Inférence Bayésienne                                                   *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())


##########################################################################
# Mener une inférence bayésienne
#   inférence sur la moyenne
##########################################################################

# Nous cherchons à estimer la taille moyenne mu des élèves de 6e
# Notre à priori est que la taille suit une loi Normale N(140, 5)

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
# Simulation d'un échantillon de la loi exponentielle
#   à partir de la loi Uniforme
#   par inversion de la fonction de répartition
# ------------------------------------------------------------------------

n <- 150
lambda <- 4

u <- runif(n, 0, 1)
e <- -log(u) / lambda   # inverse de la fonction de répartition de la loi exponentielle

# Visualisation
plot(ecdf(e), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pexp(x, 4), col = "red", add=TRUE)

# Test de Kolmogorov-Smirnov
ks.test(e, "pexp", lambda)


# ------------------------------------------------------------------------
# Simulation d'un échantillon de loi discrète
#   à partir de la loi Uniforme
#   par inversion de la fonction de répartition
# ------------------------------------------------------------------------

n <- 1000

# Dé truqué qui tombe plus souvent sur le 1
p <- c(5/10, 1/10, 1/10, 1/10, 1/10, 1/10)

u <- runif(n, 0, 1)
X <- 1 + (u > cumsum(p)[1]) + (u > cumsum(p)[2]) + (u > cumsum(p)[3]) + (u > cumsum(p)[4]) + (u > cumsum(p)[5])

# Répartition
prop.table(table(X))
plot(table(X)/n)

# Test du chi2
chisq.test(table(X), p = p)


# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet
#   Densité connue, bornée par une constante m et admet un support compact
#   Génération d'une loi Beta(2,2)
# ------------------------------------------------------------------------

rm(list=ls())

set.seed(888)

n <- 1000

# Afficher un graph sans rien
plot(100, 100,
     xlim = c(-0.2, 1.2), ylim = c(0, 6),
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
hist(b22, freq = FALSE, col = "grey", breaks = 50, main = "Distribution générée")
curve(dbeta(x, 2, 2), 0, 1, add = TRUE, col = "red")

# Fonction de répartition
plot(ecdf(b22), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(pbeta(x, shape1 = 2, shape = 2), col = "red", add = TRUE)

# Test de Kolmogorov-Smirnov
ks.test(b22, "pbeta", 2, 2)


# ------------------------------------------------------------------------
# Algorithme d'Acceptation-Rejet (autres possibilités)
#   Densité connue, bornée par une fonction
#   Densité connue à une constante multiplicative près (ex : Gamma(3/2, 1))
# ------------------------------------------------------------------------


##########################################################################
# Méthode de Monte-Carlo
##########################################################################

# Rappels :
# Nous souhaitons mener l'inférence pour calculer par exemple :
# - la moyenne à postériori
# - la variance à postériori
# - un intervalle de crédibilité de theta
# Si nous savons générer selon la loi à postériori, nous allons pouvoir
# calculer des estimations MC de ces quantités
# Le but de la méthode MC est d'évaluer l'intégrale d'une fonction h


# ------------------------------------------------------------------------
# Premier algo MC très simple
#   intégrale de x**2 entre 0 et 1
# ------------------------------------------------------------------------

rm(list=ls())

set.seed(888)

n <- 1000
U <- runif(n, 0, 1)

h <- function(x){
  x**2
}

# On obtient une valeur proche de 1/3
I_hat <- sum(h(U)) / n ; I_hat



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
Pi_hat <- 4 * I_hat_n ; Pi_hat

# Variance empirique
S2n <- 16 * sum((g(X, Y) - I_hat_n)**2) / (n - 1)

# Erreur de Monte-Carlo
sqrt(S2n) / sqrt(n)

# Intervalle de confiance à 95% de Pi
c(Pi_hat - qnorm(0.975) * sqrt(S2n) / sqrt(n),
  Pi_hat + qnorm(0.975) * sqrt(S2n) / sqrt(n))


estimer_pi <- function(n){
  res <- rep(NA, n)
  for (i in 1:n) {
    for(j in 1:i){
      X <- runif(j)
      Y <- runif(j)
      res[j] <- 4 * sum(1 * (X^2 + Y^2 <= 1)) / j
    }
  }
  plot(ts(res), main = "Moyennes ergodiques")
  abline(h = pi, col = "red")
  return(res)
}

estimer_pi(200)


# ------------------------------------------------------------------------
# Estimation par Monte-Carlo
# de intégrale entre 0 et 1 de exp(-x)/(1 + x**2)
# ------------------------------------------------------------------------

TODO cours p66


# Avec un échantillon de U[0,1]



# Avec un échantillon de exp(1)



# Avec un éhcnatillon de Cauchy



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
# Méthode de Monte-Carlo par chaine de Markov (MCMC)
##########################################################################

# Si l'on ne sait pas simuler la loi f, on ne peut pas appliquer la méthode de Monte-Carlo
# On va utiliser une chaine de Markov de loi stationnaire f

# ------------------------------------------------------------------------
# Algorithme de Metropolis Hasting
# ------------------------------------------------------------------------

set.seed(5555)

# Cette fonction crée une chaine de Markov qui a pour loi stationnaire
# la loi Normale Centrée Réduite
#   x0 : état initial de la chaine
#   L : longeur de la chaine
#   s : paramètre
chaine_mcmc_N01 <- function(x0, L, s){
  res <- rep(NA, L)
  res[1] <- x0
  for (l in 2:L){
    x_old <- res[l-1]
    x_new <- runif(1, min = x_old - 2 * s, max = x_old + 2 * s)    # générer un candidat
    u_new <- runif(1)
    
    # En gros on regarde si la nouvelle valeur est plus plausible que l'ancienne
    rho <- pmin(1, dnorm(x_new) / dnorm(x_old))
    
    # Si oui (rho = 1) 
    #   on accepte la nouvelle valeur
    # Si non
    #   on va parfois accepter, parfois rejeter la nouvelle valeur
    if (u_new < rho){               # on accepte le nouveau candidat
      res[l] <- x_new
    }
    else{                           # on conserve l'ancienne valeur
      res[l] <- x_old
    }
  }
  res
}

# Exemple
# On part volontairement d'une valeur x0 éloignée de la moyenne
# La valeur du paramètre s doit être :
#   - assez grande pour converger rapidement
#   - mais pas trop pour éviter des effets plateau
Y <- chaine_mcmc_N01(x0 = -10, L = 1600, s = 0.8)

# La convergence vers des valeurs classiques d'une N(0,1) est rapide
plot(ts(Y), main = "Série temporelle")

# La distribution ressemble à celle d'une N(0,1)
hist(a, breaks = 100, main = "Histogramme")

# Utilisation de la librairie coda
library(coda)

Y <- as.mcmc(Y)
summary(Y)

# Série temporelle
traceplot(Y)

# Si on retire 600 obs correspondant à la période de rodage de la chaine
Y <- Y[-(1:600)]
Y <- as.mcmc(Y)

# Quantiles ergodiques
cumuplot(Y)

# Estimation de la densité
densplot(Y)

# Autocorrélation (bien car cela décroit rapidement)
autocorr.plot(Y)

# Tableau 1
#   Mean           : I_L     
#   SD             : Variance empirique V(I_L)  
#   Naive SE       : Erreur MC si echantillon iid         
#   Time-series SE : Erreur MCMC
# Tableau 2
#   quantiles empiriques
summary(Y)

