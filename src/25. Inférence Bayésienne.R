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
U <- runif(n, 0, 1)

# Découpage en p classes
N <- trunc(p * U)
table(N)

# Test du chi2
#   H0 : Ui iid ~ U[0,1] 
chisq.test(table(N))


# ------------------------------------------------------------------------
# Test de Kolmogorov-Smirnov
# ------------------------------------------------------------------------

# Pour comparer les fonctions de répartition empiriques et théoriques
# H0 : Ui iid ~ U[0,1]

ks.test(U, "punif", 0 ,1)

# ECDF : Empirical Cumulative Distribution Function
plot(ecdf(U), col = "blue", main = "Comparaison de fonctions de répartitions")
curve(punif(x), col = "red", add=TRUE)
legend("topleft", inset=.05, lty=c(1, 1), 
       c("Empirique", "Théorique"), 
       col=c("blue","red"))


