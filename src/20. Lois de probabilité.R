#**************************************************************************
#* Lois usuelles                                                          *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************


rm(list=ls())


###############################################################################
# Lois continues
###############################################################################

# -----------------------------------------------------------------------------
# Loi Uniforme
# -----------------------------------------------------------------------------

runif(1, min = 0, max = 1)

curve(dunif(x, min = 0, max = 1), col = "red", xlim = c(-0.2, 1.2), main = "Densité de la loi Uniforme")
curve(punif(x, min = 0, max = 1), col = "red", xlim = c(-0.2, 1.2), main = "Fonction de répartition de la loi Uniforme")


# -----------------------------------------------------------------------------
# Loi Exponentielle
# -----------------------------------------------------------------------------

lambda <- 2               # Paramétre de la loi exponentielle
rexp(1, rate = lambda)    # Générer une valeur

curve(dexp(x, rate = lambda), col = "red", xlim = c(0, 5), main = "Densité de la loi Exponentielle")
curve(pexp(x, rate = lambda), col = "red", xlim = c(0, 5), main = "Fonction de répartition de la loi Exponentielle")
curve(1 - pexp(x, rate = lambda), col = "red", xlim = c(0, 5), main = "Fonction de survie de la loi Exponentielle")


# -----------------------------------------------------------------------------
# Loi Normale
# -----------------------------------------------------------------------------

# Densité
x <- seq(-5,5, by=0.1)
plot(x = x, 
     y = dnorm(x), 
     type = "l",
     main= "Densité de la loi Normale centrée réduite")

# Fonction de réparition
plot(x = x, 
     y = pnorm(x), 
     type = "l",
     main= "Fonction de répartition de la loi Normale centrée réduite")

# Quantiles
x1 <- seq(0,1, by=0.01)
plot(x = x1,
     y = qnorm(x1), 
     xlim = c(0,1),
     type = "l",
     main= "Fonction de répartition de la loi Normale centrée réduite")
abline(h=1.96, col="red")
abline(v=0.975, col="red")
legend("topleft", inset=.05, lty=c(1, 1),
       c("Quantile 0.975 = 1.96"), 
       col=c("red"))

x
qnorm(pnorm(x))


# -----------------------------------------------------------------------------
# Loi du Chi2
# -----------------------------------------------------------------------------

rchisq(1, df = 4)       # Générer une valeur de la loi de chi2 à 4 degrés de liberté
qchisq(0.95, df = 3)    # Quantile d'ordre 0.95 de la loi de chi2 à 3 degrés de liberté
pchisq(10, df = 2)      # Fonction de répartition


# Visualisation de la densité
curve(dchisq(x, df = 1), from = 0, to = 50, col = "blue",
      xlab = "x",
      ylab = "Densité", 
      main = "Loi du Chi2")
curve(dchisq(x, df = 2), from = 0, to = 50, col = "cyan", add = TRUE)
curve(dchisq(x, df = 3), from = 0, to = 50, col = "red", add = TRUE)
curve(dchisq(x, df = 4), from = 0, to = 50, col = "green", add = TRUE)
curve(dchisq(x, df = 10), from = 0, to = 50, col = "purple", add = TRUE)
curve(dchisq(x, df = 20), from = 0, to = 50, col = "orange", add = TRUE)

legend("topright", inset=.05, lty = 1,
       c("1","2","3","4","10","20"), 
       col = c("blue", "cyan", "red", "green", "purple", "orange"),
       title = "Degrés de liberté")


# -----------------------------------------------------------------------------
# Loi de Student
# -----------------------------------------------------------------------------

plot_student_distribution <- function() {
  x <- seq(-4, 4, length=100)
  hx <- dnorm(x)
  
  df <- c(1, 3, 8, 30)
  colors <- c("red", "blue", "darkgreen", "gold", "black")
  labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
  
  plot(x = x, 
       y = hx, 
       type="l", 
       lty=2, 
       xlab="x value",
       ylab="Density", 
       main="Comparison of Student Distributions")
  for (i in 1:4){
    lines(x, 
          dt(x,df[i]), 
          lwd=2, 
          col=colors[i])
  }
  legend("topright", inset=.05, title="Distributions",
         labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
  
}

plot_student_distribution()


# -----------------------------------------------------------------------------
# Loi de Fisher
# -----------------------------------------------------------------------------

curve(df(x, df1 = 8, df2 = 100), from = 0, to = 10, 
      xlab="x value",
      ylab="Density", 
      main="Fisher Distribution")


# -----------------------------------------------------------------------------
# Loi Gamma
# -----------------------------------------------------------------------------

# Densité
curve(dgamma(x, shape = 1, rate = 1.5), 
      from=0, 
      to=1, 
      bty="n", 
      xlab="")

curve(dgamma(x, shape = 0.001, scale = 0.001), 
      from=0, 
      to=1, 
      bty="n", 
      xlab="")


# -----------------------------------------------------------------------------
# Loi Beta
# -----------------------------------------------------------------------------

curve(dbeta(x, 10, 2), 
      from=0, 
      to=1, 
      bty="n", 
      xlab="")

# La loi Beta(1, 1) correspond à la loi uniforme sur [0, 1]
curve(dbeta(x, 1, 1), 
      from=0, 
      to=1, 
      bty="n", 
      xlab="")


###############################################################################
# Lois discretes
###############################################################################

# -----------------------------------------------------------------------------
# Loi de Poisson
# -----------------------------------------------------------------------------

lambda <- 3      # Paramètre de la loi de Poisson

plot(dpois(x = 1:50, lambda = 3), pch = 3)


# -----------------------------------------------------------------------------
# Loi de Bernouilli
# -----------------------------------------------------------------------------

rbinom(10, size = 1, prob = 0.6)


# -----------------------------------------------------------------------------
# Loi de Binomiale
# -----------------------------------------------------------------------------

n <-  8
p <- 0.8
rbinom(10, size = n, prob = p)



###############################################################################
# Divers
###############################################################################


n <- 150                 # Nombre de valeur générées
n01 <- rnorm(n, 0, 1)

curve(dnorm(x), col = 2, xlim = c(-5, 5), main = "Densité d'une loi Normale")
rug(n01, col = "blue")   # Tapis des valeurs générées

# Histogramme
hist(n01, breaks = 20)

# Densité estimée à partir des valeurs générées
density(n01)
plot(density(n01), main = "Comparaison des densités", 
     xlim = c(-5, 5), ylim = c(0, 0.5), col = "blue")
curve(dnorm(x), col = "red", add = TRUE)
legend("topleft", inset=.05, lty=c(1, 1),
       c("Densité Théorique", "Densité Empirique"), col= c("red", "blue"))

# Fonction de répartition empirique
ecdf(n01)
plot(ecdf(n01))


# -----------------------------------------------------------------------------
# Générer un échantillon selon une loi de probabilité spécifique
# -----------------------------------------------------------------------------

# Simuler 20 lancers de dé
valeurs <- c(1, 2, 3, 4, 5, 6)       # Valeurs possibles
probabilites <- rep(1/6, 6)          # Probabilités d'obtenir chaque valeur
sample(x = valeurs,
       prob = probabilites,
       size = 20,
       replace = TRUE)

# Dé truqué où l'on a une proba de 0.5 d'avoir un 6
probabilites <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
sample(x = valeurs,
       prob = probabilites,
       size = 20,
       replace = TRUE)
