#**************************************************************************
#* Régression linéaire multiple                                           *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

library(ggplot2)

setwd("P:/Ludo/Tuto/R-tuto")

# -----------------------------------------------------------------------------
# Regression sur des donnees generees
# -----------------------------------------------------------------------------

# Fonction pour generer Y et X
fonction7 <- function(n) {
  X1 <- runif(n, min=0, max=1)
  X2 <- seq(1,n)
  X3 <- X1^2
  E <- rnorm(n, mean=0, sd=1)
  Y <- 15 + 3*X1 + X2/10 + X3/2 + E
  return(data.frame(X1, X2, X3, Y))
}

n <- 200    # Taille de l echantillon
p <- 3      # Nombre de regresseurs

data7 <- fonction7(n)
head(data7, 10)


# Regression de Y sur X1 X2 et X3
reg7 <- lm(Y ~ X1 + X2 + X3, data = data7)


# Summary de la regression
#   Coefficients
#     Estimate : valeur du coefficient
#     Std. Error : variance du coefficient
#     t value : Stat de Student (si tres elevee alors le coefficient est significativement different de 0)
#     Pr(>t) : p-valeur (si tres faible alors le coefficient est significativement different de 0)
#     *** : plus il y a d etoiles plus le coefficient est significativement different de 0
#   R2 : Multiple R-squared et Adjusted R-squared
#   F-statistic : Valeur de la statistique du test global de Fisher
summary(reg7)


# Calcul des valeurs ajustees du modele (Y_hat)
data7$Y_hat <- reg7$fitted.values
# ou data7$Y_hat <- predict(reg7, subset(data7, select = -c(Y)))

# Calcul des residus (Y - Y_hat)
data7$Epsilon_hat <- reg7$residuals
# ou data7$Epsilon_hat <- resid(reg7)


# Affichage des Y et Y_hat
plot(data7$Y)
points(data7$Y_hat, col="red", pch=3)


#-------------------------------------
# On refait le calcul a la main
#-------------------------------------

X <- as.matrix(data.frame(cst=rep(1,n),subset(data7, select = c(X1, X2, X3))))
Y <- as.matrix(data7$Y)

# Calcul des elements de la 2e ligne de la matrice XtX
t(X) %*% X
c(sum(data7$X1),
  sum(data7$X1^2),
  sum(data7$X1 * data7$X2),
  sum(data7$X1 * data7$X3))

Beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y 

# Verification que l on trouve bien les memes valeurs
Beta_hat
reg7$coefficients

# residus
Y_hat <- X%*%Beta_hat
Epsilon_hat <- Y - Y_hat

# La somme des Epsilon_hat_i doit etre environs egale a 0
sum(Epsilon_hat)

SCR <- as.numeric(t(Epsilon_hat) %*% Epsilon_hat)

# Comparaison avec la fonction lm
sum((reg7$residuals)^2)
SCR

# Estimation de la variance de Epsilon (equation 2.6)
sigma2_hat <- as.numeric(SCR/(n-p-1))

# Matrice des covariances de Beta_hat
cov_Beta_hat <- sigma2_hat * solve(t(X) %*% X)

# Calcul du R2
SCE <- sum((X %*% Beta_hat - mean(Y))^2)
SCT <- SCE + SCR
R2 <- SCE/SCT
R2a <- 1 - ((n-1)/(n-p-1))*(SCR/SCT)

# Comparaison des resultats
summary(reg7)["r.squared"]
R2

summary(reg7)["adj.r.squared"]
R2a


#-------------------------------------
# Intervalle de confiance 
#   pour Beta_1
#-------------------------------------

alpha <- 0.05
Beta_hat_1 <- Beta_hat[2]

var_Beta_hat_1 <- cov_Beta_hat[2,2]

# Quantile de la loi de Student a n-p-1 degres de liberte au niveau alpha 
q_student_Beta_hat_1 <- qt(alpha, df = n-p-1)

# Intervalle de confiance de Beta_hat_1 au niveau 1 - alpha
IC_Beta_hat_1 <- c(Beta_hat_1 + q_student_Beta_hat_1 * var_Beta_hat_1,
                   Beta_hat_1 - q_student_Beta_hat_1 * var_Beta_hat_1)

sort(IC_Beta_hat_1)


#-------------------------------------
# Test de significativite de Student 
#   pour le coefficicent Beta_3
#-------------------------------------

T <- Beta_hat / sqrt(diag(cov_Beta_hat))
p_valeurs <- formatC(2*(1-pt(T, n-p-1)), format = "e", digits = 2) 
data.frame(Beta_hat = Beta_hat, 
           Std = sqrt(diag(cov_Beta_hat)), 
           T=T, 
           p_valeurs=p_valeurs)

# On retrouve les colonnes t value et Pr
summary(reg7)$coefficients

# H0 : Beta_3 = 0 Vs H1 : Beta_3 non nul
# Sous H0, T_Beta_3 suit une loi de Student a n-p-1 degres de liberte
T_Beta_3 <- T[4]


#-------------------------------------
# Graphique de la fonction de densite de la loi de Student
#-------------------------------------

# Definition des borne et du nombre de points a tracer
val1 <- seq(-5, 5, length.out = 1000)

plot(x = val1, y = dt(val1,n-1),        # Calcul des valeurs de la fonction de densite
     type = "l", lwd = 3, col = "blue", # Representation par une courbe bleue
     xlim = c(-5, 5), 
     main = "Densité de la statistique de test", 
     xlab = "Réalisation", ylab = bquote(paste("Densité sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)


lines(c(-5, 5), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilatéral)
zone_rejet_gauche <- seq(-5, qt(alpha/2, n-1), 0.01)
zone_rejet_droite <- seq(qt(1-alpha/2, n-1), 5, 0.01)

# Trace du polygone de la region de rejet
polygon(x = c(zone_rejet_gauche[1], zone_rejet_gauche, zone_rejet_gauche[length(zone_rejet_gauche)]), 
        y = c(0, dt(zone_rejet_gauche, n-1), 0), 
        col = "green")
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dt(zone_rejet_droite, n-1), 0), 
        col = "green")
legend("topright", inset=.05, 
       c("Zones de rejet"), 
       fill=c("green"))

# Affichage de T_Beta_3 observe
lines(c(T_Beta_3,T_Beta_3),c(0,dt(T_Beta_3,n-1)),lwd = 3, col="red")
legend("topleft", inset=.05, lty=c(1, 1), 
       c("T_Beta_3"), text.col = "red", 
       col=c("red"))

# p-value de Beta_3
if (T_Beta_3 < 0){
  p_value_T_Beta_3 <- 2*pt(T_Beta_3, df = n-p-1)
}else{
  p_value_T_Beta_3 <- 2*(1-pt(T_Beta_3, df = n-p-1))
}

# on retrouve celle donnee par le modele
p_value_T_Beta_3
summary(reg7)$coefficients[4,4]


#-------------------------------------
# Test global de Fisher
#-------------------------------------

# Pour tester si le modele explique quelque chose
# H0 : tous les Beta_j sont nuls 
# Sous H0, F suit une loi de Fisher a p et n-p-1 degres de liberte

# Stat de test
F <- R2 / (1 - R2) * (n - p - 1 ) / p
F

# A comparer avec le quantile de la loi de Fisher
qf(0.95, p, n-p-1)


# -----------------------------------------------------------------------------
# Validation du modele
# -----------------------------------------------------------------------------

#-------------------------------------
# Criteres d information AIC et BIC
#-------------------------------------

# Log vraisemblance du modele
- 1/2 * (n * log (SCR / n) + n * (1 + log(2 * pi)))
logLik(reg7)

# On genere differents modeles pour les comparer
reg7_X2 <- lm(Y ~ X2, data = data7)
reg7_X1_X3 <- lm(Y ~ X1 + X3, data = data7)
reg7_sans_constante <- lm(Y ~ 0 + X1 + X2 + X3, data = data7)

# On choisit le modele qui a le plus petit AIC ou BIC
AIC(reg7, reg7_X2, reg7_X1_X3, reg7_sans_constante)
BIC(reg7, reg7_X2, reg7_X1_X3, reg7_sans_constante)

# Calcul a la main
n * log (SCR / n) + n * (1 + log(2 * pi)) + 2 * (p + 1)
AIC(reg7)


#-------------------------------------
# Correlation entre les regresseurs
#-------------------------------------

# Un VIF (Variance Inflation Factor) superieur a 10 est considere comme problematique
library(car)
vif(reg7)

# Correlation entre les differents regresseurs
cor(X[-1,-1])

# X3 en fonction de X1
plot(data7$X1, data7$X3, main = "X3 = f(X1)")


#-------------------------------------
# Analyse des Residus
#-------------------------------------

# hatvalue correspond aux coefficients diagonaux hii de la matrice de projection sur Im(X)
hii <- diag(X %*% solve(t(X) %*% X) %*% t(X))
head(data.frame(hii, hatvalues(reg7)), 10)

hii <- hatvalues(reg7)

# Residus standardises
ti <- Epsilon_hat / (sigma2_hat * sqrt(1 - hii))
head(data.frame(ti, rstandard(reg7)), 10)              # legere difference dans le calcul a voir pourquoi

ti <- rstandard(reg7)

# Loi du residu standardise (Theorme 4.4.1)
loi_res_std <- ti^2 / (n-p-1)

# Suit une loi Beta(1/2, (n - p - 2) / 2)
loi_beta <- rbeta(n, 1/2, (n - p - 2) / 2)
plot(loi_beta[order(loi_beta)], type='l', main = "Loi Beta des résidus normalisés")
points(loi_res_std[order(loi_res_std)], pch=3, col="red")

# Le probleme des residus standardises est que le numerateur et le denominateur ne sont pas independants
# On calcule donc des residus studentises
#   de facon a ce que le ieme residu n intervient pas dans l estimation de la variance
#   autrement dit on genere le modele sans le ieme terme
#   puis on fait une prediction sur ce ieme terme et on calcule le residu (studentise)

# Une autre methode pour calculer les residus studentises plutot que de generer n modeles differents
ti_star <- ti * sqrt((n - p - 2) / (n - p - 1 - ti^2))
head(data.frame(ti_star, rstudent(reg7)), 10)

ti_star <- rstudent(reg7)

# Suit une loi de Student((n-p-2)
loi_student <- rt(n, n - p - 2)
plot(loi_student[order(loi_student)], type='l', main = "Loi de Student des résidus studentisés")
points(ti_star[order(ti_star)], pch=3, col="red")


#-------------------------------------
# Point aberrants
#   tels que les residus studentises sont superieurs au quantile de Student
#   mecaniquement si l on choisit une marge de 0.05, alors environs 5 points sur 100 seront consideres comme aberrants
#-------------------------------------

# Residus studentises
rs7 <- rstudent(reg7)
head(rs7,20)

# On liste les points pour lesquels le residu studentise est superieur au quantile de la loi de Student
rs7[abs(rs7) > qt(0.975, n-p-1)]

# on trace les quantiles sur le graph des rs
plot(rs7, main = "Résidus Studentisés")
abline(h=c(qt(0.975, n-p-1), qt(0.025, n-p-1)), col="orange")


#-------------------------------------
# Points leviers
#   tels que est superieurs a 2p/n (seuil Hoaglin et Welsch)
#-------------------------------------

# hatvalue correspond aux coefficients diagonaux hii de la matrice de projection sur Im(X)
hatvalues(reg7)
hatvalues(reg7) > 2*p/n
hatvalues(reg7)[hatvalues(reg7) > 2*p/n]

# On trace les differents seuils
plot(hatvalues(reg7), main = "Points Leviers")
abline(h = 2*p/n, col="orange")
abline(h = 3*p/n, col="red")
abline(h = 0.5,   col="blue")
legend("top", inset=.05, lty=c(1, 1), 
       title = "Seuils",
       c("Hoaglin-Welsch : 2p/n", "Velleman-Welsch : 3p/n", "Huber : 0.5"), 
       col=c("orange", "red", "blue"))


#-------------------------------------
# Points influents
#   Superieurs au quantile de Fisher
#-------------------------------------

cooks.distance(reg7)
cooks.distance(reg7) > qf(0.95, p+1, n-p-1)
cooks.distance(reg7)[cooks.distance(reg7) > qf(0.95, p+1, n-p-1)]


plot(cooks.distance(reg7), main = "Distance de Cook")

# Si on veut visualiser le seuil
plot(cooks.distance(reg7), ylim = c(0, 3), main = "Distance de Cook")
abline(h = qf(0.95, p+1, n-p-1), col="orange")


#-------------------------------------
# Creation d un point levier et influent
#-------------------------------------

# On diminue n pour que notre point ait plus d influence
n_bis <- 20
data7_bis <- data7[1:n_bis,]

new <- c(0, 100, 0, 35, 0, 0)
data7_bis <- rbind(data7_bis, new)

reg7_bis <- lm(Y ~ X1 + X2 + X3, data = data7_bis)
rs7_bis <- rstudent(reg7_bis)

ggplot(data = data7_bis, 
       aes(X2, Y)) +
  geom_point() + 
  ylim(0, 60) +
  geom_point(aes(X2, Y), 
             data = data7_bis[n_bis+1,], 
             color = "red") +
  geom_smooth(data = data7_bis,
              method = "lm",
              formula = y ~ x) +
  geom_smooth(data = data7_bis[1:n_bis,],     # droite de regression sans le point influent
              method = "lm",
              formula = y ~ x, 
              color = 'green', 
              se = 0) +
  ggtitle("Regression avec et sans le point influent")

# Est il vraiment influent
cooks.distance(reg7_bis)[n_bis+1] > qf(0.95, p+1, n_bis-p-1)
plot(cooks.distance(reg7_bis), main = "Points influents")
abline(h = qf(0.95, p+1, n-p-1), col="orange")

# Est il vraiment levier
hatvalues(reg7_bis)[n_bis+1] > 2*p/n_bis
plot(hatvalues(reg7_bis), main = "Points levier")
abline(h = 2*p/n_bis, col="orange")

# Est il aberrant
abs(rs7_bis[n_bis+1]) > qt(0.975, n_bis-p-1)
plot(rs7_bis, main = "Points aberrants")
abline(h=qt(0.975, n_bis-p-1), col="orange")
abline(h=qt(0.025, n_bis-p-1), col="orange")


#-------------------------------------
# Creation d un point aberrant et influent
#-------------------------------------







#-------------------------------------
# Diagramme Quantile-Quantile ou QQ-plot
#   Analyse de la normalite des residus
#-------------------------------------

# QQ-plot
#   x : quantile de la loi normale centree reduite
#   y : Residus ordonnes

qqnorm(data7$Epsilon_hat[order(data7$Epsilon_hat)], main="QQ plot")
qqline(data7$Epsilon_hat[order(data7$Epsilon_hat)], col = "red")

# Autre possibilite 
plot(x = qnorm(seq(1,n)/(n+1)), 
     y = data7$Epsilon_hat[order(data7$Epsilon_hat)], 
     main="QQ plot 2")


#-------------------------------------
# Analyse de l homoscedasticite
#   Verifier que cov(Y_hat, Epsilon_hat) = 0  
#-------------------------------------

# ok si ce nuage de points n a pas de forme particuliere
plot(rs7, data7$Y_hat, main = "Analyse de l homoscedasticite")


#-------------------------------------
# Non linearite - exemple
#-------------------------------------

fonction8 <- function(n) {
  X1 <- runif(n, min=0, max=1)
  X2 <- seq(1, n)
  X3 <- X1^2
  E <- rnorm(n, mean=0, sd=1)
  Y <- 15 + 3*X1 + X2^2/(3*n) + X3/2  + E
  return(data.frame(X1, X2, X3, E, Y))
}

data8 <- fonction8(n)

reg8 <- lm(Y ~ X1 + X2 + X3, data8)
summary(reg8)

plot(data8$Y)

# Forme de croissant
plot(rstudent(reg8), reg8$fitted.values, main = "Non linearite - exemple")


#-------------------------------------
# Heteroscedasticite - exemple
#-------------------------------------

fonction9 <- function(n) {
  X1 <- runif(n, min=0, max=1)
  X2 <- seq(1, n)
  X3 <- X1^2
  E <- rnorm(n, mean=0, sd=seq(1/n,20, by = 20/n))
  Y <- 15 + 3*X1 + X2/10 + X3/2  + E
  return(data.frame(X1, X2, X3, E, Y))
}

data9 <- fonction9(n)

reg9 <- lm(Y ~ X1 + X2 + X3, data9)
summary(reg9)

plot(data9$Y)

# Forme de trompette
plot(rstudent(reg9), reg9$fitted.values, main = "Heteroscedasticite - exemple")

