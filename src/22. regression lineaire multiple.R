rm(list=ls())

setwd("P:/Ludo/Cours2A/UE1 Regression Linéaire/R")

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
summary(reg7)


# Calcul des predictions du modele
data7$Y_hat <- round(predict(reg7, subset(data7, select = -c(Y))))

# Calcul des residus
data7$Epsilon_hat <- data7$Y - data7$Y_hat

# Affichage des Y et Y_hat
plot(data7$Y)
points(data7$Y_hat, col="red", pch=3)


# -------------------------------
# On refait le calcul a la main
# -------------------------------
X <- as.matrix(data.frame(cst=rep(1,n),subset(data7, select = c(X1, X2, X3))))
Y <- as.matrix(data7$Y)

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

# Estimation de la variance de Epsilon (equation 2.6)
sigma2_hat <- as.numeric(SCR/(n-p-1))

# Matrice des covariances de Beta_hat
cov_Beta_hat <- sigma2_hat * solve(t(X) %*% X)

# Calcul du R2
SCE <- sum((X %*% Beta_hat - mean(Y))^2)
SCT <- SCE + SCR
R2 <- SCE/SCT
R2a <- 1 - ((n-1)/(n-p-1))*(SCR/SCT)


# -----------------------------------------------------------------------------
# Intervalle de confiance pour Beta_1
# -----------------------------------------------------------------------------

alpha <- 0.05
Beta_hat_1 <- Beta_hat[2]

var_Beta_hat_1 <- cov_Beta_hat[2,2]

# Quantile de la loi de Student a n-p-1 degres de liberte au niveau alpha 
q_student_Beta_hat_1 <- qt(alpha, df = n-p-1)

# Intervalle de confiance de Beta_hat_1 au niveau 1 - alpha
IC_Beta_hat_1 <- c(Beta_hat_1 + q_student_Beta_hat_1 * var_Beta_hat_1,
                   Beta_hat_1 - q_student_Beta_hat_1 * var_Beta_hat_1)

sort(IC_Beta_hat_1)


# -----------------------------------------------------------------------------
# Test de significativite de Student de Beta_3
# -----------------------------------------------------------------------------

T <- Beta_hat / sqrt(diag(cov_Beta_hat))
T

# On retrouve la colonne t value
summary(reg7)$coefficients

# H0 : Beta_3 = 0 Vs H1 : Beta_3 non nul
# Sous H0, T_Beta_3 suit une loi de Student a n-p-1 degres de liberte
T_Beta_3 <- T[4]


# -------------------------------
# Graphique de la fonction de densite de la loi de Student
# -------------------------------

# Definition des borne et du nombre de points a tracer
val1 <- seq(-5, 5, length.out = 1000)

plot(x = val1, y = dt(val1,n-1),        # Calcul des valeurs de la fonction de densite
     type = "l", lwd = 3, col = "blue", # Representation par une courbe bleue
     xlim = c(-5, 5), 
     main = "Densité de la statistique de test; région de rejet", 
     xlab = "Réalisation", ylab = bquote(paste("Densité sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)

lines(c(-5, 5), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilatéral)
zone_rejet_gauche <- seq(-5, qt(alpha/2, n-1), 0.01)
zone_rejet_droite <- seq(qt(1-alpha/2, n-1), 5, 0.01)

# Tracé du polygone de la région de rejet
polygon(x = c(zone_rejet_gauche[1], zone_rejet_gauche, zone_rejet_gauche[length(zone_rejet_gauche)]), 
        y = c(0, dt(zone_rejet_gauche, n-1), 0), 
        col = "green")
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dt(zone_rejet_droite, n-1), 0), 
        col = "green")

# Affichage de T_Beta_3 observe
lines(c(T_Beta_3,T_Beta_3),c(0,dt(T_Beta_3,n-1)),lwd = 3, col="red")
text(-2, 0.3, "T_Beta_3", cex=0.8, pos=4, col="red")

# p-value de Beta_3
if (T_Beta_3 < 0){
  p_value_T_Beta_3 <- 2*pt(T_Beta_3, df = n-p-1)
}else{
  p_value_T_Beta_3 <- 2*(1-pt(T_Beta_3, df = n-p-1))
}

# on retrouve celle donnees par le modele
p_value_T_Beta_3
summary(reg7)$coefficients[4,4]



# -----------------------------------------------------------------------------
# Validation du modele
# -----------------------------------------------------------------------------

#-------------------
# Point aberrants
#   tels que les residus studentises sont superieurs au quantile de Student
#-------------------

rs7 <- rstudent(reg7)
head(rs7,20)
plot(rs7, main = "Résidus Studentisés")

# On liste les points pour lesquels le residu de student est superieur au quantile de la loi de Student
rs7[abs(rs7) > qt(0.975, n-p-1)]

# on trace les quantiles sur le graph des rs
abline(h=qt(0.975, n-p-1), col="orange")
abline(h=qt(0.025, n-p-1), col="orange")


#-------------------
# Points leviers
#   tels que est superieurs a 2p/n (seuil Hoaglin et Welsch)
#-------------------

# Matrice de projection sur Im(X)
hatvalues(reg7)
hatvalues(reg7) > 2*p/n
hatvalues(reg7)[hatvalues(reg7) > 2*p/n]

# hatvalue correspond aux coefficients diagonaux de la matrice de projection sur Im(X)
hii <- diag(X %*% solve(t(X) %*% X) %*% t(X))
head(data.frame(hii, hatvalues(reg7)), 10)


#-------------------
# Points influents
#   Superieurs au quantile de Fisher
#-------------------

cooks.distance(reg7)
cooks.distance(reg7) > qf(0.95, p+1, n-p-1)
cooks.distance(reg7)[cooks.distance(reg7) > qf(0.95, p+1, n-p-1)]


# -----------------------------------------------------------------------------
# Creation d un point levier et influent
# -----------------------------------------------------------------------------

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
              method = "lm") +
  geom_smooth(data = data7_bis[1:n_bis,],     # droite de regression sans le point influent
              method = "lm", 
              color = 'green', 
              se = 0) 

# Est il vraiment influent
cooks.distance(reg7_bis)[n_bis+1] > qf(0.95, p+1, n_bis-p-1)

# Est il vraiment levier
hatvalues(reg7_bis)[n_bis+1] > 2*p/n_bis

# Est il aberrant
abs(rs7_bis[n_bis+1]) > qt(0.975, n_bis-p-1)


# -----------------------------------------------------------------------------
# Creation d un point aberrant et influent
# -----------------------------------------------------------------------------
