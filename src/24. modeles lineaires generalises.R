#**************************************************************************
#* Modèles linéaires généralisés                                          *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

library(ggplot2)

setwd("P:/Ludo/Tuto/R-tuto")

# -----------------------------------------------------------------------------
# Fonctions utiles
# -----------------------------------------------------------------------------

logit <- function(x){
  log(x/(1-x))
}

invlogit <- function(x){
  exp(x)/(1+exp(x))
}


# -----------------------------------------------------------------------------
# Import du jeu de donnees beetle
# -----------------------------------------------------------------------------

# install.packages("dobson")
library(dobson)
data(beetle)
str(beetle)


beetle$Pi <- beetle$y / beetle$n
beetle

# x : dose
# n : nombre d individu dans le groupe i
# y : nombre de morts dans le groupe i
# Pi : Probabilite de mourir dans la groupe i


# -----------------------------------------------------------------------------
# Modelisation logistique
# -----------------------------------------------------------------------------

logit_model <- glm(beetle$Pi ~ beetle$x, family=binomial(link="logit"))
summary(logit_model)

# Valeurs estimees (predites) par le modele
beetle$Pi_hat <- predict(logit_model, newdata = list(beetle$x), type = "response")
beetle

# Intervalle de confiance de Wald pour les parametres Beta_0 et Beta_1
# TODO poly p23



###############################################
# TODO fix
# https://rug.mnhn.fr/semin-r/PDF/semin-R_glm_SBallesteros_100608.pdf
###############################################

# Log Vraisemblance des differents modeles
logV_sature <- sum(log(dbinom(beetle$y, beetle$n, beetle$Pi))) 
logV_nul    <- sum(log(dbinom(beetle$y, beetle$n, mean(beetle$Pi))))
logV_logit  <- sum(log(dbinom(beetle$y, beetle$n, beetle$Pi_hat))) 

# Deviance_nulle
-2 * (logV_nul - logV_sature)
logit_model$null.deviance

# Deviance residuelle du modele
-2 * (logV_logit - logV_sature)
logit_model$deviance



# -----------------------------------------------------------------------------
# Calcul des valeurs estimees a la main
# -----------------------------------------------------------------------------

Beta_0 <- logit_model$coefficients[1]
Beta_1 <- logit_model$coefficients[2]
Pi_hat_manuel <- invlogit(Beta_0 + beetle$x * Beta_1)

# Verification
data.frame(beetle$Pi_hat, Pi_hat_manuel, egal = (round(beetle$Pi_hat - Pi_hat_manuel, 0.001) == 0))


# Affichage
plot(beetle$x, beetle$Pi, pch = 3, col = "blue", xlab = "Dose", ylab = "Taux de mortalité", 
     ylim = c(0,1), main = "Taux de mortalité des scarabées en fonction de la dose")
lines(beetle$x, beetle$Pi_hat, type = "l", col = "red")
legend("topleft", inset=.05, lty = c(1,0), pch = c(-1,3), c("Modèle Logit", "Vraies valeurs"), col=c("red","blue"))

# Dose letale a 50 pourcent
LD50 <- - Beta_0 / Beta_1
LD50

# Probabilite de deces sans poison
invlogit(Beta_0)

# Matrice de covariances asymptotiques de l estimateur
vcov(logit_model)


# -----------------------------------------------------------------------------
# Intervalle de confiance pour la prediction
#   Attention a bien faire les choses dans le bon ordre
#     On calcule les IC puis on applique la fonction de lien inverse
#   Sinon on risque de se retrouver avec des IC contenant des valeurs negatives ou superieures a 1
# -----------------------------------------------------------------------------

alpha <- 0.05

# On demande a predict de renvoyer les valeurs non inversees et se.fit (Standard Error)
preds <- predict(logit_model, newdata = list(beetle$x), type = "link", se.fit = TRUE)

IC_upr <- preds$fit + (qnorm(1-alpha/2) * preds$se.fit)
IC_lwr <- preds$fit - (qnorm(1-alpha/2) * preds$se.fit)

# On applique la fonction de lien inverse
beetle$IC_inf <- logit_model$family$linkinv(IC_lwr)
beetle$IC_sup <- logit_model$family$linkinv(IC_upr)

beetle

# Visualisation des intervalles de confiance
ggplot(data=beetle, mapping=aes(x=x,y=Pi)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=beetle, mapping=aes(x=x, y=IC_inf), col="red") + 
  geom_line(data=beetle, mapping=aes(x=x, y=IC_sup), col="red") 


# -----------------------------------------------------------------------------
# Odds ratio
# -----------------------------------------------------------------------------


