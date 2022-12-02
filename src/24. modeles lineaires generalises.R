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


logit_model <- glm(Pi ~ x, data = beetle, family=binomial(link="logit"))
summary(logit_model)

# Valeurs estimees par le modele
beetle$Pi_hat <- fitted(logit_model)
beetle


n <- nrow(beetle)                            # Nombre d'observations
d <- length(logit_model$coefficients)        # Nombre de regresseurs (constante incluse)


# -----------------------------------------------------------------------------
# Calcul des valeurs estimees a la main
# -----------------------------------------------------------------------------

Beta_0 <- logit_model$coefficients[1]
Beta_1 <- logit_model$coefficients[2]
Pi_hat_manuel <- invlogit(Beta_0 + beetle$x * Beta_1)

# Verification
data.frame(beetle$Pi_hat, Pi_hat_manuel, egal = (round(beetle$Pi_hat - Pi_hat_manuel, 0.001) == 0))


# -----------------------------------------------------------------------------
# Intervalle de confiance des coefficients du modèle
# -----------------------------------------------------------------------------

# Intervalle de confiance de Wald de Beta_1
Beta_1 <- logit_model$coefficients["x"]
std_Beta_1 <- sqrt(diag(vcov(logit_model)))["x"]
IC_Beta_1 <- c(inf = Beta_1 - qnorm(0.975) * std_Beta_1, sup = Beta_1 + qnorm(0.975) * std_Beta_1)

# Ici 0 appartient à l'intervalle de confiance, donc on ne peut pas rejeter H0 : beta_1 = 0
IC_Beta_1


# -----------------------------------------------------------------------------
# Modèles nul et Modèle saturé
#   le modèle nul n'a qu'un seul paramètre, il prédit la moyenne de la variable à expliquer
#   le modèle saturé a autant de paramètres qu'il y a d'observation
#     il prédit parfaitement chaque observation
#     attention le modèle saturé n'est pas un bon modèle, car il colle trop aux données d'apprentissage
# -----------------------------------------------------------------------------

model_sat <- glm(Pi ~ as.factor(1:length(Pi)), data = beetle, family=binomial)
summary(model_sat)

model_nul <- glm(Pi ~ 1, data = beetle, family=binomial)
summary(model_nul)

# Le coefficient de Beta_0 est le logit de la moyenne des Pi
logit(mean(beetle$Pi))


# -----------------------------------------------------------------------------
# Affichage
# -----------------------------------------------------------------------------

plot(beetle$x, beetle$Pi, pch = 3, col = "blue", xlab = "Dose", ylab = "Taux de mortalité", 
     ylim = c(0,1), main = "Taux de mortalité des scarabées en fonction de la dose")
lines(beetle$x, beetle$Pi_hat, type = "l", col = "red", lwd=2)

lines(beetle$x, fitted(model_sat), type = "l", col = "green")
lines(beetle$x, fitted(model_nul), type = "l", col = "orange")

legend("topleft", inset=.05, lty = c(0,1,1,1), pch = c(3,NA,NA,NA), 
       c("Valeurs observées", "Modèle Logit", "Modèle saturé", "Modèle nul"), 
       col=c("blue", "red", "green", "orange"))


# -----------------------------------------------------------------------------
# Validation du modèle
# -----------------------------------------------------------------------------

summary(logit_model)

# Null deviance
logit_model$null.deviance

# Déviance résiduelle
logit_model$deviance
deviance(logit_model)

# H0 : le modèle n'est pas significatif (Beta_1 = 0)
# Sous H0, la différence des déviances suit une loi du chi2 à d-1 degrés de liberté
# Est-ce que l'on rejette H0 ?
logit_model$null.deviance - logit_model$deviance > qchisq(0.95, df = d - 1)




############################################
# TODO calcul à la main
# https://rpubs.com/fhernanb/deviance_glm
############################################



logLik(logit_model)
sum(beetle$Pi * log(beetle$Pi_hat) + (1 - beetle$Pi) * log((1 - beetle$Pi_hat)))

logLik(model_nul)
sum(beetle$Pi * log(logit(mean(beetle$Pi))) + (1 - beetle$Pi) * log(1 - logit(mean(beetle$Pi))))

2 * (logLik(model_sat) - logLik(model_nul))
2 * (logLik(model_sat) - logLik(logit_model))


# -----------------------------------------------------------------------------
# Dose létale a 50 pourcent
# -----------------------------------------------------------------------------

LD50 <- - Beta_0 / Beta_1
LD50

# Probabilité de décés sans poison
invlogit(Beta_0)

# Matrice de covariances asymptotiques de l'estimateur
vcov(logit_model)


# -----------------------------------------------------------------------------
# Intervalle de confiance pour la prédiction
#   Attention a bien faire les choses dans le bon ordre
#     On calcule les IC puis on applique la fonction de lien inverse
#   Sinon on risque de se retrouver avec des IC contenant des valeurs négatives ou supérieures a 1
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


