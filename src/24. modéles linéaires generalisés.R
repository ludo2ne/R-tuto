#**************************************************************************
#* Modèles linéaires généralisés                                          *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

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


###############################################################################
# Cas où Y binaire : Jeu de données toy
###############################################################################

X <- c(0.088, 0.99, 0.54, 0.062, 0.14, 0.51, 0.81, 0.39, 0.56, 0.90, 0.99, 0.63, 
       0.95, 0.0057, 0.055, 0.30, 0.91, 0.28, 0.16, 0.53)
Y <- c(0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1)
toy <- data.frame(X, Y)
rm(X, Y)

n <- nrow(toy)        # nombre d'observations
d <- 2                # nombre de regresseurs (constante incluse)

# On ordonne les données pour faciliter les tracés
toy <- toy[order(toy$X),]
rownames(toy) <- seq(1,n)


# Visualisation des données
plot(toy$X, toy$Y, pch=3, col="blue", main = "Données toy")

# Tentative de régression linéaire... ça ne marche pas bien
abline(lm(toy$Y ~ toy$X), col="red")

# Modèle de regression logistique (fonction de lien : logit par défaut)
logit_model <- glm(formula = Y ~ X, data = toy, family = binomial)
summary(logit_model)

# Estimations de Y par le modèle
toy$Y_hat <- fitted(logit_model)

# Calcul à la main (logit(Y) = alpha + beta * X)
alpha_hat <- logit_model$coefficients[1]
beta_hat <- logit_model$coefficients[2]
Y_hat <- invlogit(alpha_hat + beta_hat * toy$X)
data.frame(toy$Y_hat, a_la_main = Y_hat)


# ----------------------------------------------------------------------
# Intervalle de confiance des coefficients du modèle
# ----------------------------------------------------------------------

# Intervalle de confiance de Wald de Beta_1
std_beta_hat <- sqrt(diag(vcov(logit_model)))["X"]
IC_beta <- c(inf = beta_hat - qnorm(0.975) * std_beta_hat, 
             sup = beta_hat + qnorm(0.975) * std_beta_hat)

# Ici 0 n'appartient pas à l'intervalle de confiance, donc on peut rejeter H0 : beta = 0
IC_beta

# On voit également que la p-valeur de beta est inférieure à 0.05
# Ce qui confirme qu'au seuil de 5% on peut rejeter H0
summary(logit_model)


# ----------------------------------------------------------------------
# Modèle nul et Modèle saturé
# ----------------------------------------------------------------------

# le modèle saturé a autant de paramètres qu'il y a d'observation
#   il prédit parfaitement chaque observation
#   attention le modèle saturé n'est pas un bon modèle, car il colle trop aux données d'apprentissage
model_sat <- glm(Y ~ as.factor(1:length(Y)), data = toy, family=binomial)
summary(model_sat)


# le modèle nul n'a qu'un seul paramètre, il prédit la moyenne de la variable à expliquer
model_nul <- glm(Y ~ 1, data = toy, family=binomial)
summary(model_nul)

# Le coefficient de Beta_0 est le logit de la moyenne des Pi
model_nul$coefficients
logit(mean(toy$Y))


# ----------------------------------------------------------------------
# Visualisation
# ----------------------------------------------------------------------

plot(toy$X, toy$Y, pch = 3, col = "blue", main = "Données toy")

lines(toy$X, toy$Y_hat, type = "l", col = "red", lwd=2)
lines(toy$X, fitted(model_sat), type = "l", col = "green")
lines(toy$X, fitted(model_nul), type = "l", col = "orange")

legend("bottomright", inset = .05, lty = c(0, 1, 1, 1), pch = c(3, NA, NA, NA), 
       col = c("blue", "red", "green", "orange"), 
       legend = c("Valeurs observées", "Modèle Logit", "Modèle saturé", "Modèle nul"))


# ----------------------------------------------------------------------
# Autres modèles de la famille Binomiale (probit et loglog)
# ----------------------------------------------------------------------

probit_model <- glm(Y ~ X, family = binomial(link = "probit"), data = toy)
loglog_model <- glm(Y ~ X, family = binomial(link = "cloglog"), data = toy)

# Ajout au graphiques
lines(toy$X, fitted(probit_model), col="blue", lty=2)
lines(toy$X, fitted(loglog_model), col="darkorchid1", lty=3)

legend("bottomright", inset = .05, lty = c(0, 1, 1, 1, 3, 3), pch = c(3, NA, NA, NA, NA, NA), 
       col = c("blue", "red", "green", "orange", "blue", "darkorchid1"), 
       legend = c("Valeurs observées", "Modèle Logit", "Modèle saturé", "Modèle nul", "Modèle probit", "Modèle loglog"))


# ----------------------------------------------------------------------
# Validation du modèle logit - Calculs de Déviances
# ----------------------------------------------------------------------

# Null deviance
logit_model$null.deviance

# Déviance résiduelle
logit_model$deviance             # ou deviance(logit_model)

# Formule de la déviance résiduelle
2 * (logLik(model_sat) - logLik(logit_model))


# Formule de la log-vraisemblance
logLik(logit_model)
sum(toy$Y * log(invlogit(alpha_hat + beta_hat * toy$X)) + 
    (1 - toy$Y) * log(1 - invlogit(alpha_hat + beta_hat * toy$X)))

# log-vraisemblance du modèle nul
logLik(model_nul)
sum(toy$Y * log(model_nul$coefficients[1]) + (1 - toy$Y) * log(1 - model_nul$coefficients[1]))


# H0 : le modèle n'est pas significatif (Beta_1 = 0)
# Sous H0, la différence des déviances suit une loi du chi2 à d-1 degrés de liberté
# Est-ce que l'on rejette H0 ?
logit_model$null.deviance - logit_model$deviance > qchisq(0.95, df = d - 1)


# ----------------------------------------------------------------------
# Visualisation du test
# ----------------------------------------------------------------------

# Definition des borne et du nombre de points a tracer
val1 <- seq(0, 10, length.out = 1000)
alpha <- 0.05


plot(x = val1, y = dchisq(val1, d-1),      # Calcul des valeurs de la fonction de densite
     type = "l", lwd = 2, col = "blue",  # Representation par une courbe bleue
     xlim = c(0, 10), ylim = c(0, 1),
     main = "Densité de la statistique de test", 
     xlab = "Réalisation", ylab = bquote(paste("Densité sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)

lines(c(0, 10), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilatéral)
zone_rejet_droite <- seq(qchisq(1-alpha, d-1), 10, 0.01)

# Trace du polygone de la region de rejet
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dchisq(zone_rejet_droite, d-1), 0), 
        col = "green")

x <- logit_model$null.deviance - logit_model$deviance
lines(c(x, x),c(0,1),lwd = 3, col="red")
legend("top", inset=.05,  
       c("Différence des déviances", "Zone de rejet"), 
       text.col = c("red", "green"))

rm(x, zone_rejet_droite, val1)


# ----------------------------------------------------------------------
# Qualité du modèle(AIC, ROC)
# ----------------------------------------------------------------------

# l'AIC est un compromis entre avoir une bonne vraisemblance mais pas trop de paramètres
# Plus l'AIC est petit, meilleur est le modèle
data.frame(Modèle = c("logit", "probit", "loglog", "nul", "saturé"),
           AIC = c(logit_model$aic, probit_model$aic, loglog_model$aic, model_nul$aic, model_sat$aic))

# Formule de l'AIC
2 * d - 2 * logLik(logit_model)
logit_model$aic


library("ROCR")
# install.packages("ROCR")

# Courbes ROC
plot(performance(prediction(fitted(logit_model), toy$Y), "tpr", "fpr"), col="blue", 
     main = "ROC des différents modèles")
plot(performance(prediction(fitted(model_nul), toy$Y), "tpr", "fpr"), add=TRUE, col="red")
plot(performance(prediction(fitted(model_sat), toy$Y), "tpr", "fpr"), add=TRUE, col="green")

legend("bottomright", inset = .05, lty = c(1, 1, 1), 
       col = c("blue", "green", "red"), 
       legend = c("Modèle Logit", "Modèle saturé", "Modèle nul"))




###############################################################################
# Données groupées : beetle
###############################################################################

rm(list = setdiff(ls(), lsf.str()))

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
# Validation du modèle
# -----------------------------------------------------------------------------

# H0 : le modèle n'est pas significatif (Beta_1 = 0)
# Sous H0, la différence des déviances suit une loi du chi2 à d-1 degrés de liberté
# Est-ce que l'on rejette H0 ?
logit_model$null.deviance - logit_model$deviance > qchisq(0.95, df = d - 1)


# -----------------------------------------------------------------------------
# Dose létale a 50 pourcent
# -----------------------------------------------------------------------------

Beta_0 <- coefficients(logit_model)[1]
Beta_1 <- coefficients(logit_model)[2]

LD50 <- -Beta_0 / Beta_1
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

beetle
beetle$x
list(beetle$x)

# On demande a predict de renvoyer les valeurs non inversées et se.fit (Standard Error)
preds <- predict(logit_model, newdata = list(x=beetle$x), type = "link", se.fit = TRUE)

IC_upr <- preds$fit + (qnorm(1-alpha/2) * preds$se.fit)
IC_lwr <- preds$fit - (qnorm(1-alpha/2) * preds$se.fit)

# On applique la fonction de lien inverse
beetle$IC_inf <- logit_model$family$linkinv(IC_lwr)
beetle$IC_sup <- logit_model$family$linkinv(IC_upr)

beetle

# Visualisation des intervalles de confiance
library(ggplot2)
ggplot(data=beetle, mapping=aes(x=x,y=Pi)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=beetle, mapping=aes(x=x, y=IC_inf), col="red") + 
  geom_line(data=beetle, mapping=aes(x=x, y=IC_sup), col="red") 





###############################################################################
# Y catégorielle avec notion d'ordre
###############################################################################

rm(list = setdiff(ls(), lsf.str()))

# install.packages("VGAM")
library(VGAM)

mental <- read.csv(file = "data/Mental.txt", sep="")
mental$ses <- as.factor(mental$ses)

mental
summary(mental)
str(mental)

# Variables
#   impair : décrivant l'état mental de la personne concernée, de 1 (sain) à 4 (en mauvaise santé)
#   ses    : qui vaut 1 si la personne a un statut socio-économique élevé, 0 sinon
#   life   : mesurant le nombre et l'intensité des bouleversements qu'a connus la personne au cours 
#            des trois dernières années, de 0 (aucun changement) à 9 (changements très importants)


n <- nrow(mental)   # Nombre d'observations
d <- 3              # Nombre de prédicteurs


# --------------------------------------------------------------------------
# Modèle Cumulatif proportionnel
# --------------------------------------------------------------------------

model_cum_prop <- vglm(formula = impair ~ life + ses, 
                       data = mental, 
                       family = cumulative(parallel = TRUE))

summary(model_cum_prop)
coefficients(model_cum_prop, matrix=TRUE)

# Odds ratio associé à la variable ses
#   toutes choses égales par ailleurs, avoir un ses de 1 permet d'avoir 
#   3 fois moins de chances d'être atteint mentalement qu'un ses de 0
exp(model_cum_prop@coefficients["ses1"])

# Intervalle de confiance de Wald pour cet Odds Ratio
exp(confint(model_cum_prop, 
            parm = "ses1", 
            level = 0.95))

# Calcul à la main de l'IC de Wald
Beta_1_hat <- coefficients(model_cum_prop)["ses1"]
std_Beta_1_hat <- sqrt(vcov(model_cum_prop)["ses1","ses1"])
IC_OR_min <- exp(Beta_1_hat - qnorm(0.975) * std_Beta_1_hat)
IC_OR_max <- exp(Beta_1_hat + qnorm(0.975) * std_Beta_1_hat)
c(IC_OR_min , IC_OR_max)

# --------------------------------------------------------------------------
# Modèle Cumulatif proportionnel avec terme d'interaction
# --------------------------------------------------------------------------

model_cum_prop_avec_ti <- vglm(formula = impair ~  life + ses + life:ses, 
                                data = mental, 
                                family = cumulative(parallel = TRUE))

summary(model_cum_prop_avec_ti)

# Différence des déviances
deviance_diff <- deviance(model_cum_prop) - deviance(model_cum_prop_avec_ti)
deviance_diff

# Différence de degrés de liberté
df_diff <- model_cum_prop@df.residual - model_cum_prop_avec_ti@df.residual
df_diff


# La différence des Déviances suit une loi du chi2 avec df = différence de degrés de liberté
# entre les 2 modèles
# On rejette H0 : Beta_3 = 0 ? 
#   où Beta_3 : coefficient du terme d'interaction life:ses
deviance_diff > qchisq(0.95, df = df_diff)

# donc le coefficient Beta_3 n'est pas significatif


# --------------------------------------------------------------------------
# Modèle Cumulatif sans structure proportionnelle
# --------------------------------------------------------------------------

model_cum <- vglm(formula = impair ~  life + ses, 
                  data = mental, 
                  family = cumulative)

summary(model_cum)

# Différence des déviances
deviance_diff <- deviance(model_cum_prop) - deviance(model_cum)
deviance_diff

# Différence de degrés de liberté
df_diff <- model_cum_prop@df.residual - model_cum@df.residual
df_diff


# On rejette H0 : Beta_1_1 = Beta_1_2 = Beta_1_3 et Beta_2_1 = Beta_2_2 = Beta_2_3 ? 
deviance_diff > qchisq(0.95, df = df_diff)



###############################################################################
# Données de comptage
###############################################################################

rm(list = setdiff(ls(), lsf.str()))

green <- read.csv(file = "data/green.csv", sep=",")
green

str(green)
summary(green)

# Modèle lo-linéaire de Poisson
model_poisson <- glm(formula = Species ~ pH + Biomass + pH:Biomass, 
                     family = "poisson",
                     data = green)

summary(model_poisson)

# Les prédictions du modèle correspondent assez bien aux observations
plot(green$Species, fitted(model_poisson), xlab = "Observations", ylab = "Prédictions",
     main = "Représentation des moyennes de Poisson prédites")
abline(1,1, col="red")


# Visualisation des résidus 
plot(residuals.glm(model_poisson))


# --------------------------------------------------------------------------
# Données hétérogènes
# --------------------------------------------------------------------------

# Dans le cas où les données ne sont pas toutes exprimées dans la même unité

# Jeu de données heartattack
death <- c(32,104,206,186,102,2,12,28,28,31)
personyears <- c(52407, 43248, 28612, 12663, 5317, 18790, 10673, 5710, 2585, 1462)
age <- rep (c (40 ,50 ,60 ,70 ,80) ,2)
status <- as.factor(c(rep("Smoker",5), rep("Non - smoker",5)))
heartattack <- data.frame(status, age, death, personyears)
heartattack

# offset(log(personyears)) correspond au terme log(n_i)
model_poisson_hetero <- glm (formula= death ~ age + status + offset(log(personyears)), 
                             family = "poisson", 
                             data = heartattack)

# Le nombre de  morts semble augmenter avec l'age et si on fume
summary(model_poisson_hetero)


# Modèle binomial négatif
#   pour résoudre un problème de sur-dispersion
require (MASS)
model_bin_negatif <- glm.nb(formula = death ~ age + status + offset(log(personyears)), 
                            data = heartattack)

# L'AIC et la déviance résiduelle sont bien meilleurs que le modèle de Poisson
summary(model_bin_negatif)


