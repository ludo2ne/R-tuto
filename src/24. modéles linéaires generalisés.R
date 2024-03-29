#**************************************************************************
#* Mod�les lin�aires g�n�ralis�s                                          *
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
# Cas o� Y binaire : Jeu de donn�es toy
###############################################################################

X <- c(0.088, 0.99, 0.54, 0.062, 0.14, 0.51, 0.81, 0.39, 0.56, 0.90, 0.99, 0.63, 
       0.95, 0.0057, 0.055, 0.30, 0.91, 0.28, 0.16, 0.53)
Y <- c(0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1)
toy <- data.frame(X, Y)
rm(X, Y)

n <- nrow(toy)        # nombre d'observations
d <- 2                # nombre de regresseurs (constante incluse)

# On ordonne les donn�es pour faciliter les trac�s
toy <- toy[order(toy$X),]
rownames(toy) <- seq(1,n)


# Visualisation des donn�es
plot(toy$X, toy$Y, pch=3, col="blue", main = "Donn�es toy")

# Tentative de r�gression lin�aire... �a ne marche pas bien
abline(lm(toy$Y ~ toy$X), col="red")

# Mod�le de regression logistique (fonction de lien : logit par d�faut)
logit_model <- glm(formula = Y ~ X, data = toy, family = binomial)
summary(logit_model)

# Estimations de Y par le mod�le
toy$Y_hat <- fitted(logit_model)

# Calcul � la main (logit(Y) = alpha + beta * X)
alpha_hat <- logit_model$coefficients[1]
beta_hat <- logit_model$coefficients[2]
Y_hat <- invlogit(alpha_hat + beta_hat * toy$X)
data.frame(toy$Y_hat, a_la_main = Y_hat)


# ----------------------------------------------------------------------
# Intervalle de confiance des coefficients du mod�le
# ----------------------------------------------------------------------

# Intervalle de confiance de Wald de Beta_1
std_beta_hat <- sqrt(diag(vcov(logit_model)))["X"]
IC_beta <- c(inf = beta_hat - qnorm(0.975) * std_beta_hat, 
             sup = beta_hat + qnorm(0.975) * std_beta_hat)

# Ici 0 n'appartient pas � l'intervalle de confiance, donc on peut rejeter H0 : beta = 0
IC_beta

# On voit �galement que la p-valeur de beta est inf�rieure � 0.05
# Ce qui confirme qu'au seuil de 5% on peut rejeter H0
summary(logit_model)


# ----------------------------------------------------------------------
# Mod�le nul et Mod�le satur�
# ----------------------------------------------------------------------

# le mod�le satur� a autant de param�tres qu'il y a d'observation
#   il pr�dit parfaitement chaque observation
#   attention le mod�le satur� n'est pas un bon mod�le, car il colle trop aux donn�es d'apprentissage
model_sat <- glm(Y ~ as.factor(1:length(Y)), data = toy, family=binomial)
summary(model_sat)


# le mod�le nul n'a qu'un seul param�tre, il pr�dit la moyenne de la variable � expliquer
model_nul <- glm(Y ~ 1, data = toy, family=binomial)
summary(model_nul)

# Le coefficient de Beta_0 est le logit de la moyenne des Pi
model_nul$coefficients
logit(mean(toy$Y))


# ----------------------------------------------------------------------
# Visualisation
# ----------------------------------------------------------------------

plot(toy$X, toy$Y, pch = 3, col = "blue", main = "Donn�es toy")

lines(toy$X, toy$Y_hat, type = "l", col = "red", lwd=2)
lines(toy$X, fitted(model_sat), type = "l", col = "green")
lines(toy$X, fitted(model_nul), type = "l", col = "orange")

legend("bottomright", inset = .05, lty = c(0, 1, 1, 1), pch = c(3, NA, NA, NA), 
       col = c("blue", "red", "green", "orange"), 
       legend = c("Valeurs observ�es", "Mod�le Logit", "Mod�le satur�", "Mod�le nul"))


# ----------------------------------------------------------------------
# Autres mod�les de la famille Binomiale (probit et loglog)
# ----------------------------------------------------------------------

probit_model <- glm(Y ~ X, family = binomial(link = "probit"), data = toy)
loglog_model <- glm(Y ~ X, family = binomial(link = "cloglog"), data = toy)

# Ajout au graphiques
lines(toy$X, fitted(probit_model), col="blue", lty=2)
lines(toy$X, fitted(loglog_model), col="darkorchid1", lty=3)

legend("bottomright", inset = .05, lty = c(0, 1, 1, 1, 3, 3), pch = c(3, NA, NA, NA, NA, NA), 
       col = c("blue", "red", "green", "orange", "blue", "darkorchid1"), 
       legend = c("Valeurs observ�es", "Mod�le Logit", "Mod�le satur�", "Mod�le nul", "Mod�le probit", "Mod�le loglog"))


# ----------------------------------------------------------------------
# Validation du mod�le logit - Calculs de D�viances
# ----------------------------------------------------------------------

# Null deviance
logit_model$null.deviance

# D�viance r�siduelle
logit_model$deviance             # ou deviance(logit_model)

# Formule de la d�viance r�siduelle
2 * (logLik(model_sat) - logLik(logit_model))


# Formule de la log-vraisemblance
logLik(logit_model)
sum(toy$Y * log(invlogit(alpha_hat + beta_hat * toy$X)) + 
    (1 - toy$Y) * log(1 - invlogit(alpha_hat + beta_hat * toy$X)))

# log-vraisemblance du mod�le nul
logLik(model_nul)
sum(toy$Y * log(model_nul$coefficients[1]) + (1 - toy$Y) * log(1 - model_nul$coefficients[1]))


# H0 : le mod�le n'est pas significatif (Beta_1 = 0)
# Sous H0, la diff�rence des d�viances suit une loi du chi2 � d-1 degr�s de libert�
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
     main = "Densit� de la statistique de test", 
     xlab = "R�alisation", ylab = bquote(paste("Densit� sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)

lines(c(0, 10), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilat�ral)
zone_rejet_droite <- seq(qchisq(1-alpha, d-1), 10, 0.01)

# Trace du polygone de la region de rejet
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dchisq(zone_rejet_droite, d-1), 0), 
        col = "green")

x <- logit_model$null.deviance - logit_model$deviance
lines(c(x, x),c(0,1),lwd = 3, col="red")
legend("top", inset=.05,  
       c("Diff�rence des d�viances", "Zone de rejet"), 
       text.col = c("red", "green"))

rm(x, zone_rejet_droite, val1)


# ----------------------------------------------------------------------
# Qualit� du mod�le(AIC, ROC)
# ----------------------------------------------------------------------

# l'AIC est un compromis entre avoir une bonne vraisemblance mais pas trop de param�tres
# Plus l'AIC est petit, meilleur est le mod�le
data.frame(Mod�le = c("logit", "probit", "loglog", "nul", "satur�"),
           AIC = c(logit_model$aic, probit_model$aic, loglog_model$aic, model_nul$aic, model_sat$aic))

# Formule de l'AIC
2 * d - 2 * logLik(logit_model)
logit_model$aic


library("ROCR")
# install.packages("ROCR")

# Courbes ROC
plot(performance(prediction(fitted(logit_model), toy$Y), "tpr", "fpr"), col="blue", 
     main = "ROC des diff�rents mod�les")
plot(performance(prediction(fitted(model_nul), toy$Y), "tpr", "fpr"), add=TRUE, col="red")
plot(performance(prediction(fitted(model_sat), toy$Y), "tpr", "fpr"), add=TRUE, col="green")

legend("bottomright", inset = .05, lty = c(1, 1, 1), 
       col = c("blue", "green", "red"), 
       legend = c("Mod�le Logit", "Mod�le satur�", "Mod�le nul"))




###############################################################################
# Donn�es group�es : beetle
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
# Validation du mod�le
# -----------------------------------------------------------------------------

# H0 : le mod�le n'est pas significatif (Beta_1 = 0)
# Sous H0, la diff�rence des d�viances suit une loi du chi2 � d-1 degr�s de libert�
# Est-ce que l'on rejette H0 ?
logit_model$null.deviance - logit_model$deviance > qchisq(0.95, df = d - 1)


# -----------------------------------------------------------------------------
# Dose l�tale a 50 pourcent
# -----------------------------------------------------------------------------

Beta_0 <- coefficients(logit_model)[1]
Beta_1 <- coefficients(logit_model)[2]

LD50 <- -Beta_0 / Beta_1
LD50

# Probabilit� de d�c�s sans poison
invlogit(Beta_0)

# Matrice de covariances asymptotiques de l'estimateur
vcov(logit_model)


# -----------------------------------------------------------------------------
# Intervalle de confiance pour la pr�diction
#   Attention a bien faire les choses dans le bon ordre
#     On calcule les IC puis on applique la fonction de lien inverse
#   Sinon on risque de se retrouver avec des IC contenant des valeurs n�gatives ou sup�rieures a 1
# -----------------------------------------------------------------------------

alpha <- 0.05

beetle
beetle$x
list(beetle$x)

# On demande a predict de renvoyer les valeurs non invers�es et se.fit (Standard Error)
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
# Y cat�gorielle avec notion d'ordre
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
#   impair : d�crivant l'�tat mental de la personne concern�e, de 1 (sain) � 4 (en mauvaise sant�)
#   ses    : qui vaut 1 si la personne a un statut socio-�conomique �lev�, 0 sinon
#   life   : mesurant le nombre et l'intensit� des bouleversements qu'a connus la personne au cours 
#            des trois derni�res ann�es, de 0 (aucun changement) � 9 (changements tr�s importants)


n <- nrow(mental)   # Nombre d'observations
d <- 3              # Nombre de pr�dicteurs


# --------------------------------------------------------------------------
# Mod�le Cumulatif proportionnel
# --------------------------------------------------------------------------

model_cum_prop <- vglm(formula = impair ~ life + ses, 
                       data = mental, 
                       family = cumulative(parallel = TRUE))

summary(model_cum_prop)
coefficients(model_cum_prop, matrix=TRUE)

# Odds ratio associ� � la variable ses
#   toutes choses �gales par ailleurs, avoir un ses de 1 permet d'avoir 
#   3 fois moins de chances d'�tre atteint mentalement qu'un ses de 0
exp(model_cum_prop@coefficients["ses1"])

# Intervalle de confiance de Wald pour cet Odds Ratio
exp(confint(model_cum_prop, 
            parm = "ses1", 
            level = 0.95))

# Calcul � la main de l'IC de Wald
Beta_1_hat <- coefficients(model_cum_prop)["ses1"]
std_Beta_1_hat <- sqrt(vcov(model_cum_prop)["ses1","ses1"])
IC_OR_min <- exp(Beta_1_hat - qnorm(0.975) * std_Beta_1_hat)
IC_OR_max <- exp(Beta_1_hat + qnorm(0.975) * std_Beta_1_hat)
c(IC_OR_min , IC_OR_max)

# --------------------------------------------------------------------------
# Mod�le Cumulatif proportionnel avec terme d'interaction
# --------------------------------------------------------------------------

model_cum_prop_avec_ti <- vglm(formula = impair ~  life + ses + life:ses, 
                                data = mental, 
                                family = cumulative(parallel = TRUE))

summary(model_cum_prop_avec_ti)

# Diff�rence des d�viances
deviance_diff <- deviance(model_cum_prop) - deviance(model_cum_prop_avec_ti)
deviance_diff

# Diff�rence de degr�s de libert�
df_diff <- model_cum_prop@df.residual - model_cum_prop_avec_ti@df.residual
df_diff


# La diff�rence des D�viances suit une loi du chi2 avec df = diff�rence de degr�s de libert�
# entre les 2 mod�les
# On rejette H0 : Beta_3 = 0 ? 
#   o� Beta_3 : coefficient du terme d'interaction life:ses
deviance_diff > qchisq(0.95, df = df_diff)

# donc le coefficient Beta_3 n'est pas significatif


# --------------------------------------------------------------------------
# Mod�le Cumulatif sans structure proportionnelle
# --------------------------------------------------------------------------

model_cum <- vglm(formula = impair ~  life + ses, 
                  data = mental, 
                  family = cumulative)

summary(model_cum)

# Diff�rence des d�viances
deviance_diff <- deviance(model_cum_prop) - deviance(model_cum)
deviance_diff

# Diff�rence de degr�s de libert�
df_diff <- model_cum_prop@df.residual - model_cum@df.residual
df_diff


# On rejette H0 : Beta_1_1 = Beta_1_2 = Beta_1_3 et Beta_2_1 = Beta_2_2 = Beta_2_3 ? 
deviance_diff > qchisq(0.95, df = df_diff)



###############################################################################
# Donn�es de comptage
###############################################################################

rm(list = setdiff(ls(), lsf.str()))

green <- read.csv(file = "data/green.csv", sep=",")
green

str(green)
summary(green)

# Mod�le lo-lin�aire de Poisson
model_poisson <- glm(formula = Species ~ pH + Biomass + pH:Biomass, 
                     family = "poisson",
                     data = green)

summary(model_poisson)

# Les pr�dictions du mod�le correspondent assez bien aux observations
plot(green$Species, fitted(model_poisson), xlab = "Observations", ylab = "Pr�dictions",
     main = "Repr�sentation des moyennes de Poisson pr�dites")
abline(1,1, col="red")


# Visualisation des r�sidus 
plot(residuals.glm(model_poisson))


# --------------------------------------------------------------------------
# Donn�es h�t�rog�nes
# --------------------------------------------------------------------------

# Dans le cas o� les donn�es ne sont pas toutes exprim�es dans la m�me unit�

# Jeu de donn�es heartattack
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


# Mod�le binomial n�gatif
#   pour r�soudre un probl�me de sur-dispersion
require (MASS)
model_bin_negatif <- glm.nb(formula = death ~ age + status + offset(log(personyears)), 
                            data = heartattack)

# L'AIC et la d�viance r�siduelle sont bien meilleurs que le mod�le de Poisson
summary(model_bin_negatif)


