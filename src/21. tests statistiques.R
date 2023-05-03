#**************************************************************************
#* Tests Statisiques                                                      *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

#--------------------------------------------------------------------------
# Plantage de décors
#   nous souhaitons tester si une pièce est non truquée
#--------------------------------------------------------------------------

set.seed(1986)

n <- 100     # nombre de lancers
p <- 0.5     # Probabilité d'avoir un pile

# Simulation de n lancers
X <- rbinom(n, 1, prob = p)

#-----------------------------------
# Quelques statistiques descriptives
#-----------------------------------

mean(X)
var(X)     # p * (1-p)
table(X)
prop.table(table(X))

#-----------------------------------
# Loi Forte des Grands Nombres
#   plus nous augmentons le nombre de lancers
#   plus la moyenne empirique converge vers l'espèrance
#-----------------------------------

x_bar <- rep(NA, n)
for (i in 1:n) {
  x_bar[i] <- sum(X[1:i]) / i
}
plot(x_bar, type = "l")
abline(h = p, col = "red", lty = 2)

#-----------------------------------
# Théorème Central Limite
#   nous répètons L fois l'expérience de n lancers
#   à chaque fois on calcule la moyenne obtenue
#-----------------------------------

L <- 1000
x_bar <- rep(NA, L)
for (i in 1:L){
  X <- rbinom(n, 1, prob = p)
  x_bar[i] <- mean(X)
}

# Nous normalisons
Y <- sqrt(n) * (x_bar - p) / sqrt(p * (1 - p))
hist(Y, breaks = 30, probability = TRUE)

# Cette loi correspond bien à une loi normale centrée réduite
plot(density(Y), xlim = c(-5, 5), col = "blue", lwd = 2,
     main = "Comparaison des densités empiriques et théoriques")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topleft", inset = 0.05, lty = c(1, 2),
       c("Densité empirique", "Densité de la loi N(0, 1)"), 
       col = c("blue", "red"))


#--------------------------------------------------------------------------
# Mon premier test
#   Tester si une pièce est truquée
#--------------------------------------------------------------------------

# Supposons que nous lançons 100 fois une pièce et que nous obtenons
#   58 pile (1)
#   42 face (0)
n <- 100
nb_pile <- 58
X <- c(rep(1, nb_pile), rep(0, n - nb_pile))
table(X)

# Est-ce que cette pièce est truquée ?
# Nous allons établir un test pour voir si la moyenne empirique obtenue
# s'éloigne trop de la moyenne théorique


#-----------------------------------
# Soit H0 : la pièce n'est pas truquée (p = 0.5)
#   Vs H1 : la pièce est truquée (p != 0.5)
#-----------------------------------

# Nous définissons le niveau de risque
#   un niveau de risque à O.05 signifie que l'on veut être sûr à 95 %
#   de ne pas se tromper
alpha <- 0.05

x_bar <- mean(X)

#-----------------------------------
# Soit la statistique de test
#-----------------------------------
T <- sqrt(n) * (x_bar - 0.5) / sqrt(p * (1 - p))

#-----------------------------------
# Sous HO, T suit une loi Normale Centrée Réduite
#   Nous définissons donc la zone de rejet suivante
#   Nous rejetons H0 si T n'appartient pas à l'intervalle suivant
#-----------------------------------
c(-1, 1) * qnorm(1 - alpha / 2)

# Est-ce que l'on rejette H0 ?
T > qnorm(1 - alpha / 2) & T > qnorm(1 - alpha / 2)

# On ne peut pas rejeter H0 au niveau de risque 0.05

#-----------------------------------
# Resprésentation graphique
#   Nous remarquons que T ne se trouve pas dans la zone de rejet
#   Si T se trouvait dans la zone de rejet, nous aurions affirmé
#   qu'il est peut probable que T suive une N(0, 1) et donc nous
#   aurions rejeté H0
#-----------------------------------

curve(dnorm(x), col = "blue", lwd = 2, xlim = c(-4, 4),
      main = "Loi de T sous H0 : N(0, 1)")

zone_rejet_gauche <- seq(-4, qnorm(alpha/2), 0.01)
zone_rejet_droite <- seq(qnorm(1-alpha/2), 4, 0.01)
polygon(x = c(zone_rejet_gauche[1], zone_rejet_gauche, zone_rejet_gauche[length(zone_rejet_gauche)]), 
        y = c(0, dnorm(zone_rejet_gauche), 0), 
        col = "orange")
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dnorm(zone_rejet_droite), 0), 
        col = "orange")

rug(T, lwd = 2, col = "green", ticksize = 1, lty = 2)

legend("topleft", inset=.05, 
       c("Zones de rejet", "Stat de test"), 
       fill = c("orange", NA), lty = c(NA, 2), col = c("orange", "green"),border=c(1,0))


#-----------------------------------
# p-valeur
# plusieurs définitions
#   Probabilité d'obtenir une valeur aussi extrême que celle observée
#   plus grande valeur possible de α conduisant à ne pas rejeter H0
#-----------------------------------
p_val <- 2 * (1 - pnorm(T)) ; p_val




#--------------------------------------------------------------------------
# Test Unilatéral
#   dans l'exemple précédent, nous avons effectué un test bilatéral
#   supposons que nous soyons sûrs que si la pièce est truquée
#   c'est en faveur du pile
#   on ne va rejeter que d'un seul coté
#--------------------------------------------------------------------------













#-------------------------------------
# Densite de la loi de Student
#-------------------------------------

n <- 10           # degres de liberte
alpha <- 0.05     # niveau de risque

# Definition des borne et du nombre de points a tracer
val1 <- seq(-5, 5, length.out = 1000)

plot(x = val1, y = dt(val1,n),        # Calcul des valeurs de la fonction de densite
     type = "l", lwd = 3, col = "blue", # Representation par une courbe bleue
     xlim = c(-5, 5), 
     main = "Densit? de la statistique de test", 
     xlab = "R?alisation", ylab = bquote(paste("Densit? sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)


lines(c(-5, 5), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilat?ral)
zone_rejet_gauche <- seq(-5, qt(alpha/2, n), 0.01)
zone_rejet_droite <- seq(qt(1-alpha/2, n), 5, 0.01)

# Trace du polygone de la region de rejet
polygon(x = c(zone_rejet_gauche[1], zone_rejet_gauche, zone_rejet_gauche[length(zone_rejet_gauche)]), 
        y = c(0, dt(zone_rejet_gauche, n), 0), 
        col = "green")
polygon(x = c(zone_rejet_droite[1], zone_rejet_droite, zone_rejet_droite[length(zone_rejet_droite)]), 
        y = c(0, dt(zone_rejet_droite, n), 0), 
        col = "green")
legend("topright", inset=.05, 
       c("Zones de rejet"), 
       fill=c("green"))


# -----------------------------------------------------------------------------
# P-valeur
#   niveau de significativit? le plus petit alpha tel que l'on rejette H0
# -----------------------------------------------------------------------------

# On veut tester H0 : t = 0 vs H1 : t <> 0 au niveau de risque alpha
# On suppose que sous H0, t suit une loi de student

alpha <- 0.05     # Niveau de risque
t_obs <- 4        # Soit t_obs la valeur observee 

# Probabilite qu une realisation suivant la loi de Student soit inferieure a la valeur absolue de t_obs
pt(abs(t_obs), n)

# La p-valeur est le niveau de risque pivot
#   si alpha > p_val alors on rejette H0
#   si alpha < p_val alors on ne rejette pas H0
p_val <- 2 * (1 - pt(abs(t_obs), n))
p_val

# Est ce que l on rejette H0
p_val < alpha


qt(1 - p_val / 2, n)




# -----------------------------------------------------------------------------
# Test de Shapiro-Wilk
#   Test de Normalité
# -----------------------------------------------------------------------------

#d issu d'une loi normale
d<-rnorm(100, mean = 5, sd = 3)

#e issu d'une loi uniforme
e<-runif(100, min = 2, max = 4)

# on r?alise un test de shapiro

shapiro.test(d)
qqnorm(d) #p=0.35 on ne rejette pas l'hypoth?se et on v?rifie avec un QQplot

shapiro.test(e)
qqnorm(e) # p=0.00036 on rejette l'hypoth?se et on v?rifie avec un QQplot


# -----------------------------------------------------------------------------
# Test de Kolmogorov-Smirnov
# -----------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Test du chi2 d'ad?quation
#   H0 : V?rifie si un ?chantillon suit bien une certaine variable al?atoire 
# ------------------------------------------------------------------------

https://sites.google.com/site/rgraphiques/4--stat/comparer-et-%C3%A9tudier-des-effectifs-et-proportions-en-langage-r/test-du-ch2-khi2

# ------------------------------------------------------------------------
# Test du chi2 d'ind?pendance
# ------------------------------------------------------------------------



# ------------------------------------------------------------------------
# Test du chi2 d'homog?n?it?
# ------------------------------------------------------------------------



##########################################################################
# Tests Non param?triques
##########################################################################


https://fr.wikipedia.org/wiki/Test_de_Wilcoxon-Mann-Whitney
https://fr.wikipedia.org/wiki/Test_de_Kruskal-Wallis

