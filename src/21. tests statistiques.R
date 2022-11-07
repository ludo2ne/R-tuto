#**************************************************************************
#* Tests Statisiques                                                      *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")


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
     main = "Densité de la statistique de test", 
     xlab = "Réalisation", ylab = bquote(paste("Densité sous ", H[0])), 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1)


lines(c(-5, 5), c(0, 0)) # Trace de la ligne inferieure

# Determination de la zone de rejet (attention, il y en a deux, car test bilatéral)
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




