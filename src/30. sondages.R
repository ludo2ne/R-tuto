#**************************************************************************
#* Introduction à la Théorie des Sondages avec R                          *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************


rm(list=ls())

# install.packages("Rlab")
library(Rlab)      # pour la loi de Bernouilli
library(dplyr)

setwd("P:/Ludo/Tuto/R-tuto")

# Import de la base d aeroports
aeroports <- read.csv(file = "data/aeroports.csv",
                      row.names = 1, 
                      header= TRUE,
                      sep=";")

aeroports

# Ajout des totaux, moyennes et variances
totaux <- colSums(aeroports)
moyennes <- round(colMeans(aeroports))
variances <- format(aeroports %>% summarise_if(is.numeric, var), scientific = TRUE, digit = 2)

rbind(format(aeroports, scientific = FALSE), 
      Total=totaux, 
      Moyenne=moyennes,
      S2 = variances)


# ---------------------------------------------------------------------
# Sondage aleatoire simple Sans Remise (SRS)
# ---------------------------------------------------------------------

N <- nrow(aeroports)        # Taille de la population
n <- 6                      # Taille de l echantillon
f <- n / N                  # Taux de sondage

Pi_k <- n / N               # Probabilites d inclusion
Pi_kl <- n/N * (n-1)/(N-1)  # Probabilites d inclusion d ordre 2


# ------------------------------
# Exemple de tirage
# ------------------------------

# On fait N tirages
rbern(N, prob = Pi_k) == 1

# Le probleme est que l on n est pas du tout sur d avoir n individus tires
# il peut y en avoir plus ou moins
aeroports[rbern(N, prob = Pi_k) == 1,]

# Par exemple si l on simule 10 tirages voici le resultat
for (i in 1:10) {
  print(sprintf("Tirage %s : nombre d aeroports tires = %s", 
                i, 
                nrow(aeroports[rbern(N, prob = Pi_k) == 1,])))
}

# ------------------------------
# On fixe un echantillon S1 pour la suite
# ------------------------------

S1 <- aeroports[c("Bastia", "Ajaccio","Strasbourg","Figari","Pau","Tarbes"),]
S1


# ------------------------------
# Estimation de la moyenne de Pass20
# ------------------------------

y_bar_S1 <- round(colMeans(S1[c("Pass20")]))   # Moyenne de Pass20
format(y_bar_S1, scientific = FALSE, big.mark=" ")

# Variance de la moyenne de Pass20
V_y_bar_Pass20 <- (1-f)/n * var(S1$Pass20)

# Estimation de l Intervalle de confiance 0.95 de la moyenne de Pass20
c(inf = round(y_bar_S1 - 1.96 * sqrt(V_y_bar_Pass20)), 
  sup = round(y_bar_S1 + 1.96 * sqrt(V_y_bar_Pass20))) 

# Estimation du Coefficient de variation
sprintf("%0.1f%%", sqrt(V_y_bar_Pass20) / y_bar_S1 * 100)


# ------------------------------
# Estimation du total pour Pass20
# ------------------------------

t_hat_Pass20 <- N * y_bar_S1              # Estimateur de Horvitz Thompson
format(t_hat_Pass20, scientific = FALSE, big.mark=" ")

# Estimation de la variance de l estimateur de Horvitz Thompson
s2_y <- var(S1$Pass20)                         # Estimateur de la variance des y_k (Variance corrigee)
V_hat_HT_Pass20 <- N^2 * (1 - f)/n * s2_y      # Estimateur de la variance de t_hat_y_Pi
format(V_hat_HT_Pass20, scientific = FALSE, big.mark=" ")

# Estimation de l Intervalle de confiance 0.95 de la moyenne de Pass20
c(inf = round(t_hat_Pass20 - 1.96 * sqrt(V_hat_HT_Pass20)), 
  sup = round(t_hat_Pass20 + 1.96 * sqrt(V_hat_HT_Pass20)))

# Estimation du Coefficient de variation
sprintf("%0.1f%%", sqrt(V_hat_HT_Pass20) / t_hat_Pass20 * 100)


# ---------------------------------------------------------------------
# Sondage aleatoire simple Stratifie
# ---------------------------------------------------------------------

# Decoupage de la population en 2 strates
U_1 <- aeroports[1:6,]
U_2 <- aeroports[7:12,]
N_1 <- nrow(U_1)
N_2 <- nrow(U_2)

# Allocation optimale pour la variable Pass19
n_1 <- n * (N_1 * sqrt(var(U_1$Pass19))) / (N_1 * sqrt(var(U_1$Pass19)) + N_2 * sqrt(var(U_2$Pass19)))
n_2 <- n * (N_2 * sqrt(var(U_2$Pass19))) / (N_1 * sqrt(var(U_1$Pass19)) + N_2 * sqrt(var(U_2$Pass19)))

c(n1=n_1, n2=n_2)
round(c(n1=n_1, n2=n_2))

# Allocation optimale pour la variable Pop19
n_1 <- n * (N_1 * sqrt(var(U_1$Pop19))) / (N_1 * sqrt(var(U_1$Pop19)) + N_2 * sqrt(var(U_2$Pop19)))
n_2 <- n * (N_2 * sqrt(var(U_2$Pop19))) / (N_1 * sqrt(var(U_1$Pop19)) + N_2 * sqrt(var(U_2$Pop19)))

c(n1=n_1, n2=n_2)
round(c(n1=n_1, n2=n_2))


# ------------------------------
# Exo page 72
# ------------------------------

# Decoupage en strates
V_0 <- aeroports[c("Montpellier","Figari"),]
V_1 <- aeroports[c("Bastia", "Ajaccio", "Strasbourg", "Brest", "Biarritz"),]
V_2 <- aeroports[c("Rennes","Pau", "Toulon", "Perpignan", "Tarbes"),]

Nh_0 <- nrow(V_0)
Nh_1 <- nrow(V_1)
Nh_2 <- nrow(V_2)

# Tirage dans chacune des strates
S_V_1 <- V_1[c("Bastia", "Brest"),]
S_V_2 <- V_2[c("Rennes","Pau"),]

# Moyenne de Pass20 dans les strates
y_bar_V_0 <- round(colMeans(V_0[c("Pass20")]))   
y_bar_V_1 <- round(colMeans(S_V_1[c("Pass20")]))   
y_bar_V_2 <- round(colMeans(S_V_2[c("Pass20")]))   

# Estimateur de Horvitz Thompson de Pass20
format(Nh_0 * y_bar_V_0 + 
       Nh_1 * y_bar_V_1 + 
       Nh_2 * y_bar_V_2, 
       scientific = FALSE, 
       big.mark=" ")


# ---------------------------------------------------------------------
# Tirage a probabilites inegales
# ---------------------------------------------------------------------

# ------------------------------
# Probabilites d inclusion proportionnelles a Pass19
# ------------------------------

aeroports$Pi_k <- round(n * aeroports$Pass19 / sum(aeroports$Pass19), 2)
aeroports

# Visualisation des Pi_k
barplot(height = aeroports$Pi_k,
        name = rownames(aeroports),
        col = "lightblue",
        las = 2,
        ylim = c(0,1))

# ------------------------------
# Probabilites d inclusion proportionnelles a Pop19
# ------------------------------

aeroports$Pi_k <- round(n * aeroports$Pop19 / sum(aeroports$Pop19), 2)

# Probleme il y a des probabilites superieures a 1
aeroports
aeroports[aeroports$Pi_k > 1,]

# On ramene ces probabilites a 1 et on recalcule l ensemble des autres probabilites
aeroports$Pi_k[aeroports$Pi_k > 1] <- 1
n_restants <- n - nrow(aeroports[aeroports$Pi_k == 1,])

aeroports[aeroports$Pi_k < 1,]$Pi_k <- round(n_restants * aeroports[aeroports$Pi_k < 1,]$Pop19 / sum(aeroports[aeroports$Pi_k < 1,]$Pop19), 2)
aeroports

# Verification que la somme des Pi_k est bien egale a n
sum(aeroports$Pi_k)

# Visualisation des Pi_k
barplot(height = aeroports$Pi_k,
        name = rownames(aeroports),
        col = "lightblue",
        las = 2,
        ylim = c(0,1))
abline(h=1 , col="grey")


# ---------------------------------------------------------------------
# Tirage de Poisson
# ---------------------------------------------------------------------

# TODO


# ---------------------------------------------------------------------
# Tirage systematique
# ---------------------------------------------------------------------

# On remet les probabilites d inclusion proportionnelles a Pass19
aeroports$Pi_k <- round(n * aeroports$Pass19 / sum(aeroports$Pass19), 2)


# Determination des intervalles
aeroports$Vk <- cumsum(aeroports$Pi_k)     # Somme cumulative
aeroports$Vk_1 <- c(0, aeroports$Vk[-N])   # on recopie V_k dans V_k1 en commencant par 0 et ainsi en decalant de 1
aeroports

# Generation d une valeur entre 0 et 1
u <- runif(1, min=0, max=1)

# Tirage
aeroports[aeroports$Vk_1 - floor(aeroports$Vk) <= u & u < aeroports$Vk - floor(aeroports$Vk),]

# Autre tirage
u <- 0.11
aeroports[aeroports$Vk_1 - floor(aeroports$Vk) <= u & u < aeroports$Vk - floor(aeroports$Vk),]



# TODO ex p91

# ---------------------------------------------------------------------
# Echantillonage par grappes
# ---------------------------------------------------------------------


