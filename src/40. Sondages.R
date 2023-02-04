#**************************************************************************
#* Introduction à la Théorie des Sondages avec R                          *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")


# ---------------------------------------------------------------------
# Packages utilisés
# ---------------------------------------------------------------------

library(dplyr)


# ---------------------------------------------------------------------
# Import des données
# ---------------------------------------------------------------------

# Import de la base d aeroports
aeroports <- read.csv2(file = "data/aeroports.csv",
                       row.names = 1,                      # utiliser la colonne 1 comme rownames
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
# Sondage aléatoire simple Sans Remise (SRS)
# ---------------------------------------------------------------------

N <- nrow(aeroports)        # Taille de la population
n <- 6                      # Taille de l echantillon
f <- n / N                  # Taux de sondage

Pi_k <- n / N               # Probabilites d inclusion
Pi_kl <- n/N * (n-1)/(N-1)  # Probabilites d inclusion d ordre 2


# ------------------------------
# Exemple de tirage
# ------------------------------

# On fait N tirages suivant une loi de Bernouilli de paramètre Pi_k
rbinom(N, 1, prob = Pi_k) == 1

# Le problème est que l'on n'est pas du tout sur d'avoir n individus tirés
# il peut y en avoir plus ou moins
aeroports[rbinom(N, 1, prob = Pi_k) == 1,]

# Par exemple si l'on simule 10 tirages voici le resultat
for (i in 1:10) {
  print(sprintf("Tirage %s : nombre d aeroports tires = %s", 
                i, 
                nrow(aeroports[rbinom(N, 1, prob = Pi_k) == 1,])))
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

# Estimation de l'Intervalle de confiance 0.95 de la moyenne de Pass20
c(inf = round(y_bar_S1 - 1.96 * sqrt(V_y_bar_Pass20)), 
  sup = round(y_bar_S1 + 1.96 * sqrt(V_y_bar_Pass20))) 

# Estimation du Coefficient de variation
sprintf("%0.1f%%", sqrt(V_y_bar_Pass20) / y_bar_S1 * 100)


# ------------------------------
# Estimation du total pour Pass20
# ------------------------------

t_hat_Pass20 <- N * y_bar_S1              # Estimateur de Horvitz-Thompson
format(t_hat_Pass20, scientific = FALSE, big.mark=" ")

# Estimation de la variance de l'estimateur de Horvitz-Thompson
s2_y <- var(S1$Pass20)                         # Estimateur de la variance des y_k (Variance corrigée)
V_hat_HT_Pass20 <- N^2 * (1 - f)/n * s2_y      # Estimateur de la variance de t_hat_y_Pi
format(V_hat_HT_Pass20, scientific = FALSE, big.mark=" ")

# Estimation de l'Intervalle de confiance 0.95 de la moyenne de Pass20
c(inf = round(t_hat_Pass20 - 1.96 * sqrt(V_hat_HT_Pass20)), 
  sup = round(t_hat_Pass20 + 1.96 * sqrt(V_hat_HT_Pass20)))

# Estimation du Coefficient de variation
sprintf("%0.1f%%", sqrt(V_hat_HT_Pass20) / t_hat_Pass20 * 100)


# ---------------------------------------------------------------------
# Sondage aléatoire simple Stratifié
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
pie(round(c(n1=n_1, n2=n_2)) , 
    # labels = c("U1","U2"),
    labels = paste0(c("U1","U2"), " : ", round(c(n_1, n_2))),
    col = c("lightblue", "pink"))

# Allocation optimale pour la variable Pop19
n_1 <- n * (N_1 * sqrt(var(U_1$Pop19))) / (N_1 * sqrt(var(U_1$Pop19)) + N_2 * sqrt(var(U_2$Pop19)))
n_2 <- n * (N_2 * sqrt(var(U_2$Pop19))) / (N_1 * sqrt(var(U_1$Pop19)) + N_2 * sqrt(var(U_2$Pop19)))

c(n1=n_1, n2=n_2)
pie(round(c(n1=n_1, n2=n_2)) , 
    # labels = c("U1","U2"),
    labels = paste0(c("U1","U2"), " : ", round(c(n_1, n_2))),
    col = c("lightblue", "pink"))


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

# Estimateur de Horvitz-Thompson de Pass20
format(Nh_0 * y_bar_V_0 + 
       Nh_1 * y_bar_V_1 + 
       Nh_2 * y_bar_V_2, 
       scientific = FALSE, 
       big.mark=" ")


# ---------------------------------------------------------------------
# Tirage à probabilités inégales
# ---------------------------------------------------------------------

# ------------------------------
# Probabilités d'inclusion proportionnelles a Pass19
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
# Probabilités d'inclusion proportionnelles a Pop19
# ------------------------------

aeroports$Pi_k <- round(n * aeroports$Pop19 / sum(aeroports$Pop19), 2)

# Problème il y a des probabilités supérieures a 1
aeroports
aeroports[aeroports$Pi_k > 1,]

# On ramène ces probabilites a 1 et on recalcule l ensemble des autres probabilités
aeroports$Pi_k[aeroports$Pi_k > 1] <- 1
n_restants <- n - nrow(aeroports[aeroports$Pi_k == 1,])

aeroports[aeroports$Pi_k < 1,]$Pi_k <- round(n_restants * aeroports[aeroports$Pi_k < 1,]$Pop19 / sum(aeroports[aeroports$Pi_k < 1,]$Pop19), 2)
aeroports

# Vérification que la somme des Pi_k est bien égale a n
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
#   pour chaque unité k de la population on génère u_k ~ U[0,1]
#   si u_k < Pi_k alors l'unité est retenue dans l'échantillon
# ---------------------------------------------------------------------

# On remet les probabilités d'inclusion proportionnelles a Pass19
aeroports$Pi_k <- round(n * aeroports$Pass19 / sum(aeroports$Pass19), 2)

aeroports$u_k <- runif(N, min=0, max=1)
aeroports

# Tirage
aeroports[aeroports$u_k <= aeroports$Pi_k,]

# ------------------------------
# Exo p91
# ------------------------------

aeroports$u_k <- c(0.46, 0.75, 0.58, 0.71, 0.79, 0.14, 0.93, 0.36, 0.40, 0.51, 0.41, 0.40)
echantillon <- aeroports[aeroports$u_k <= aeroports$Pi_k,]
echantillon

# Estimation du total de Pass20 et Trans20
t_hat_Pass20 <- sum(echantillon$Pass20 / echantillon$Pi_k)
t_hat_Pass20

t_hat_Trans20 <- sum(echantillon$Trans20 / echantillon$Pi_k)
t_hat_Trans20

# Estimation de la variance
V_hat_HT_Pass20 <- sum((echantillon$Pass20 / echantillon$Pi_k)^2 * (1 - echantillon$Pi_k))
V_hat_HT_Trans20 <- sum((echantillon$Trans20 / echantillon$Pi_k)^2 * (1 - echantillon$Pi_k))

# Estimation du Coefficient de variation
sprintf("%0.1f%%", sqrt(V_hat_HT_Pass20) / t_hat_Pass20 * 100)
sprintf("%0.1f%%", sqrt(V_hat_HT_Trans20) / t_hat_Trans20 * 100)


# ---------------------------------------------------------------------
# Tirage systematique
# ---------------------------------------------------------------------

# on retire la colonne u_k dont on n'a plus besoin
aeroports <- subset(aeroports, select = -c(u_k))

# Détermination des intervalles
V_k <- cumsum(aeroports$Pi_k)     # Somme cumulative
aeroports$V_k_1 <- c(0, V_k[-N])   # on recopie V_k dans V_k1 en commencant par 0 et ainsi en decalant de 1
aeroports$V_k <- V_k
aeroports

# Génération d'une valeur entre 0 et 1 avec une loi uniforme
u <- runif(1, min=0, max=1)

# Tirage
#   à vérifier car formule un peu bancale
aeroports[u >= aeroports$V_k_1 - floor(aeroports$V_k) & u < aeroports$V_k - floor(aeroports$V_k) | u >= aeroports$V_k_1 - floor(aeroports$V_k_1) & u < aeroports$V_k - floor(aeroports$V_k_1),]


# ------------------------------
# Exo p91
# ------------------------------

aeroports

u <- 0.11
echantillon <- aeroports[u >= aeroports$V_k_1 - floor(aeroports$V_k) & u < aeroports$V_k - floor(aeroports$V_k) | u >= aeroports$V_k_1 - floor(aeroports$V_k_1) & u < aeroports$V_k - floor(aeroports$V_k_1),]
echantillon

# Estimation du total de Pass20 et Trans20
t_hat_Pass20 <- sum(echantillon$Pass20 / echantillon$Pi_k)
t_hat_Pass20

t_hat_Trans20 <- sum(echantillon$Trans20 / echantillon$Pi_k)
t_hat_Trans20


# ---------------------------------------------------------------------
# Approche assistée par un modèle
# ---------------------------------------------------------------------

# Régression liénaire de Pass20 sur Pass19
reg1 <- lm(Pass20 ~ Pass19, data = aeroports)
summary(reg1)

plot(aeroports$Pass19, aeroports$Pass20, xlim = c(0,2000000), ylim = c(-100000, 800000))
abline(reg1)


# -----------------------------------------------------------------------------
# Bonus
# -----------------------------------------------------------------------------

notes_2022 <- c(0, 1.5, 3, 3, 3.5, 3.5, 4, 4.5, rep(5,3), 5.5, 6, 6, 6.5, 6.5, rep(7,6))
notes_2022 <- c(notes_2022, rep(7.5,3), rep(8,4), rep(8.5,5), rep(9,2), rep(10,8), rep(10.5,8))
notes_2022 <- c(notes_2022, rep(11,7), rep(11.5,8), rep(12,11), rep(12.5, 13), rep(13,15), rep(13.5,6))
notes_2022 <- c(notes_2022, rep(14,5), rep(14.5,8), rep(15,12), rep(15.5,8), rep(16,9), rep(16.5,4))
notes_2022 <- c(notes_2022, rep(17,3), rep(17.5,2), rep(18,2), 19)

moyenne_notes_2022 <- mean(notes_2022)

hist(notes_2022, 
     main="Distribution 2022 des notes de Théorie des Sondages",
     xlab="Notes 2022",
     xlim=c(0,20),
     col="lightblue", 
     breaks = length(table(notes_2022)))
abline(v=moyenne_notes_2022, col="red")

