#**************************************************************************
#* Classification non supervisée                                                             *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************


# Ici nous ne connaissons ni les classes, ni leur nombre
# Nous allons utiliser des algorithmes pour déterminer ces classes
# Le but est de regrouper les observations en clusters
# ayant une variance intra faible et une variance inter la plus forte possible

rm(list=ls())

library(FactoMineR)
library(cluster)
library(VarSelLCM)

setwd("P:/Ludo/Tuto/R-tuto")


# -----------------------------------------------------------------------------
# Import du jeu de données
# -----------------------------------------------------------------------------

temperature <- read.table("data/temperatures.csv", 
                          header = TRUE,
                          sep = ";",
                          row.names = 1)

# on transforme la variable Region est factor
temperature$Region <- as.factor(temperature$Region)

str(temperature)

# On ne garde que les variables de températures mensuelles
temperature <- temperature[, 1:12]

# Suppression des variables non numériques
dataToCluster <- Filter(is.numeric, temperature)

# Centrage des variables
dataToCluster <- scale(dataToCluster)

head(dataToCluster)


# -------------------------------------------------
# Algo du K-means
# -------------------------------------------------

nb_cluster_max <- 8
resKmeans <- list()              # va contenir les resultats des k-means pour chaque valeur de K
CPtheta <- rep(0,nb_cluster_max) # va contenir l'inertie intra pour chaque valeur de K

# Pour K allant de 1 a 8 (nombre de clusters)
for (K in 1:nb_cluster_max){
  resKmeans[[K]] <- kmeans(dataToCluster, K, nstart = 50)
  CPtheta[K] <- resKmeans[[K]]$tot.withinss
}

# on voit qu en dessous de 3 clusters l'inertie intra augmente significativement (coude)
plot(x= 1:nb_cluster_max, 
     y= CPtheta, 
     type = "b", 
     xlab = "K", 
     ylab = "Inertie intra",
     main = "Inertie intra en fonction du nombre de clusters")


# Nombre d'éléments dans chacun des 3 clusters
resKmeans[[3]]$size

# Ajout de la variable qualitative cluster
cluster_kmeans <- resKmeans[[3]]$cluster
temperature <- cbind(temperature, cluster_kmeans = as.factor(cluster_kmeans))

# Visulisation des 3 clusters sur une ACP
acp_kmeans <- PCA(temperature, 
                  quali.sup = which(colnames(temperature) == 'cluster_kmeans'),
                  graph = FALSE)

plot.PCA(acp_kmeans, 
         choix = "ind", 
         habillage = "cluster_kmeans",
         col.hab = c("blue","red","green"),
         invisible = c("quali"), 
         title = "Clustering par K-means")


# -------------------------------------------------
# Classification ascendante hierarchique (CAH)
# -------------------------------------------------

# CAH avec critere de Ward
#    method = "single" pour mini
CAHward <- agnes(dataToCluster, metric = "euclidean", method = "ward") 
plot(CAHward, 
     which.plots = 2,
     main = "Dendogramme Ward")

# Découpage en 3 clusters
cluster_cah <- cutree(CAHward, 3) 
temperature <- cbind(temperature, cluster_cah = as.factor(cluster_cah))

# Visulisation des 3 clusters sur une ACP
acp_cah <- PCA(temperature, 
               quali.sup = which(colnames(temperature) %in% c("cluster_cah","cluster_kmeans")),
               graph = FALSE)

plot.PCA(acp_cah, 
         choix = "ind",  
         habillage = "cluster_cah",
         invisible = c("quali"), 
         title = "Clustering par CAH")


# -------------------------------------------------
# Comparaisons CAH Ward et K-means
# -------------------------------------------------

partitionCAH <- cutree(CAHward, 3) 
partitionKmeans <- kmeans(dataToCluster, 3) 

# Matrice de confusion entre les deux partitions 
table(partitionKmeans$cluster, partitionCAH) 

# Adjusted Rand Index (Indice de Rand)
ARI(partitionKmeans$cluster, partitionCAH)
