#**************************************************************************
#* M�thodes factorielles                                                  *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

library(FactoMineR)
# data(poison)
# data(footsize)

setwd("P:/Ludo/Tuto/R-tuto")

# -----------------------------------------------------------------------------
# R�aliser une ACP
# -----------------------------------------------------------------------------

# 1. Description des donn�es (source, type de variables, donn�es manquantes...)
# 2. Statistiques descriptives univari�es et bivari�es
# 3. Choix des �l�ments actifs et suppl�mentaires
# 4. Choix de la m�trique
# 5. Interpr�ter les axes (choisir le nombres d'axes)
# 6. Conclure (revenir aux variables de d�part et montrer que les interpr�tations sont correctes)


# -----------------------------------------------------------------------------
# Import du jeu de donn�es
# -----------------------------------------------------------------------------

temperature <- read.table("data/temperatures.csv", 
                          header = TRUE,
                          sep = ";",
                          row.names = 1)

# on transforme la variable Region est factor
temperature$Region <- as.factor(temperature$Region)


# -----------------------------------------------------------------------------
# Statistiques descriptives
# -----------------------------------------------------------------------------

str(temperature)
summary(temperature)

# On voit que sur des mois proches les temp�ratures sont tr�s corr�l�es
plot(temperature[,1:6])
plot(temperature[,7:12])


# -----------------------------------------------------------------------------
# Analyse en Composantes Principales (ACP)
#   variables quantitatives
# -----------------------------------------------------------------------------

# On r�alise une ACP avec
#   variables actives         : uniquement les temp�ratures mensuelles (colonnes 1 � 12)
#   individus actifs          : principalement les capitales (lignes 1 � 23)
#   individus suppl�mentaires : les autres villes (lignes 24 � 35)
# Les individus suppl�mentaires ne participent pas � la cr�ation des axes
# mais l'on souhaite tout de m�me les observer sur les nouveaux axes
res_acp <- PCA(temperature[,1:12], ind.sup = c(24:35), graph = FALSE)

summary(res_acp)

# Valeurs propres et variances expliqu�es
res_acp$eig

# Inertie par axe
barplot(res_acp$eig[,2], names.arg = paste0("dim", 1:nrow(res_acp$eig)), 
        ylim = c(0,100), main="Inertie par axe")

# Cercle des corr�lations
#   toutes les fl�ches vont dans la m�me direction : c'est un effet taille
plot(res_acp, choix = "var")

# Nuage des individus selon les deux premiers axes factoriels
plot(res_acp, choix = "ind")

# Individus les mieux repr�sent�s sur l'axe 1
#   ie ceux qui ont un cos2 proche de 1
res_acp$ind$cos2[order(res_acp$ind$cos2[,"Dim.1"], decreasing=TRUE),]

# Individus les plus contributeurs � la construction de l'axe 2
res_acp$ind$contrib[order(res_acp$ind$contrib[,"Dim.2"], decreasing=TRUE),]


# ACP en ajoutant des variables suppl�mentaires
res_acp2 <- PCA(temperature, ind.sup = c(24:35), 
                quali.sup=17, quanti.sup=c(13:16), graph = FALSE)

# Mise en �vidence des r�gions
plot(res_acp2, choix = "ind",
     habillage = "Region",
     col.hab = c("grey", "blue", "black", "red"))


# L'axe 1 permet de distinguer les villes plut�t chaudes et celles plut�t froide
# L'axe 2 semble s�parer les villes au climat oc�anique de celles au climat continental


# -----------------------------------------------------------------------------
# Analyse Factorielle Discriminante (AFD)
#   ACP en ajoutant une variable qualitative
# -----------------------------------------------------------------------------

# Elle permet de visualiser l'inertie par modalit� de cette variable qualitative
# il faut utiliser un codage disjonctif complet
# c'est � dire transformer chaque modalit� en variable

TODO

# -----------------------------------------------------------------------------
# Analyse Factorielle des Correspondances (AFC)
#   liaisons entre 2 variables qualitatives
# -----------------------------------------------------------------------------

# au pr�alable, tester l'ind�pendance entre ces 2 variables 
# par un test d'ind�pendance du Chi-2


# Number of medals in athletism during olympic games per country
data(JO)
JO
image(JO[,58:1])



res_afc <- CA(JO, graph = FALSE)

# Inertie par axe
barplot(res_afc$eig[,2], names.arg = 1:nrow(res_afc$eig), 
        ylim = c(0,20), ylab = "Pourcentage d'inertie",
        xlab = "Axes",
        main = "Inertie par axe")

plot.CA(res_afc, invisible = "row", selectCol="cos2 0.4")


# Afficher les contributions triees DESC par axe 1
View(res_afc$row$contrib[order(res_afc$row$contrib[,1], decreasing=TRUE),])

# Contribution par axe des pays
View(res_afc$col$contrib)
# ken, eth, mar, usa
View(res_afc$col$coord)
# Par contre au niveau des coordonnees usa est cote positif alors que les autres forts contributeurs sont cote negatif

# Qualite de representation par axe
View(res$col$cos2)
View(res$row$cos2)


# Axe 1 : 
#   epreuves d endurance Vs autres
#   Pays d Afrique Vs autres
# Axe 2 : 
#   forte contribution : Disque, Marteau
#   epreuves Force (+Grande endurance) Vs Sprint
#   ex URSS Vs USA, Jamaique

# -----------------------------------------------------------------------------
# Analyse factorielle des correspondances multiples (ACM)
#   liaisons entre plusieurs variables qualitatives
# -----------------------------------------------------------------------------

