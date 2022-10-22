#**************************************************************************
#* Les DataFrames en R                                                      *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

# Un dataframe est comme un tableau avec des lignes (donnees) et des colonnes (variables)

setwd("P:/Ludo/Tuto/R-tuto")

# Lire un fichier csv
personne <- read.csv2("data/personne.csv", 
                      sep = ";", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

# Ecrire dans un fichier
write.csv2(personne,"data/personne_bis.tmp")

# Quelques infos de base sur le dataframe
str(personne)
summary(personne$pointure)

colnames(personne)
rownames(personne)

# Renommer une colonne
colnames(personne)[colnames(personne) == "dnais"] <- "naissance"

# Renommer les lignes
row.names(personne) <- personne$prenom
personne

# Afficher les premieres ou dernieres lignes
head(personne,5)
personne[c(1:5),]
tail(personne,5)

# Supprimer les lignes 2 et 3
personne[-c(2,3),]

# Supprimer des colonnes
personne[,-c(1)]
personne[!colnames(personne) %in% c("taille", "pointure")]
select(personne, -"taille")

# Creer un vecteur avec toutes les donnees d une colonne
ma_colonne_2 <- personne[2]
ma_colonne_prenom <- list(personne$prenom)

# Changer le type
personne$pointure <- as.character(personne$pointure)
personne$pointure <- as.numeric(personne$pointure)
personne$naissance <- as.Date(personne$naissance, "%d/%m/%Y")
personne$ville <- as.factor(personne$ville)

str(personne)

# Recherche de doublons
any(duplicated(personne))      # renvoie TRUE s il y en a
personne <- unique(personne)   # suppression des doublons

# Jointure
#fusion <- merge(pop, superficie, by.x = "CODGEO", by.y = "DEPCOM")

# Trier par taille dans le desordre
personne[order(personne$taille, decreasing = TRUE),]
arrange(personne, desc(taille))   # avec dplyr

# Filter les personnes dont la taille est superieure a la moyenne
personne[personne$taille > mean(personne$taille),]
filter(personne, taille > mean(taille))   # avec dplyr
