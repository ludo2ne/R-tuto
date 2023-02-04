#**************************************************************************
#* Statistiques avec R                                                    *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

library(dplyr)

setwd("P:/Ludo/Tuto/R-tuto")

# Lire un fichier csv
personne <- read.csv2("data/personne.csv", 
                      sep = ";", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)


# Compter le nombre d individus par ville
table(personne$ville)
margin.table(table(personne[colnames(personne) %in% c("sexe", "ville")]),2)
count(personne, personne$ville)

personne %>%
  group_by(personne$ville) %>%
  summarize(cnt = n())


# Pourcentage par ville
prop.table(table(personne$ville))

personne %>%
  group_by(personne$ville) %>%
  summarize(cnt = n()) %>%
  mutate(pct = round(cnt / sum(cnt) * 100, 2)) %>% 
  arrange(desc(pct))


# Moyenne, variance
mean(personne$taille)
var(personne$taille)          # variance corrigee
sd(personne$taille)

# Moyenne des tailles par ville
tapply(personne$taille, personne$ville, mean)
