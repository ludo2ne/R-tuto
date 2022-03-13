# Tips
#   Nouveau fichier RStudio > File > New file > R script
#   Cliquer sur la ligne a executer puis CTRL + ENTREE 
#   Taper le nom de la fonction puis F1 pour acceder a l aide
#   Cliquer dans la console puis CTRL + L pour nettoyer la console

# vider environnement
rm(list=ls())

library(dplyr)
library(ggplot2) 

# Definir un dossier comme repertoire de travail
setwd("P:/Ludo/Cours/UE2 Introduction au langage R/tuto R")

#-------------------------------------------------------------------
# Vecteurs
#-------------------------------------------------------------------
# le vecteur contient des elements qui sont tous du meme type

# creer
mon_vecteur <- c(8, 12, 19, 20, 9)
mon_vecteur_sequence <- seq(1, 9, by=2)
mon_vecteur_repetition <- rep(1:3, 2)
mon_2e_vecteur_repetition <- rep(1:3, each=2)

# Comparer 2 vecteurs element par element
mon_vecteur_repetition == mon_2e_vecteur_repetition

# supprimer un objet
rm(mon_2e_vecteur_repetition)

# type et nombre d elements contenus
typeof(mon_vecteur)
length(mon_vecteur)

# afficher
mon_vecteur      # ou print(mon_vecteur) mais le print est facultatif
mon_vecteur[2]
mon_vecteur[2:4]

# plus grand element
max(mon_vecteur)                           # valeur
which(mon_vecteur == max(mon_vecteur))     # position(s)

#Somme des elements
sum(mon_vecteur)

# Elements pairs
mon_vecteur[which(mon_vecteur %% 2 == 0)]
mon_vecteur[mon_vecteur %% 2 == 0]

# Combiner en ligne ou en colonne
rbind(mon_vecteur,mon_vecteur_sequence)
cbind(mon_vecteur,mon_vecteur_sequence)


# indicateurs statistiques (moyenne, min, max)
summary(mon_vecteur)

#-------------------------------------------------------------------
# Vecteurs et Booleens
#-------------------------------------------------------------------
#  &  ET
#  |  OU
#  !  NON
#  == EGAL
#  != NON EGAL

# creer un vecteur de booleens qui pour chaque element verifie si la condition est vraie ou fausse
mon_vecteur_booleen <- mon_vecteur > 10
mon_2e_vecteur_booleen <- mon_vecteur > 10 & mon_vecteur < 20

# Test si valeur presente dans le vecteur
8 %in% mon_vecteur

!mon_vecteur_booleen      # complementaire
any(mon_vecteur_booleen)  # x1 OR x2 OR ... OR xN
all(mon_vecteur_booleen)  # x1 AND x2 AND ... AND xN


#-------------------------------------------------------------------
# Matrices
#-------------------------------------------------------------------

ma_matrice <- matrix(seq(1:9),nrow = 3,ncol = 3)
ma_matrice_diagonale <- diag(c(1, 5, 9), nrow=3, ncol=3)    # Matrice 3x3 avec les elements du vecteur en param 1 sur la diagonale

dim(ma_matrice)

# Produit matriciel
ma_matrice %*% ma_matrice_diagonale

# Transposee
ma_matrice_transposee <- t(ma_matrice)


#-------------------------------------------------------------------
# Dataframe
#-------------------------------------------------------------------
#   on peut visualiser cela comme un tableau avec des lignes (donnees) et des colonnes (variables)

# Lire un fichier csv
personne <- read.csv2("personne.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Ecrire dans un fichier
write.csv2(personne,"personne_bis.csv")

# Quelques infos de base sur le dataframe
str(personne)
summary(personne$pointure)

colnames(personne)
rownames(personne)

#Renommer une colonne
colnames(personne)[colnames(personne) == "dnais"] <- "naissance"

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
arrange(personne, desc(taille))   #dplyr

# Filter les personnes dont la taille est superieure a la moyenne
personne[personne$taille > mean(personne$taille),]
filter(personne, taille > mean(taille))


#-------------------------------------------------------------------
# Stats
#-------------------------------------------------------------------

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
sd(personne$taille)

# Moyenne des tailles par ville
tapply(personne$taille, personne$ville, mean)


# Loi Normale N(0, 1) - 10 tirages
rnorm(10, 0, 1)

# Loi Binomiale B(2, 0,5) - 15 tirages
rbinom(15, 2, 0.5)

# Regression
reg <- lm(data = personne, taille ~ pointure)
coeff <- coefficients(reg)
eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
plot(x = personne$pointure, y = personne$taille)
abline(reg = reg)                        # Droite de regression
text(x = 41, y = 190, labels = eq)       # texte centre en x, y

# Coefficient de correlation
cor(personne$pointure, personne$taille)

#-------------------------------------------------------------------
# Graphique
#-------------------------------------------------------------------

plot(x = personne$taille, 
     y = personne$pointure, 
     main = "pointure en fonction de la taille")

boxplot(personne$taille ~ personne$sexe)

# Diagramme en baton
barplot(table(personne$ville), col = c("lightblue", "lightgreen", "pink"))

# ggplot
ggplot(data = personne,
       mapping = aes(x = naissance, y = taille)) + 
  geom_line() +
  labs(title = "Taille en fonction de la date de naissance", 
       caption = "Source : stats du dimanche")

ggplot(data = personne,
       mapping = aes(x = pointure, y = taille)) + 
  geom_point() +
  labs(title = "Pointure en fonction de la taille") +
  geom_point(data = personne %>%
               filter(taille == 175, pointure == 38), 
             colour = "red",
             shape = 0,
             size = 5, 
             stroke = 3) +
  geom_label(
    label="Point important", 
    x = 38,
    y = 180
  ) +
  geom_abline(intercept = coeff[1], slope = coeff[2])

#-------------------------------------------------------------------
# Conditions et boucles
#-------------------------------------------------------------------

# If - Else
condition1 = TRUE
if (condition1){
  print("condition1 est vraie")
}else{
  print("condition1 est fausse")
}

# Operateur ternaire
condition2 = FALSE
ifelse(condition2, "Si condition2 est vraie je fais ceci", "Sinon je fais cela")

# For
for (i in 1:5) {
  print(personne[i,]$prenom)
}

# While
countdown = 10
while(countdown){    #quand countdown = 0 cela equivaut a FALSE
  print(countdown)
  countdown <- countdown - 1
}

# Case
pop = 500
case_when(
  pop > 100000 ~ "Grande métropole",
  pop > 10000  ~ "Grande ville",
  pop > 2000   ~ "Ville",
  TRUE         ~ "Village"
)

#-------------------------------------------------------------------
# Fonctions
#-------------------------------------------------------------------

# Avec des valeurs par defaut pour les parametres
addition1 <- function(a = 3, b = 6) {
  return <- a + b
}

print(addition1(5,))
