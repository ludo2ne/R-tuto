#**************************************************************************
#* Les Vecteurs en R                                                      *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

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
mon_vecteur
mon_vecteur[2]
mon_vecteur[2:4]

# plus grand element
max(mon_vecteur)                           # valeur
which(mon_vecteur == max(mon_vecteur))     # position(s)

#Somme des elements
sum(mon_vecteur)

# Elements pairs
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
# Trucs et astuces
#-------------------------------------------------------------------

# Garder une valeur sur 2 (par exemple les index impair)
v <- 1:20
v[c(TRUE, FALSE)]
