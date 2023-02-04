#**************************************************************************
#* Initiation � R avec RStudio                                            *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************


#-------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------

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

# V�rifier le r�pertoire de travail
getwd()

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
  pop > 100000 ~ "Grande m�tropole",
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




