#**************************************************************************
#* Initiation à R avec RStudio                                            *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

# Tips
#   Nouveau fichier RStudio > File > New file > R script
#   Cliquer sur la ligne a executer puis CTRL + ENTREE 
#   Taper le nom de la fonction puis F1 pour acceder a l aide
#   Cliquer dans la console puis CTRL + L pour nettoyer la console

# vider environnement
rm(list=ls())


#-------------------------------------------------------------------
# Installer et utiliser une librairie (package)
#-------------------------------------------------------------------

# Installer un package (à faire une seule fois)
install.packages("dplyr")

# Utiliser le package
library(dplyr)

# Installer un package en choisissant le dossier d'installation
my_lib <- paste0("C:",Sys.getenv("HOMEPATH"), "\\R")
install.packages("ggplot2", lib = my_lib)

# Liste des packages installés
library()


#-------------------------------------------------------------------
# Répertoire de travail
#-------------------------------------------------------------------

# Definir un dossier comme répertoire de travail
setwd("P:/Ludo/Projets/R-tuto")

# Vérifier le répertoire de travail
getwd()


#-------------------------------------------------------------------
# Charger un fichier de données
#-------------------------------------------------------------------

# Jeux de données inclus dans R
#   R dispose de nombreux jeux de données inclus
data()               # pour lister tous les datasets disponibles
data(mtcars)         # pour charger en mémoire un dataset
mtcars


#-------------------------------------------------------------------
# Conditions et boucles
#-------------------------------------------------------------------

# If - Else
condition1 <- TRUE
if (condition1){
  print("condition1 est vraie")
}else{
  print("condition1 est fausse")
}

# Operateur ternaire
condition2 <- FALSE
ifelse(condition2, "Si condition2 est vraie je fais ceci", "Sinon je fais cela")

# For
for (i in 1:5) {
  print(i * 2)
}

# While
countdown <- 10
while(countdown){    # quand countdown = 0 cela equivaut a FALSE
  print(countdown)
  countdown <- countdown - 1
}

# Case
pop <- 500
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





