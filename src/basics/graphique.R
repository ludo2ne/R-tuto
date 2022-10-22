#**************************************************************************
#* Tracer des Graphiques en R                                             *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

#-------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------

rm(list=ls())

library(ggplot2)

setwd("P:/Ludo/Tuto/R-tuto")

# Lire un fichier csv
personne <- read.csv2("data/personne.csv", 
                      sep = ";", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

plot(x = personne$taille, 
     y = personne$pointure, 
     main = "pointure en fonction de la taille")

boxplot(personne$taille ~ personne$sexe)

# Diagramme en baton
barplot(table(personne$ville), col = c("lightblue", "lightgreen", "pink"))

# ggplot
ggplot(data = personne,
       mapping = aes(x = dnais, y = taille)) + 
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
