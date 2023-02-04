#**************************************************************************
#* Tracer des Graphiques en R                                             *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

#-------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------

rm(list=ls())


setwd("P:/Ludo/Tuto/R-tuto")

# Lire un fichier csv
personne <- read.csv2("data/personne.csv", 
                      sep = ";", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

#-------------------------------------------------------------------
# Nuage de points
#-------------------------------------------------------------------

plot(x = personne$taille, 
     y = personne$pointure, 
     xlab = "Taille",                               # Titre axe des abcisses
     ylab = "Pointure",                             # Titre axe des ordonnées
     main = "Pointure en fonction de la taille",    # Titre du graphique
     pch = 3,                                       # Type de point
     xlim = c(140, 210),                            # Valeur min et max
     col = "blue")

# Ajouter des points
points(x = 175, y = 46, col = "red")


#-------------------------------------------------------------------
# Courbe
#-------------------------------------------------------------------

n <- 1000
Y <- sort(rnorm(n, 0, 1))

plot(Y,
     type = "l",                 # Type ligne
     lwd = 5                     # Epaisseur de la ligne 
)

# Tracer une ligne
lines(c(0, n), c(0, 0), col = "red")

# Tracer des polygones
zone <- seq(0, n-1, 1)
polygon(x = c(zone, zone[n]), 
        y = c(Y, Y[1]), 
        col = "green")

#-------------------------------------------------------------------
# Boxplot
#-------------------------------------------------------------------

boxplot(personne$taille ~ personne$sexe)

# Diagramme en baton
barplot(table(personne$ville), col = c("lightblue", "lightgreen", "pink"))


#-------------------------------------------------------------------
# ggplot
#-------------------------------------------------------------------

library(ggplot2)

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




#-------------------------------------------------------------------
# Visualiser deux variables qualitatives
#-------------------------------------------------------------------

x1<-sample(c("Lower","Middle","Upper"),20,replace=TRUE)
x2<-sample(c("Male","Female"),20,replace=TRUE)
df<-data.frame(x1,x2)
mosaicplot(x2~x1,data=df,col=c("Blue","Red"))