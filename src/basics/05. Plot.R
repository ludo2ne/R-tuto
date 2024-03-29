#**************************************************************************
#* Tracer des Graphiques en R                                             *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")

#-------------------------------------------------------------------
# Ressources
#-------------------------------------------------------------------

# https://r-coder.com/plot-r/


#-------------------------------------------------------------------
# Import de donn�es
#-------------------------------------------------------------------

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
     ylab = "Pointure",                             # Titre axe des ordonn�es
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


curve(pnorm(x, 0, 1), col = "red", , from = -5, to = 5)
points(c(2, 4), c(0.6, 0.8), pch = 3, col = "green")
legend("left", inset=.05, lty=c(1, NA), pch = c(NA, 3), 
       c("Fonction r�partition", "des points"), 
       col=c("red", "green"))

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
  geom_point() +
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
  )




#-------------------------------------------------------------------
# Visualiser deux variables qualitatives
#-------------------------------------------------------------------

x1<-sample(c("Lower","Middle","Upper"),20,replace=TRUE)
x2<-sample(c("Male","Female"),20,replace=TRUE)
df<-data.frame(x1,x2)
mosaicplot(x2~x1,data=df,col=c("Blue","Red"))
