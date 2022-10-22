rm(list=ls())

setwd("P:/Ludo/Tuto/R-tuto")

aeroports <- read.csv(file = "data/aeroports.csv",
                      row.names = 1, 
                      header= TRUE,
                      sep=";")

row.names(aeroports) <- aeroports$Ville