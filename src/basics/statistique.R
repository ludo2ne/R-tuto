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


# Loi Normale N(0, 1) - 10 tirages
rnorm(10, 0, 1)

# Loi Binomiale B(2, 0,5) - 15 tirages
rbinom(15, 2, 0.5)


# -----------------------------------------------------------------------------
# Affichage des densites
# -----------------------------------------------------------------------------

# Densite d une loi Normale Centree Reduite


# -------------------------------
# Loi Normale
# -------------------------------

# Densite
x <- seq(-5,5, by=0.1)
plot(x = x, 
     y = dnorm(x), 
     type = "l",
     main= "Densité de la loi Normale centrée réduite")

# Fonction de reparition
plot(x = x, 
     y = pnorm(x), 
     type = "l",
     main= "Fonction de répartition de la loi Normale centrée réduite")

# Quantiles (inverse de la fonction de repartition)
x1 <- seq(0,1, by=0.01)
plot(x = x1,
     y = qnorm(x1), 
     xlim = c(0,1),
     type = "l",
     main= "Fonction de répartition de la loi Normale centrée réduite")
abline(h=1.96, col="red")
abline(v=0.975, col="red")
legend("topleft", inset=.05, lty=c(1, 1),
       c("Quantile 0.975 = 1.96"), 
       col=c("red"))

x
qnorm(pnorm(x))


# -------------------------------
# Loi du Chi2
# -------------------------------

curve(dchisq(x, df = 5), from = 0, to = 50, 
      xlab="x value",
      ylab="Density", 
      main="Chi2 Distribution")


# -------------------------------
# Loi de Student
# -------------------------------

plot_student_distribution <- function() {
  x <- seq(-4, 4, length=100)
  hx <- dnorm(x)
  
  df <- c(1, 3, 8, 30)
  colors <- c("red", "blue", "darkgreen", "gold", "black")
  labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
  
  plot(x = x, 
       y = hx, 
       type="l", 
       lty=2, 
       xlab="x value",
       ylab="Density", 
       main="Comparison of Student Distributions")
  for (i in 1:4){
    lines(x, 
          dt(x,df[i]), 
          lwd=2, 
          col=colors[i])
  }
  legend("topright", inset=.05, title="Distributions",
         labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
  
}

plot_student_distribution()


# -------------------------------
# Loi de Fisher
# -------------------------------

curve(df(x, df1 = 8, df2 = 100), from = 0, to = 10, 
      xlab="x value",
      ylab="Density", 
      main="Fisher Distribution")


# -------------------------------
# Loi Beta
# -------------------------------

curve(dbeta(x, 10, 2), 
      from=0, 
      to=1, 
      bty="n", 
      xlab="")



# -----------------------------------------------------------------------------
# Lois discretes
# -----------------------------------------------------------------------------

# -------------------------------
# Loi de Poisson
# -------------------------------

plot(dpois(x=1:50,lambda=3))



