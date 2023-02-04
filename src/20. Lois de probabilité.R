#**************************************************************************
#* Lois usuelles                                                   *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************


# Loi Normale N(0, 1) - 10 tirages
rnorm(n = 10, mean = 0, sd = 1)

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

curve(dchisq(x, df = 1), from = 0, to = 50, col = "blue",
      xlab="x value",
      ylab="Density", 
      main="Chi2 Distribution")
curve(dchisq(x, df = 2), from = 0, to = 50, col = "cyan", add = TRUE)
curve(dchisq(x, df = 3), from = 0, to = 50, col = "red", add = TRUE)
curve(dchisq(x, df = 4), from = 0, to = 50, col = "green", add = TRUE)
curve(dchisq(x, df = 10), from = 0, to = 50, col = "purple", add = TRUE)
curve(dchisq(x, df = 20), from = 0, to = 50, col = "orange", add = TRUE)

legend("topright", inset=.05, lty = 1,
       c("1","2","3","4","10","20"), 
       col = c("blue", "cyan", "red", "green", "purple", "orange"),
       title = "Degrees of freedom")


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



