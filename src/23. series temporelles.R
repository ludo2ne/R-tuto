#**************************************************************************
#* Séries Temporelles avec R                                              *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list = ls())

require(lubridate)

# -----------------------------------------------------------------------------
# Generer une serie temporelle de type Time-Series
# -----------------------------------------------------------------------------

n <- 120               # Nombre d observations
p <- 12                # periode
t <- seq(1,n)          # temps

m_t <- 400 - 0.5 * t - 1/ (2*n) * t^2    # tendance polynomiale deterministe
s_t <- 20 * sin(2 * pi * t / p)          # tendance saisonniere p-periodique
Z_t <- rnorm(n, 0, 10)                   # bruit

X_t <- ts(m_t + s_t + Z_t, start= c(2010,01,01), end=c(2019,12,01), frequency = p)
X_t

plot(X_t)


# -----------------------------------------------------------------------------
# Operateurs
# -----------------------------------------------------------------------------

# Operateur Backward
lag(X_t, 3)

# Operateur Forward
lag(X_t, -2)


# Quelques exemples simples
a <- ts(c(1,2,3,4,5,6,7,8,9), start= c(1986,01,01), frequency = 12)
a
lag(a, 2)
a + lag(a, 2)
lag(a, -3)
a + lag(a, -3)


# Filtre moyenne mobile arithmetique paire d ordre 12
#   absorbe les saisonalites de periode 12
t1 <- (1/12) * filter(X_t, c(0.5, rep(1,times=11), 0.5))
plot(t1, main = "Tendance polynomiale")

# Filtre moyenne mobile arithmetique impaire d ordre 3
#   absorbe les saisonalites de periode 3
s1 <- ((1/3)*filter(X_t - t1,rep(1,times=3)))
plot(s1, main = "Tendance saisonnière")

# Le reste est le bruit
plot(X_t - t1 - s1, main = "Bruit")

plot(X_t - lag(X_t, 12))


# -----------------------------------------------------------------------------
# Decomposition de la serie temporelle
# -----------------------------------------------------------------------------

ts_components <- decompose(X_t)
ts_components
plot(ts_components)

# Tendance polynomiale 
plot(ts(m_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Tendance polynomiale")
lines(ts_components$trend, col="green")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("green"))


# Tendance saisonniere 
plot(ts(s_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Tendance saisonnière")
lines(ts_components$seasonal, col="red")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("red"))

# Bruit
plot(ts(Z_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Bruit")
lines(ts_components$random, col="orange")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("orange"))


# -----------------------------------------------------------------------------
# Autocorrelogramme
# -----------------------------------------------------------------------------

# acf d un bruit blanc
acf(ts_components$random, na.action = na.pass)




# -----------------------------------------------------------------------------
# function fit
# -----------------------------------------------------------------------------


fit <- stl(X_t, s.window="period")
plot(fit)


# -----------------------------------------------------------------------------
# Quelques verifications sur la serie temporelle
# -----------------------------------------------------------------------------

# Verification que le bruit est centre
mean(Z_t)

# Visualisation du bruit
plot(Z_t, type ="l")

# On verifie que la saisonnalite est de moyenne nulle sur une periode p
total_saisonnalite <- 0
for (i in seq(1, floor((n-p)/p) * p)) {
  total_saisonnalite <- total_saisonnalite + sum(s_t[i:i+p])
}
mean(total_saisonnalite)

# Visualisation de la saisonnalite
plot(s_t, type ="l")



# -----------------------------------------------------------------------------
# Moyenne mobile
# -----------------------------------------------------------------------------

# Moyenne mobile
#   Filtre lineaire dont les coefficients s annulent a partir d un certain rang
#   Somme pour k allant -m1 a m2 de simga_k * B^(-k)
# Moyenne mobile centree
#   si m1 = m2
# Moyenne mobile symetrique
#   Si quelque soit k sigma_k = sigma_(-k)
# Ordre = m1 + m2 + 1
#


# install.packages("rmarkdown")


# -----------------------------------------------------------------------------
# Processus MA(1)
#   Moyenne mobile d ordre 1
# -----------------------------------------------------------------------------

n <- 1000
epsilon <- ts(rnorm(n, mean=0, sd=1))        # Bruit blanc

head(epsilon)
head(lag(epsilon) + epsilon)

ma_1 <- 3 + lag(epsilon) + 2 * epsilon

plot(ma_1, type="l", col = "red", main = "Processus MA(1)")

# Fonction d autocorrelation
acf(ma_1, type="correlation")
# Le pic a l index 0 vaut 1 ce qui est toujours le cas
# index du dernier pic significatif : q=1 => MA(1)

# Fonction d autocorrelation partielle empirique
pacf(ma_1)
# decroissante exponentielle : AR : p=0

# -----------------------------------------------------------------------------
# Processus MA(2)
#   Moyenne mobile d ordre 2
# -----------------------------------------------------------------------------

eta <- ts(rnorm(n, mean=0, sd=sqrt(2)))    # Bruit blanc

ma_2 <- eta - 1/2 * lag(eta, 1) + 1/3 * lag(eta, 2)


plot(ma_2, type="l", col = "blue", main = "Processus MA(2)")

acf(ma_2, type="correlation")
# Le pic a l index 0 vaut 1 ce qui est toujours le cas
# index du dernier pic significatif : q=2 => MA(2)
pacf(ma_2)
# decroissante exponentielle : AR : p=0

acf(ma_1 + ma_2)
pacf(ma_1 + ma_2)


# -----------------------------------------------------------------------------
# Processus AR(1)
#   Autoregressif d ordre 1
# -----------------------------------------------------------------------------

generate_ar1 <- function(n, mu, phi, sigma2){
  epsilon <- ts(rnorm(n, mean=0, sd=sqrt(sigma2)))
  ar_1 <- ts(rep(0,n))
  ar_1[1] <- mu + epsilon[1]
  
  for(t in 2:n){
    ar_1[t] = mu + phi * ar_1[t-1] + epsilon[t]
  }
  
  # Verification
  print(ar_1[2] == mu + phi * ar_1[1] + epsilon[2])
  print(ar_1[3] == mu + phi * ar_1[2] + epsilon[3])
  print(ar_1[4] == mu + phi * ar_1[3] + epsilon[4])
  
  return (ar_1)
}

ar_1 <- generate_ar1(n, 1, 0.45, 0.5)

plot(ar_1, col="blue")

acf(ar_1, main = "ACF")                    # decroissance exponentielle
pacf(ar_1, main = "PACF")                  # decroissance rapide (1 pic)


# Phi = 0.95
ar_1_bis <- generate_ar1(n, 1, 0.95, 0.5)
plot(ar_1_bis, col="blue")

acf(ar_1_bis, main = "ACF")               # decroissance exponentielle 
pacf(ar_1_bis, main = "PACF")             # decroissance rapide (2 pics)


# Phi = 1
ar_1_ter <- generate_ar1(n, 1, 1, 0.5)
plot(ar_1_ter, col="blue")

acf(ar_1_ter, main = "ACF")               # Non stationnaire
pacf(ar_1_ter, main = "PACF")



# ---------------------------------------------------------------
# Ex 11
# ---------------------------------------------------------------
n <- 300

epsilon <- ts(rnorm(n+2, mean=0, sd=1))
X_t <- ts(3 + epsilon + 1/2 * lag(epsilon, 1) - 1/3 * lag(epsilon, 2))
X_t
plot(X_t)

# Modeliser la serie avec la methode ARIMA
fit <- arima(X_t[1:200], order=c(0,0,2))    # order pour un MA(2)
fit

acf(X_t)
pacf(X_t)

predictions <- predict(fit, n.ahead = 100)
predictions
plot(predictions$pred)

plot(c(X_t[1:200], predictions$pred), type = "l")
