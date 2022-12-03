#**************************************************************************
#* Séries Temporelles avec R                                              *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list = ls())

# require(lubridate)


# -----------------------------------------------------------------------------
# Jeux de données integrés a R
# -----------------------------------------------------------------------------

# data()                                             # Jeux de donnees de R base
# data(package = .packages(all.available = TRUE))    # Jeux de donnees de tous les packages installes

data(nottem)
data(AirPassengers)


# -----------------------------------------------------------------------------
# Générer une série temporelle de type Time-séries
# -----------------------------------------------------------------------------

n <- 120               # Nombre d observations
p <- 12                # periode
t <- seq(1,n)          # temps

m_t <- 400 - 0.5 * t - 1/ (2*n) * t^2    # tendance polynomiale deterministe
s_t <- 20 * sin(2 * pi * t / p)          # tendance saisonniere p-periodique
Z_t <- rnorm(n, 0, 10)                   # bruit

X_t <- ts(m_t + s_t + Z_t, start= c(2010,01), end=c(2019,12), frequency = p)
X_t
str(X_t)

plot(X_t)

start(X_t)
end(X_t)
time(X_t)
frequency(X_t)

# Extraire une fenêtre de la série
window(X_t, start= c(2012,01), end=c(2014,12))


# -----------------------------------------------------------------------------
# Quelques vérifications sur la série temporelle
# -----------------------------------------------------------------------------

# Vérification que le bruit est centré
mean(Z_t)

# Visualisation du bruit
plot(Z_t, type ="l")

# On verifie que la saisonnalité est de moyenne nulle sur une période p
total_saisonnalite <- 0
for (i in seq(1, floor((n-p)/p) * p)) {
  total_saisonnalite <- total_saisonnalite + sum(s_t[i:i+p])
}
mean(total_saisonnalite)

# Visualisation de la saisonnalité
plot(s_t, type ="l")


# -----------------------------------------------------------------------------
# Opérateurs
# -----------------------------------------------------------------------------

# Opérateur Backward
lag(X_t, 3)

# Opérateur Forward
lag(X_t, -2)


# Quelques exemples simples
a <- ts(c(1,2,3,4,5,6,7,8,9), start= c(1986,01,01), frequency = 12)
a
lag(a, 2)
a + lag(a, 2)
lag(a, -3)
a + lag(a, -3)


# -----------------------------------------------------------------------------
# Filtre de différenciation
#   transforme les tendances polynomiales en constantes
# -----------------------------------------------------------------------------

plot(X_t)
plot(diff(X_t, differences = 2))

# Soit un polynome de degré 4
p4 <- 10 + 1/2 * t - 1/10 * t^2 + 1/500 * t^3 - 1/2000 * t^4 
p4

# On va appliquer un filtre de différenciation d'ordre p Delta_p avec p allant de 1 a 4
plot(diff(p4, differences = 1), pch=3)
plot(diff(p4, differences = 2), pch=3)
plot(diff(p4, differences = 3), pch=3)
plot(diff(p4, differences = 4), pch=3, ylim = c(-3, 3))    # constante


# -----------------------------------------------------------------------------
# Filtre moyenne mobile
# -----------------------------------------------------------------------------

# Filtre moyenne mobile arithmétique paire d'ordre 12
#   absorbe les saisonnalités de période 12 (paire)
t1 <- (1/12) * filter(X_t, c(0.5, rep(1,times=11), 0.5))
plot(t1, main = "Tendance polynomiale")

# Filtre moyenne mobile arithmetique impaire d ordre 3
#   absorbe les saisonnalités de période 3 (impaire)
s1 <- ((1/3)*filter(X_t - t1,rep(1,times=3)))
plot(s1, main = "Tendance saisonnière")

# Le reste est le bruit
plot(X_t - t1 - s1, main = "Bruit")

plot(X_t - lag(X_t, 12))

s_t_hat <- X_t - m_t_hat
plot(s_t_hat)

# Filtre de différenciation saisonnière d'ordre p=12
plot(diff(s_t, lag = 12), pch = 3, ylim = c (-3, 3))


# -----------------------------------------------------------------------------
# Décomposition de la série temporelle
# -----------------------------------------------------------------------------

ts_components <- decompose(X_t)
ts_components
plot(ts_components)

# Tendance polynomiale 
plot(ts(m_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Tendance polynomiale")
lines(ts_components$trend, col="green")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("green"))


# Tendance saisonnière 
plot(ts(s_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Tendance saisonnière")
lines(ts_components$seasonal, col="red")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("red"))

# Bruit
plot(ts(Z_t, start = c(2010,01,01), end = c(2019,12,01), frequency = p), main = "Bruit")
lines(ts_components$random, col="orange")
legend("topright", inset=.05, lty=c(1, 1), c("Estimé par la fonction decompose"), col = c("orange"))


# -----------------------------------------------------------------------------
# Fonction d'autocorrelation et Autocorrelogramme
# -----------------------------------------------------------------------------

# Voir chap2 p3

# acf d'un bruit blanc
acf(ts_components$random, na.action = na.pass)


# -----------------------------------------------------------------------------
# Génerer des séries temporelles
#   avec la fonction arima.sim
# -----------------------------------------------------------------------------

bruit_blanc <- arima.sim(n = 1000, model = list())
ts.plot(bruit_blanc, main = "Bruit blanc")

ma_1 <- 3 + arima.sim(n = 1000, model = list(ma = c(2)), sd = 2)
ts.plot(ma_1, main = "Processus MA(1)")

ar_2 <- arima.sim(model = list(ar = c(0.2, -0.5)), n = 1000)
ts.plot(ar_2, main = "Processus AR(2)")

arma_4_2 <- arima.sim(model = list(ar = c(0.6, -0.2, 0.3, -0.5), ma = c(0.4, 0.2)), n = 1000)
plot(arma_4_2, main = "Processus ARMA(4,2)")


# install.packages("astsa")
library(astsa)

#SARIMA(0,1,1)x(0,1,1)_12
sarima_011_011_12 <- sarima.sim(d=1, ma=-.4, D=1, sma=-.6, S=12, n=120)
ts.plot(sarima_011_011_12)

# -----------------------------------------------------------------------------
# Modéliser avec un processus AR
#   AR(1) = ARIMA(1, 0, 0)
# -----------------------------------------------------------------------------

AR_1 <- arima(X_t, order = c(1,0,0))
AR_1
AR_1_fit <- X_t - residuals(AR_1)

ts.plot(X_t, main = "Modelisation avec une processsus AR(1)")
points(AR_1_fit, type = "l", col = 2, lty = 2)
legend("topright", inset=.05, lty=2, c("fitted values"), col = c("red"))



# -----------------------------------------------------------------------------
# Modéliser avec un processus MA
#   MA(1) = ARIMA(0, 0, 1)
# -----------------------------------------------------------------------------

MA_1 <- arima(X_t, order = c(0,0,1))
MA_1
MA_1_fit <- X_t - residuals(MA_1)

ts.plot(X_t, main = "Modelisation avec une processsus MA(1)")
points(MA_1_fit, type = "l", col = 2, lty = 2)
legend("topright", inset=.05, lty=2, c("fitted values"), col = c("red"))


# -----------------------------------------------------------------------------
# Processus des innovations
# -----------------------------------------------------------------------------

# chap2 p4
epsilon_t <- X_t - "prediction a partir du passé"


# TODO proprement

fit <- arima(X_t[1:(n-1)], order=c(0,0,2))    # order pour un MA(2)
fit
acf(X_t)
pacf(X_t)
predictions <- predict(fit, n.ahead = 1)


# -----------------------------------------------------------------------------
# function stl
# -----------------------------------------------------------------------------

fit <- stl(X_t, s.window="period")
plot(fit)


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
# Ex 9
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
# Ex 10
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

n <- 1000
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

# Modeliser la série avec la methode ARIMA
fit <- arima(X_t[1:200], order=c(0,0,2))    # order pour un MA(2)
fit

acf(X_t)
pacf(X_t)

predictions <- predict(fit, n.ahead = 100)
predictions
plot(predictions$pred)

plot(c(X_t[1:200], predictions$pred), type = "l")


# -----------------------------------------------------------------------------



arma.res <- rep(0, 16)
arma.res[1] <- arima(arma_2_1, order = c(3, 0, 2))$aic  # fit arma(3,2) and save aic value
arma.res[2] <- arima(arma_2_1, order = c(2, 0, 2))$aic
arma.res[3] <- arima(arma_2_1, order = c(2, 0, 1))$aic
arma.res[4] <- arima(arma_2_1, order = c(1, 0, 2))$aic
arma.res[5] <- arima(arma_2_1, order = c(1, 0, 1))$aic
arma.res[6] <- arima(arma_2_1, order = c(3, 0, 0))$aic
arma.res[7] <- arima(arma_2_1, order = c(2, 0, 0))$aic
arma.res[8] <- arima(arma_2_1, order = c(0, 0, 2))$aic
arma.res[9] <- arima(arma_2_1, order = c(3, 0, 2), include.mean = FALSE)$aic
arma.res[10] <- arima(arma_2_1, order = c(2, 0, 2), include.mean = FALSE)$aic
arma.res[11] <- arima(arma_2_1, order = c(2, 0, 1), include.mean = FALSE)$aic
arma.res[12] <- arima(arma_2_1, order = c(1, 0, 2), include.mean = FALSE)$aic
arma.res[13] <- arima(arma_2_1, order = c(1, 0, 1), include.mean = FALSE)$aic
arma.res[14] <- arima(arma_2_1, order = c(3, 0, 0), include.mean = FALSE)$aic
arma.res[15] <- arima(arma_2_1, order = c(2, 0, 0), include.mean = FALSE)$aic
arma.res[16] <- arima(arma_2_1, order = c(0, 0, 2), include.mean = FALSE)$aic
which(arma.res == min(arma.res))

