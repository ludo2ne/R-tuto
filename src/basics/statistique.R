#**************************************************************************
#* Statistiques avec R                                                    *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

#-------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------

rm(list=ls())

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
sd(personne$taille)

# Moyenne des tailles par ville
tapply(personne$taille, personne$ville, mean)


# Loi Normale N(0, 1) - 10 tirages
rnorm(10, 0, 1)

# Loi Binomiale B(2, 0,5) - 15 tirages
rbinom(15, 2, 0.5)

# Regression
reg <- lm(data = personne, taille ~ pointure)
coeff <- coefficients(reg)
eq <- paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
plot(x = personne$pointure, y = personne$taille)
abline(reg = reg)                        # Droite de regression
text(x = 41, y = 190, labels = eq)       # texte centre en x, y

# Coefficient de correlation
cor(personne$pointure, personne$taille)