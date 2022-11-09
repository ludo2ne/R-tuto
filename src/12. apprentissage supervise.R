#**************************************************************************
#* Apprentissage supervis� avec R                                         *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************

rm(list = ls())

# -----------------------------------------------------------------------------
# Bonus
# -----------------------------------------------------------------------------

notes_2022 <- c(4.5, 5, 6, 7, 7, rep(7.5,5), rep(8,3), 8.5, rep(9,4), rep(9.5,2))
notes_2022 <- c(notes_2022, rep(10,7), rep(10.5,6), rep(11,8), rep(11.5, 9), rep(12,7), rep(12.5,10))
notes_2022 <- c(notes_2022, rep(13,14), rep(13.5,16), rep(14,12), rep(14.5, 8), rep(15,12), rep(15.5,9))
notes_2022 <- c(notes_2022, rep(16,8), rep(16.5,8), rep(17,8), rep(17.5,5), rep(18,4), 18.5)

moyenne_notes_2022 <- mean(notes_2022)

hist(notes_2022, 
        main="Distribution 2022 des notes d'apprentissage supervis�",
        xlab="Notes 2022",
        xlim=c(0,20),
        col="lightblue", 
        breaks = length(table(notes_2022)))
abline(v=moyenne_notes_2022, col="red")


# -----------------------------------------------------------------------------
# TODO
# -----------------------------------------------------------------------------


rm(list = ls())

library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Import des donnees spambase
# -----------------------------------------------------------------------------

# library(nutshell) 
# install.packages("nutshell")
# data(spambase, package = "nutshell")

# ou sinon si pb pour installer le package nutshell


setwd("P:/Ludo/Cours2A/UE2 Apprentissage supervisé/TP/TP3")

spambase <- read.csv(file = "spambase.csv", 
                 stringsAsFactors = TRUE, 
                 sep=",")

summary(spambase)
str(spambase)

# transformation de la variable is_spam en factor
spambase$is_spam <- as.factor(spambase$is_spam)

# Enlever la colonne X
spambase <- spambase[, !names(spambase) == "X"]


train_size <- floor(nrow(spambase) * 0.75)         # taille de l echantillon d apprentissage 75 pourcent

# Figer l aleatoire 
set.seed(111)

# Separation des donnees d apprentissage et des donnees de test
#   important de choisir train et test au hasard et surtout pas prendre les 900 premieres lignes pour l un
train <- spambase[sample(x=nrow(spambase), size=train_size), ] # extrait un echantillon de train_size valeurs
test <- setdiff(spambase, train)                               # le reste va dans train

rm(train_size)



# -----------------------------------------------------------------------------
# KNN
# -----------------------------------------------------------------------------

library("caret")    # ensemble de meta-fonctions necessaires au processus d'apprentissage supervise
library("class")    # fonction knn
library("e1071")    # librairie necessaire pour la fonction tune.knn

# -------------------------------
# Trouver le meilleur k
#   par cross validation
# -------------------------------

knn_cross_results <- tune.knn(
  x = train[ , !names(train) %in% c("is_spam")],    # predicteurs (toutes les colonnes sauf is_spam)
  y = train$is_spam,                                # reponse
  k = 1:20,                                         # essayer knn avec K variant de 1 a 20
  tunecontrol = tune.control(sampling = "cross"),   # utilisation de la cross validation
  cross = 5                                         # 5 blocs
)

ggplot(
  data = knn_cross_results$performances,
  mapping = aes(x = k, y = error)
) + 
  geom_line() +
  geom_point() + 
  labs(
    x = "k",
    y = "Erreur de validation",
    title = "Evolution de l'erreur selon k"
  )

# Meilleur k trouve
knn_cross_results$best.parameters$k

# Predictions sur les donnees de test avec l algo KNN
knn_pred <- knn(
  train = as.matrix(train[ , !names(train) %in% c("is_spam")]),      # données d'apprentissage
  test = as.matrix(test[ , !names(test) %in% c("is_spam")]),         # données à prédire
  cl = train$is_spam,                                                # vraies valeurs du jeu d apprentissage
  k = knn_cross_results$best.parameters$k                            # nombre de voisins optimal
)

# Nombre de bonnes predictions
sum(knn_pred == test$is_spam)

# Taux de bonnes predictions
sum(knn_pred == test$is_spam) / nrow(test)

# Taux d erreur
1 - sum(knn_pred == test$is_spam) / nrow(test)

# Matrice de confusion 
table(knn_pred, test$is_spam)


# -----------------------------------------------------------------------------
# Baysien naif
# -----------------------------------------------------------------------------

# On defint les hyper parametres a tester

hyperparam_grid <- expand.grid(
   usekernel = TRUE, # si vrai utilisation d'un noyau sinon gaussien
   fL = 0, # correction avec lissage de Laplace (ici ce paramètre n'est pas nécessaire, x étant continue)
   adjust = seq(1, 5, by = 1) # largeur de bande
 )
hyperparam_grid <- rbind(hyperparam_grid, data.frame(usekernel = FALSE, 
                                                     fL = 0, 
                                                     adjust = 1))

# Definir la methode de validation, ici cross validation avec 5 blocs
control <- trainControl(method = "cv", number = 5)

# On optimise les hyper parametres du modele
nb_test_hp <- train(
  x = train[ , !names(train) %in% c("is_spam")],    # predicteurs
  y = train$is_spam,                                # vraie valeur
  method = "nb",                                    # classifieur utilise, ici Naive Bayes
  trControl = control,                              # methode d'échantillonage, ici 5-fold CV
  tuneGrid = hyperparam_grid                        # liste des hyper paramètres a comparer
)

# visualisation des resultats
plot(nb_test_hp)


library("klaR") # fonction Naivebayes utilisee avec la librairie caret

# Apprentissage du modele
naive_bayes_model <- NaiveBayes(
  formula = is_spam ~ .,
  data = train,
  usekernel = FALSE,
  fL = 0
)

# Prediction sur les donnees de test
naive_bayes_pred <- predict(
  object = naive_bayes_model,
  newdata = test[ , !names(test) %in% c("is_spam")]
)

# Matrice de confusion et taux d erreur
table(naive_bayes_pred$class, test$is_spam)

# Taux de bonnes predictions
sum(naive_bayes_pred$class == test$is_spam) / nrow(test)

# Taux d erreurs
1 - sum(naive_bayes_pred$class == test$is_spam) / nrow(test)



# -----------------------------------------------------------------------------
# Arbre CART
# -----------------------------------------------------------------------------

library("rpart")      # modele CART
library("rpart.plot") # visualisation du modèle CART

# Entrainement du modele
cart_model <- rpart(
  formula = is_spam ~ .,            # is_spam en fonction de toutes les autres variables
  data = train,
  method = "class",                 # class pour Classification, ANOVA pour regression
  parms = list(split = 'gini')
)

# Affichage de l arbre
rpart.plot(cart_model)

# Prediction des nouvelles valeurs sur le test set
cart_pred <- predict(cart_model, test, type = "class")

# Matrice de confusion, taux de bonnes precisions et taux d erreur
table(cart_pred, test$is_spam)

# Taux de bonnes predictions
sum(cart_pred == test$is_spam) / nrow(test)

# Taux d erreurs
sum(cart_pred != test$is_spam) / nrow(test)


# -----------------------------------------------------------------------------
# Analyse Discriminante
# -----------------------------------------------------------------------------

# Construction du modele
lda <- lda(is_spam ~ ., 
           data = train)

# Prediction sur les donnees de test
lda_pred <- predict(lda, test)

# Matrice de confusion, taux de bonnes precisions et taux d erreur
table(lda_pred$class, test$is_spam)

# Taux de bonnes predictions
sum(lda_pred$class == test$is_spam) / nrow(test)

# Taux d erreurs
sum(lda_pred$class != test$is_spam) / nrow(test)



# -----------------------------------------------------------------------------
# Graph ROC et LIFT de l arbre CART
# -----------------------------------------------------------------------------

library("ROCR")
# install.packages("ROCR")

# Calcul des scores de prediction
score_cart <- predict(cart_model, test, type = "prob")[, 2 ]

# Generer les infos pour le graph ROC
graph_roc_cart <- prediction(score_cart, test$is_spam)

# Construction du ROC TVP (tpr) en fonction de TFP (fpr)
roc_cart <- performance(graph_roc_cart, "tpr", "fpr")

# Affichage du ROC
plot(roc_cart, main = "ROC de l'arbre CART")


# Construction du LIFT TVP (sens) en fonction de TP (rpp)
lift_cart <- performance(graph_roc_cart, "sens", "rpp") 
plot(lift_cart)

AUC <- performance(graph_roc_cart, "auc")
AUC@y.values


# -----------------------------------------------------------------------------
# Comparaison de methodes
# -----------------------------------------------------------------------------

# ROC du Baysien naif
score_nb <- predict(naive_bayes_model, test, type = "prob")$posterior[, 2 ]
graph_roc_nb <- prediction(score_nb, test$is_spam)
roc_nb <- performance(graph_roc_nb, "tpr", "fpr")
plot(roc_nb, main = "ROC du Baysien Naïf")

# ROC de l analyse discriminante
score_lda <- predict(lda, test, type = "prob")$posterior[, 2 ]
graph_roc_lda <- prediction(score_lda, test$is_spam)
roc_lda <- performance(graph_roc_lda, "tpr", "fpr")
plot(roc_lda, main = "ROC de l'analyse discriminante")


# Construction des data.frames necessaires à l affichage des courbes ROC
roc_lda <- data.frame(
  "fpr" = roc_lda@x.values[[1]],
  "tpr" = roc_lda@y.values[[1]],
  "seuil" = roc_lda@alpha.values[[1]]
)
roc_cart <- data.frame(
  "fpr" = roc_cart@x.values[[1]],
  "tpr" = roc_cart@y.values[[1]],
  "seuil" = roc_cart@alpha.values[[1]]
)
roc_nb <- data.frame(
  "fpr" = roc_nb@x.values[[1]],
  "tpr" = roc_nb@y.values[[1]],
  "seuil" = roc_nb@alpha.values[[1]]
)


# Courbes ROC des 3 modeles
ggplot(
  data = roc_lda,
  mapping = aes(x = fpr, y = tpr)
) +
  # courbe du modele LDA
  geom_line(
    mapping = aes(color = "LDA")
  ) +
  # courbe du modele cart
  geom_line(
    data = roc_cart,
    mapping = aes(x = fpr, y = tpr, color = "CART")
  ) +
  # courbe du modele NB
  geom_line(
    data = roc_nb,
    mapping = aes(x = fpr, y = tpr, color = "Naive Bayes")
  ) +
  # ajout du modele aléatoire
  geom_segment(
    mapping = aes(x = 0, y = 0, xend = 1, yend = 1),
    linetype = "dashed",
    size = .2
  ) +
  # ajout du meilleur modele
  geom_segment(
    mapping = aes(x = 0, y = 0, xend = 0, yend = 1),
    linetype = "dashed",
    size = .2
  ) +
  geom_segment(
    mapping = aes(x = 0, y = 1, xend = 1, yend = 1),
    linetype = "dashed",
    size = .2
  ) +
  # valeurs des axes en pourcentage
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # definition des couleurs des courbes
  scale_colour_manual(
    values = c("CART" = "#1D88E5", "Naive Bayes" = "#FFC108", "LDA" = "#D81A60")
  ) +
  # définition des noms des axes, légende et titre
  labs(
    x = "Taux de faux positifs",
    y = "Taux de vrais positifs",
    title = "Courbes ROC",
    colour = "Modèle"
  ) +
  # position de la legende sur le graphe et non pas a cote
  theme(
    legend.position = c(.8, .3)
  )


