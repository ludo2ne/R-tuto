#**************************************************************************
#* Les formats de données R                                               *
#* Ludovic Deneuville                                                     *
#* ENSAI 2023                                                             *
#**************************************************************************



# -------------------------------------------------------------------
# Chaines de caractères
# -------------------------------------------------------------------

s <- "on attend pas Patrick"

str(s)
class(s)

# Nombre de caractères
nchar(s)

# Séparer les mots (et stocker dans une liste)
strsplit(s, split = " ")

# Concaténer
paste("oublie", "que", "t'as", "aucune", "chance")

# Extraire
substr(s, 4, 9)


# -------------------------------------------------------------------
# Date
# -------------------------------------------------------------------

# Créer un objet date
d <- as.Date("15/02/2016", format = "%d/%m/%Y"); d

str(d)
class(d)

today <- Sys.Date(); today

# Comparaisons entre dates
between(d, 
        left = as.Date(ISOdate(1986, 1, 1)),
        right = today)

# Opérations
today - 10

# Extraire le jour
as.numeric(format(d, "%d"))
format(d, "%A")
as.numeric(format(d, "%j"))     # jour de l'année

# Extraire le mois
as.numeric(format(d, "%m"))
format(d, "%B")

# Extraire l'année
as.numeric(format(d, "%Y"))


# Afficher au format local
format(d, "%x")


