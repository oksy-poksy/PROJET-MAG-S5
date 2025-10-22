#-------------------------------------------------------------------------------
# ETAPE 1 : préparation des donées et les installations necessaires
#-------------------------------------------------------------------------------

#balh blah

library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(EFAtools)


# Utilisation de 'row.names=1' pour définir la première colonne (pays) comme noms de lignes
# (comme dans l'exemple codeTP1_25.R : row.names=1)
# Utilisation de 'dec="."' car le fichier utilise le point pour les décimales
pays_data = read.table("pays7.txt", header = TRUE, row.names = 1, dec = ".")

# Afficher les dimensions et les premières lignes pour vérification
print("Dimensions des données :")
dim(pays_data)
print("Premières lignes :")
head(pays_data)

# Vérifier et gérer les valeurs manquantes (NA)
# Le fichier 'pays7.txt' importé peut contenir des NA (par exemple, pour Kazakhstan)
pays_data_clean = na.omit(pays_data)
print("Nombre de pays après suppression des lignes avec NA :")
dim(pays_data_clean) # S'il y a des NA, le nombre de lignes sera < 50

# Attacher les données pour pouvoir appeler les variables directement par leur nom
# (comme dans l'exemple codeTP1_25.R)
attach(pays_data_clean)

# Sauvegarder les données nettoyées pour l'analyse
data_analyser <- pays_data_clean
