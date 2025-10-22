#-------------------------------------------------------------------------------
# ETAPE 1 : préparation des donées et les installations necessaires
#-------------------------------------------------------------------------------

#library(FactoMineR)
#library(factoextra)
#library(ggcorrplot)
#library(EFAtools)


pays_data = read.table("pays7.txt", header = TRUE, row.names = 1, dec = ".") #définir la première colonne (pays) noms de lignes

print("Dimensions des données :") # Afficher les dimensions
dim(pays7)

attach(pays7) # Attacher les données pour les appeler directement par le nom

#-------------------------------------------------------------------------------
# ETAPE 2 : STATISTIQUES UNIVARIÉES ET BIVARIÉES
#-------------------------------------------------------------------------------

summary(pays7)

# Calcul des mesures de position et de dispersion
min_vars = apply(pays7, 2, min)
q1_vars = apply(pays7, 2, quantile, probs = 0.25)
mediane_vars = apply(pays7, 2, median)
mean_vars = apply(pays7, 2, mean)
q3_vars = apply(pays7, 2, quantile, probs = 0.75)
max_vars = apply(pays7, 2, max)
sd_vars = apply(pays7, 2, sd)
tabCV = sd_vars / mean_vars #(CV = écart-type / moyenne)

# Création d'un tableau récapitulatif (je l'ai mis sous forme de joli tableau dans le doc)
tableau_recap = data.frame(
  Min = round(min_vars, 2),
  Q1 = round(q1_vars, 2),
  Médiane = round(mediane_vars, 2),
  Moyenne = round(mean_vars, 2),
  Q3 = round(q3_vars, 2),
  Max = round(max_vars, 2),
  Ecart_Type = round(sd_vars, 2),
  CV = round(tabCV, 2)
)
print("Tableau récapitulatif des statistiques descriptives :")
print(tableau_recap)

# -------------------- Visualisations Univariées ---------------------

#COMMENTAIRE POUR ISA ET MANU: je sais pas si c'est pertinent de faire des graphiques pour chaque variable...
# ca fait peut etre trop, mais on peut toujours enlever ce qui derange

#histogrammes
par(mfrow=c(3, 3))
hist(mortinfant, freq=FALSE, col="skyblue", main="Mortalité Infantile")
hist(exportations, freq=FALSE, col="pink", main="Exportations de B/S (en % du PIB total)")
hist(sante, freq=FALSE, col="cornflowerblue", main="Dépenses en santé (en % du PIB total)")
hist(importations, freq=FALSE, col="magenta4", main="Importations de biens et services (en % du PIB total)")
hist(revenu, freq=FALSE, col="lightgreen", main="Revenu")
hist(inflation, freq=FALSE, col="blue4", main="Taux d'inflation (en %)")
hist(espvie, freq=FALSE, col="gold", main="Espérance de Vie")
hist(fertilite, freq=FALSE, col="yellow4", main="Nombre moyen d'enfants par femme")
hist(pibh, freq=FALSE, col="lightcoral", main="PIB par Habitant")
par(mfrow=c(1, 1))

#boîtes à moustaches
par(mfrow=c(2, 2))
boxplot(mortinfant, main="Mortalité Infantile", col="skyblue")
points(mean(mortinfant), col="cyan", pch="*", cex=2)
boxplot(pibh, main="PIB par Habitant", col="lightcoral")
points(mean(pibh), col="cyan", pch="*", cex=2)
boxplot(revenu, main="Revenu", col="lightgreen")
points(mean(revenu), col="cyan", pch="*", cex=2)
boxplot(espvie, main="Espérance de Vie", col="gold")
points(mean(espvie), col="cyan", pch="*", cex=2)
par(mfrow=c(1, 1))


# -------------------- Visualisations Bivariées ---------------------
matcor = cor(pays7)
print("Matrice des corrélations (arrondie à 2 décimales) :")
round(matcor, digits = 2)

ggcorrplot(matcor, hc.order = TRUE, type = "lower", lab = TRUE,
           title = "Matrice des corrélations des caractéristiques des pays")
