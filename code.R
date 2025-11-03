#-------------------------------------------------------------------------------
# ETAPE 1 : préparation des donées et les installations necessaires
#-------------------------------------------------------------------------------

library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(EFAtools)
library(viridis)
library(ggrepel)


pays7 = read.table("pays7.txt", header = TRUE, row.names = 1, dec = ".") #définir la première colonne (pays) noms de lignes

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
hist(fertilite, freq=FALSE, col="plum", main="Nombre moyen d'enfants par femme")
hist(pibh, freq=FALSE, col="lightcoral", main="PIB par Habitant")
par(mfrow=c(1, 1))

#boîtes à moustaches
par(mfrow=c(3, 3))

boxplot(mortinfant, main="1. Mortalité Infantile",col="skyblue")
points(mean(mortinfant), col="red", pch="*", cex=2)

boxplot(exportations,main="2. Exportations", col="pink")
points(mean(exportations), col="red", pch="*", cex=2)


boxplot(sante, main="3. Dépenses de Santé", col="cornflowerblue")
points(mean(sante), col="red", pch="*", cex=2)


boxplot(importations, main="4. Importations", col="magenta4")
points(mean(importations), col="red", pch="*", cex=2)


boxplot(revenu, main="5. Revenu", col="lightgreen")
points(mean(revenu), col="red", pch="*", cex=2)

boxplot(inflation, main="6. Taux d'Inflation", col="blue4")
points(mean(inflation), col="red", pch="*", cex=2)

boxplot(espvie,main="7. Espérance de Vie", col="gold")
points(mean(espvie), col="red", pch="*", cex=2)

boxplot(fertilite, main="8. Taux de Fertilité", col="plum")
points(mean(fertilite), col="red", pch="*", cex=2)

boxplot(pibh,  main="9. PIB par Habitant", col="lightcoral")
points(mean(pibh), col="red", pch="*", cex=2)

par(mfrow=c(1, 1))


# -------------------- Visualisations Bivariées ---------------------
matcor = cor(pays7)
print("Matrice des corrélations (arrondie à 2 décimales) :")
round(matcor, digits = 2)

ggcorrplot(matcor, hc.order = TRUE, type = "lower", lab = TRUE,
           title = "Matrice des corrélations des caractéristiques des pays")


#-------------------------------------------------------------------------------
# ETAPE 3 : ACP
#-------------------------------------------------------------------------------


#JUSTIFICATION DE L'ACP
tabcor=cor(pays7)

plot(pays7,main="Matrice des nuages de points",col= viridis(3,option="G",direction=-1));

ggcorrplot(tabcor, type ="lower",lab = TRUE, lab_col = "white", col=viridis(3,option="G",direction=-1),outline.color="white",ggtheme="theme_bw")


ggplot(pays7, aes(x=fertilite,y=mortinfant)) +
  geom_jitter(size=2, col=viridis(1,option="A"), shape=1) +
  geom_text_repel(label=rownames(pays7))


# Indice KMO ___________________________________________________________________

#library(EFAtools)
indice_KMO = KMO(tabcor)
print("Indice KMO global :")
print(indice_KMO)

# REALISATION DE L'ACP _________________________________________________________

#library(FactoMineR)
#Choisir le nombre de composantes principales en arbitrant entre les differents criteres vus en cours,

resuacp=PCA(pays7, graph = FALSE)
acp_data=resuacp$eig

#CHOIX DES COMPOSTANTES (Critères graphiques) _________________________________________________________

print("Valeurs propres :")
print(acp_data)

# (Critère de Kaiser et du coude)
par(mfrow=c(1, 1))
barplot(acp_data[,1],main ="Graphique des valeurs propres", col="#DE3163")
abline(h=1, col="blue", lwd=2) # Ligne de référence pour Kaiser (Val. Propre = 1)

plot(resuacp$eig[,1], type="b", main="Éboulis des valeurs propres (Ligne)",
     xlab="Composante Principale", ylab="Valeur Propre", pch=19, col="#DE3163")

# INTERPRETATION DES COMPOSANTES
#etude des correlations entre composantes principales et  variables initiales
resuacp$var$cor
corACP= round(resuacp$var$cor,digits=2)

ggcorrplot(corACP[, 1:3], type ="full",lab = TRUE, lab_col = "white", col=viridis(3,option="G",direction=-1),outline.color="white",ggtheme="theme_bw")

fviz_pca_var(resuacp, axes=c(1,2), col.var="cadetblue",
             repel=TRUE,
             title="Cercle des corrélations (C1, C2)",
             col.circle = "black")
help(fviz_pca_var)

fviz_pca_var(resuacp, axes=c(1,3), col.var="cadetblue",
             gradient.cols=viridis(10,option="G",direction=-1),
             repel=TRUE,
             title="Cercle des corrélations (Dim1, Dim3)",
             col.circle = "black")

#etude des contributions
resuacp$var$contrib
contrib=round(resuacp$var$contrib,digits=2) #seuil 100/9=11,11


# -------------------------------------------------------------------------

#Etape 4 : qualite de repr?sentation des pays sur les cp retenues

resuacp$ind$coord #composantes principales
tabcos2=resuacp$ind$cos2 #le tableau des cos2 des individus
resuacp$ind$dist

# Qualité de représentation des variables (carrés des cosinus)
print("Qualité de représentation des variables (cos2) :")
cos2var = round(resuacp$var$cos2[,1:3], digits=2)
print(cos2var)
PCAshiny(pays7)

#library(factoextra)

#individus bien représentés sur C1 (cos2>0.5)
tabcos2[tabcos2[,1]>0.5,1]

#individus bien représentés sur C2 (cos2>0.25)
tabcos2[tabcos2[,2]>0.25,2]

#individus bien représentés sur C3 (cos2>0.15)
tabcos2[tabcos2[,3]>0.15,3]

#etude des contributions
resuacp$ind$contrib
contrib=round(resuacp$ind$contrib,digits=2) #seuil 100/50=2

contrib[contrib[,1]>2,1]
contrib[contrib[,2]>2,2]
contrib[contrib[,3]>2,3]

#cercles des correlations et individus

fviz_pca_var(resuacp, axes=c(1,2), col.var="cos2",
             gradient.cols=viridis(10,option="G",direction=-1),
             repel=TRUE,
             title="Cercle des corrélations (C1, C2)",
             col.circle = "black",
             fill.var = "pink")
help(fviz_pca_var)
fviz_pca_var(resuacp, axes=c(1,3), col.var="cos2",
             gradient.cols=viridis(10,option="G",direction=-1),
             repel=TRUE,
             title="Cercle des corrélations (C1, C3)",
             col.circle = "black")


# Graphique des individus sur le plan 1-2
fviz_pca_ind(resuacp, axes=c(1, 2), col.ind="cos2",
             gradient.cols=viridis(10,option="G",direction=-1),
             repel=TRUE,
             title="Nuage de points des pays sur (C1,C2)")

# Graphique des individus sur le plan 1-3
fviz_pca_ind(resuacp, axes=c(1, 3), col.ind="cos2",
             gradient.cols=viridis(10,option="G",direction=-1),
             repel=TRUE,
             title="Nuage de points des pays sur (C1,C3)")

# Biplot (variables et individus) sur le plan 1-2
fviz_pca_biplot(resuacp, axes=c(1, 2), repel=TRUE,
                col.var="red", col.ind="darkblue",
                title="Biplot des pays et variables (C1,C2)")

# Biplot (variables et individus) sur le plan 1-2
fviz_pca_biplot(resuacp, axes=c(1, 3), repel=TRUE,
                col.var="red", col.ind="darkblue",
                title="Biplot des pays et variables (C1,C3)")

#interpétation des individus extrèmes

pays_ext = pays7[c("Switzerland", "Netherlands","Niger","Sierra-Leone"),c("pibh","revenu", "espvie","mortinfant")]
moy = colMeans(pays7[,c("pibh","revenu", "espvie","mortinfant")])
pays_ext["Moyenne",]=moy
round(pays_ext,digits=0)
