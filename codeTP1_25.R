#TP1 : Stat. des. et ACP 
#A faire au début de chaque séance:session-set working directory
#-choose directory pour relier le dossier anadon à RStudio

#Importation du fichier pays_eu.txt

pays=read.table("pays_eu.txt",header=TRUE)#on peut aussi importer par les menus "Import Dataset")
#autre méthode
pays3=read.table(file.choose(),header=TRUE)
help(read.table)
pays
head(pays)
#On change le nom des lignes (pour les grahiques de l'ACP)
pays=read.table("pays_eu.txt",header=TRUE,row.names="PAYS")
#alternative 
paysbis=read.table("pays_eu.txt",header=TRUE,row.names=1)#lacolone PAYS est la première colonne du fichier


dim(pays)
pays=pays[,2:12]#on enleve les variables qualitatives 
dim(pays)

# Statistique descriptive univariee
summary(pays)
POP
pays2$POP#la variable POP

attach(pays)#pour appeler les variables du fichier sans utiliser le $
POP
#ecart-type
sd(POP)#standard deviation
help(sd)

#Calul de tous les ecarts-type en meme temps
apply(pays,2,sd)# tous les ecartypes
apply(pays,2,mean)#toutes les moyennes

#Coef de variations (écart-type/moyenne) : seuil 0.25 (si CV est >0.25 variable dispersee autour de sa moyenne et variable peu dispersee si CV<0.25)
tabCV=apply(pays,2,sd)/apply(pays2,2,mean)
tabCV

round(tabCV,digits=2)#arrondir

# histogramme de TACT
hist(TACT,freq=FALSE,col="green",main="Répartition des pays selon le taux d'activité",ylim=c(0,0.06))
# boite a moustaches de la variable TCHOM
boxplot(TCHOM,main="Boîte à moustaches du taux de chômage")
points(mean(TCHOM),col="cyan",pch="*",cex=2)#pour rajouter la moyenne à la boîte à moustaches

#Statistique bivariee
# Nuages de points
plot(POP,TEL)
plot(pays,main="Matrice des nuages de points")
# Matrice des correlations
cor(POP,TEL)
tabcor=cor(pays2)
round(tabcor,digits=2)

#Avec les packages ggplot2 et ggcorrplot 
#installer d'abord les deux packages
library(ggplot2)
library(ggcorrplot)
ggcorrplot(tabcor)
ggcorrplot(tabcor, hc.order = TRUE, type = "lower",lab = TRUE)
# Nuage de points simples avec ggplot2
ggplot(pays, aes(x=POP, y=TEL)) 
# Changer la taille la couleur et la forme et mettre des labels
ggplot(pays, aes(x=POP, y=TEL)) +
  geom_point(size=2,col="BLUE", shape=23)+geom_text(label=rownames(pays2))
#####################################################################################################
#ACP avec FactoMineR sur le fichier
#des pays européens


#Etape 1 : corrélations entre les variables initiales
matcor=cor(pays)
round(matcor,digits=2)#ou avec le package ggcorrplot (voir stat des-ci-dessus)
#il existe des corrélations linéaires assez éleées en valeur absolue entre les 
#variables de départ (les donner), ce qui justifie l'ACP


#Indice KMO
#Installer er charger le package EFAtools
indice_KMO=KMO(tabcor)
help(KMO)

#Kaiser-Meyer-Olkin criterion (KMO) ------------------

#  x The overall KMO value for your data is miserable.
#These data are hardly suitable for factor analysis.

#Overall: 0.593

#For each variable:
# EVH     EVF     POP    TEMP    PIBH    TINF    TACT 
#0.683   0.608   0.532   0.424   0.790   0.283   0.605 
#TCHOM TCHOMLD  MARIAG     TEL 
0.598   0.666   0.361   0.529 

#On fait l'ACP avec la fonction PCA()
resuacp=PCA(pays)

#Etape2 : choix de la dimension
resuacp$eig
#Kaiser : 3vp >1 mais lambda_4=0,97
#Part d'inertie expliquée : 
#84% de l'inertie est conservée 
#si l'on retient 
#les 4 premières composantes principales
#Eboulis des valeurs propres
plot(resuacp$eig[,1])
lines(resuacp$eig[,1])
#Coude à la 3ème valeur propre donc 
#on retiendrait les 2 premières
#cp avec ce critère
#Décision:k=4 ou 3
help(PCA)#pour voir les différents arguments de la fonction PCA()
resuacp=PCA(pays,ncp=4,axes=c(3,4))#pour gader les résultats sur les 4 premières cp
#Etape 3 : Interprétation des cp retenues (corrélations et contributions)
resuacp$var$cor
round(resuacp$var$cor,digits=2)
#C1 très corrélée linéairement >0 avec EVH,EVF,PIBH,TACT
#et <0 avec CHOM, CHOMLD
#C2 corrélée linéairement >0 avec POP et TEL
#C3 corrélée linéairement >0 avec TEMP et<0 avec
#TINF
#C4 corrélée linéairement >0 avec MARIAGE
#Contributions des variables initiales 
#aux cp retenues
resuacp$var$contrib
round(resuacp$var$contrib,digits=2)
#seuil = 100/11 =9
#mêmes conclusions que les cor
resuacp=PCA(pays2,ncp=4,axes=c(3,4))#pour avoir les graphiques sur les axes 3 et 4
#Etape 4 : qualité de représentation des pays sur les cp retenues
resuacp$ind$coord #composantes principales
tabcos2=resuacp$ind$cos2 #le tableau des cos2 des individus
#individus bien représentés sur C1 (cos2>0.5)
tabcos2[tabcos2[,1]>0.5,1]
#Estonie  Lettonie  Lituanie  Pays-Bas 
#0.6352067 0.7284809 0.6849868 0.6432816 
#Autriche   Pologne  Slovaqui     Suede 
#0.6874003 0.7263514 0.7726960 0.6680321 
#individus bien représentés sur C2 (cos2>0.25)
tabcos2[tabcos2[,2]>0.25,2]
#Danemark  Allemagn   Espagne    France 
#0.4071777 0.6794906 0.7146678 0.7602916 
#Irlande    Italie  Portugal 
#0.2557235 0.9071638 0.4512612 
#individus bien représentés sur C3 (cos2>0.15)
tabcos2[tabcos2[,3]>0.15,3]
#Belgique  Rep-tche  Danemark  Allemagn 
#0.2248240 0.3620024 0.1590900 0.1502900 
#Grece   Irlande  Lituanie   Hongrie 
#0.5424276 0.2455376 0.1825275 0.3079377 
#Malte  Slovénie  Royaume- 
# 0.4270459 0.4032351 0.2614944
#individus bien représentés sur C4 (cos2>0.10)
tabcos2[tabcos2[,4]>0.10,4]
#Grece    Chypre     Malte 
#0.1154031 0.6008295 0.2006048

#Etape 5 : commentaire du graphique des pays
#Sur C1 : Suède, pays-bas et Autriche ont un EVH, EVH, PIBH
#et TACT élevés et TCHOM et TCHOMLD faibles
# Et pour la Pologne, la Slovaquie, La lettonie, Lituanie et l'Estonie
#c'est le contraire
#Sur C2 : France, All, Espagne, Italie :  POP et TEL élevés
#Danemark, Irlande Portugal : POP et TEL faibles
#Sur C3 : rep Tchèque, danemark, Allemagne, Lituanie, Royaume-Uni,
#ont une fort TEMP et un TINF faible
#Belgique, Grèce, Irlande, Hongrie, malte Slovénie : faible TEMP
#et fort TINF
#Sur C4 : Taux de mariage TRES élevé pour CHYPRE et élevé pour Malte
#et Grèce

#######################################################################
#######################################################################
#Graphiques en couleur
#Installer et charger le package factoextra
#Cercles des corrélations
fviz_pca_var(resuacp, axes = c(1, 2), col.var = "cos2",
             gradient.cols = c("skyblue", "gold", "coral"),
             repel = TRUE,
             title = "Cercle des corrélations - Plan (Dim1, Dim2)")

fviz_pca_var(resuacp, axes = c(3, 4), col.var = "cos2",
gradient.cols = c("skyblue", "gold", "coral"),
repel = TRUE,
title = "Cercle des corrélations - Plan (Dim3, Dim4)") 

#Graphiques des individus
fviz_pca_ind(resuacp, axes = c(1, 2), col.ind = "cos2",
             gradient.cols = c("darkblue", "gold", "darkred"),
             repel = TRUE,
             title = "Nuage de points des pays sur (C1,C2)")

fviz_pca_ind(resuacp, axes = c(3, 4), col.ind = "cos2",
             gradient.cols = c("darkblue", "gold", "darkred"),
             repel = TRUE,
             title = "Nuage de points des pays sur (C3,C4)")

fviz_pca_biplot(resuacp, axes = c(1, 2), repel = TRUE,
                col.var = "red", col.ind = "darkblue",
                title = "Nuage de points des pays sur (C1,C2) et variables")


fviz_pca_biplot(resuacp, axes = c(3, 4), repel = TRUE,
                col.var = "red", col.ind = "darkblue",
                title = "Nuage de points des pays sur (C3,C4) et variables") 

#######
#Indice pour justifier l'ACP
#Indice KMO
#Installer et charger le package EFAtools
indice_KMO=KMO(tabcor)#
#######################################################################
#######################################################################
#Avec Factoshiny#installer le package Factoshiny
PCAshiny(pays)




 
