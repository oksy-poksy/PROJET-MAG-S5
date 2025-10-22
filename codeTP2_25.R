#TP2 AFC
#importation du fichier assurance.txt
#On transforme la variable valeur en factor (var quali.)
conducteurs$valeur=factor(conducteurs$valeur,
                          labels=c("faible valeur"
                                   ,"valeur moyenne",
                                   "valeur élevée",
                               "valeur très elevée"))

conducteurs$sexe=factor(conducteurs$sexe,labels=c("F","M"))
conducteurs$formule=factor(conducteurs$formule)
summary(conducteurs)
attach(conducteurs)#permet d'appeler les variables par leur nom
#Stat univariee
table(valeur)#effectifs
table(formule)
tab1=round(prop.table(table(valeur)),digits=2)#freq rel.
round(prop.table(table(formule)),digits=3)
cumsum(tab1) #les freq. rel. cumulées de valeur

#Diagramme en colonnes
barplot(prop.table(table(valeur)),
        main="Répartition des conducteurs selon 
  \n la valeur marchande de leur vehicule",
        col="orange",
        xlab= "Valeur marchande",
        ylab="Frequence relative",
        ylim=c(0,0.7))
#camembert
pie(table((sexe)))
help(pie)

#Stat bivariee
tabcontin=table(formule,valeur)#tableau de contingence
addmargins(tabcontin)#pour ajouter les totaux

tabPL=prop.table(tabcontin,1)#profils-lignes
round(tabPL,digits=2)            
tabPC=prop.table(tabcontin,2)#profils-colonnes

round(tabPC,digits=2) 
#Gaphiques
barplot(t(tabPL),beside=TRUE,col=1:4,
        main="Repartition des conducteurs
        selon  la valeur du vehicule 
         pour chaque formule d'assurance",
        xlab = "Formule d'assurance",
        ylab="Frequence relative",ylim=c(0,0.8),
        legend.text=TRUE)
help(legend)
barplot(tabPC,beside=TRUE,col=c("blue","red","green"),
        main="Repartition des conducteurs 
        selon la formule d'assurance \n par categorie de valeur marchande",
        xlab = "Valeur marchande",
        ylab="Frequence relative", ylim=c(0,0.7),
        legend.text=TRUE)

#Test du khi2 d'independance
chisq.test(tabcontin)
resutest=chisq.test(tabcontin)
#khi2=136
resutest#les 2 variables sont liees (ecrire H0)
#Etude des contributions au khi2

round(resutest$residuals^2,2)#c_ij
#2 plus fortes contributions : (C,tres elevee)
#(C,faible)
#Comparer n_ij et e_ij pour ces 2 couples
round(resutest$expected,2)#les e_ij

#(C,tres elevee) :n_ij=74 >e_ij=35.74 : sur-representation des jeunes conducteurs ayant un vehicule de valeur
#tres eleve et ayant choisi la formule d'assurance C
#(C,faible):n_ij= 0 < e_ij=37.86 : sous-representation des jeunes conducteurs ayant un vehicule de faible valeur
#et ayant choisi la formule d'assurance C
#Test du khi2 d'independance
chisq.test(tabcontin)
resutest=chisq.test(tabcontin)
#khi2=136
resutest#les 2 variables sont liees (ecrire H0)
#car p< 2,2 *10^-16<5%
#Etude des contributions au khi2
round(resutest$residuals^2,2)#c_ij

#2 plus fortes contributions : (C,tres elevee)
#(C,faible)
#Comparer n_ij et e_ij pour ces 2 couples


#(C,tres elevee) :n_ij=74 >e_ij=35.74 : sur-representation des jeunes conducteurs ayant un vehicule de valeur
#tres eleve et ayant choisi la formule d'assurance C
#(C,faible):n_ij= 0 < e_ij=37.86 : sous-representation des jeunes conducteurs ayant un vehicule de faible valeur
#et ayant choisi la formule d'assurance C

##########################################################################################################################################

#AFC
#Charger FactoMinER
resuAFC=CA(tabcontin)
resuAFC$eig
help(CA)
resuAFC=CA(tabcontin,ncp=1)
resuAFC$row
resuAFC$col

#AFCM
tabchiens$TA=factor(tabchiens$TA)
tabchiens$VE=factor(tabchiens$VE)
tabchiens$PO=factor(tabchiens$PO)
tabchiens$IN=factor(tabchiens$IN)
tabchiens$AF=factor(tabchiens$AF)
tabchiens$AG=factor(tabchiens$AG)
summary(tabchiens)

resuAFCM=MCA(tabchiens)
resuAFCM$eig
resuAFCM$var
resuAFCM$ind
