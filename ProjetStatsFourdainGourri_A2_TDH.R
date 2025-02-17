getwd()
tableau = read.csv("ProjetStatsFourdainGourri.csv", TRUE , sep = ";", row.names =1, dec = ",")
View(tableau)

##Exemple 1 :Diab�te
#Cr�ation colonne pr�valence en pourcentage
prevalence.diabete.pourcentage = (tableau[,4]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.diabete.pourcentage)


##Exemple 2 : Art�riopathies
#Cr�ation colonne pr�valence en pourcentage
prevalence.Arteriopathies.pourcentage = (tableau[,5]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.Arteriopathies.pourcentage)


##Exemple 3 : Accident vasculaire c�r�bral invalidant
#Cr�ation colonne pr�valence en pourcentage
prevalence.AVC.pourcentage = (tableau[,2]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.AVC.pourcentage)


##Exemple 4 : Insuffisance cardiaque et cardiopathies
#Cr�ation colonne pr�valence en pourcentage
prevalence.cardiopathies.pourcentage = (tableau[,3]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.cardiopathies.pourcentage)


#Calcul pourcentage de m�decins par d�partements
medecins.departements.pourcentage = (tableau[,7]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,medecins.departements.pourcentage)


########### Param�tres ########### 

#Donne min/max, 1er et 3eme Qu, moyenne et m�diane ; ici moyenne et m�diane sont similaire donc la pr�valence du diab�te est sym�trique
summary(tableau$prevalence.diabete.pourcentage)
summary(tableau$prevalence.Arteriopathies.pourcentage)
summary(tableau$prevalence.AVC.pourcentage)
summary(tableau$prevalence.cardiopathies.pourcentage)
summary(tableau$medecins.departements.pourcentage)

#ecart-type 
sd(tableau$prevalence.diabete.pourcentage)
sd(tableau$prevalence.Arteriopathies.pourcentage)
sd(tableau$prevalence.AVC.pourcentage)
sd(tableau$prevalence.cardiopathies.pourcentage)
sd(tableau$medecins.departements.pourcentage)



########### Graphiques et diagrammes ########### 

#L'age moyen en fonction de la pr�valence du diab�te
plot(tableau$AGE.moyen.en.2019.par.d�partement, tableau$prevalence.diabete.pourcentage, type = "p")

#L'age moyen en fonction de la pr�valence des Arteriopathies
plot(tableau$AGE.moyen.en.2019.par.d�partement, tableau$prevalence.Arteriopathies.pourcentage, type = "p")

#L'age moyen en fonction de la pr�valence des AVC
plot(tableau$AGE.moyen.en.2019.par.d�partement, tableau$prevalence.AVC.pourcentage, type = "p")

#L'age moyen en fonction de la pr�valence des cardiopathies
plot(tableau$AGE.moyen.en.2019.par.d�partement, tableau$prevalence.cardiopathies.pourcentage, type = "p")

#Le nombre de m�decins en fonction de la pr�valence du diab�te 
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.diabete.pourcentage, type = "p")

#Le nombre de m�decins en fonction de la pr�valence des Arteriopathies
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.Arteriopathies.pourcentage, type = "p")

#Le nombre de m�decins en fonction de la pr�valence des AVC
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.AVC.pourcentage, type = "p")

#Le nombre de m�decins en fonction de la pr�valence des cardiopathies
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.cardiopathies.pourcentage, type = "p")


##Nombre de m�decins g�n�ralistes en fonction du d�partement 
plot(tableau$D�partements, tableau$medecins.departements.pourcentage, type = "p")
boxplot(tableau$medecins.departements.pourcentage)
boxplot(tableau$prevalence.diabete.pourcentage~tableau$medecins.departements.pourcentage)




########### Tests de corr�lation de Kendall : ###########

### Age moyen ###

#Entre diab�te et age moyen
cor(tableau$prevalence.diabete.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement)
cor.test(tableau$prevalence.diabete.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement, method="kendall")

#Entre Arteriopathies et age moyen 
cor(tableau$prevalence.Arteriopathies.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement)
cor.test(tableau$prevalence.Arteriopathies.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement, method="kendall")

#Entre AVC et age moyen 
cor(tableau$prevalence.AVC.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement)
cor.test(tableau$prevalence.AVC.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement, method="kendall")

#Entre cardiopathies et age moyen
cor(tableau$prevalence.cardiopathies.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement)
cor.test(tableau$prevalence.cardiopathies.pourcentage,tableau$AGE.moyen.en.2019.par.d�partement, method="kendall")

### Nombre de m�decins ###

#Entre diab�te et nombre de m�decins 
cor(tableau$prevalence.diabete.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.diabete.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre Arteriopathies et nombre de m�decins 
cor(tableau$prevalence.Arteriopathies.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.Arteriopathies.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre AVC et nombre de m�decins 
cor(tableau$prevalence.AVC.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.AVC.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre cardiopathies et nombre de m�decins 
cor.test(tableau$prevalence.cardiopathies.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")
