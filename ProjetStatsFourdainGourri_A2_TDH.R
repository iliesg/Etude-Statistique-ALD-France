getwd()
tableau = read.csv("ProjetStatsFourdainGourri.csv", TRUE , sep = ";", row.names =1, dec = ",")
View(tableau)

##Exemple 1 :Diabète
#Création colonne prévalence en pourcentage
prevalence.diabete.pourcentage = (tableau[,4]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.diabete.pourcentage)


##Exemple 2 : Artériopathies
#Création colonne prévalence en pourcentage
prevalence.Arteriopathies.pourcentage = (tableau[,5]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.Arteriopathies.pourcentage)


##Exemple 3 : Accident vasculaire cérébral invalidant
#Création colonne prévalence en pourcentage
prevalence.AVC.pourcentage = (tableau[,2]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.AVC.pourcentage)


##Exemple 4 : Insuffisance cardiaque et cardiopathies
#Création colonne prévalence en pourcentage
prevalence.cardiopathies.pourcentage = (tableau[,3]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,prevalence.cardiopathies.pourcentage)


#Calcul pourcentage de médecins par départements
medecins.departements.pourcentage = (tableau[,7]/as.numeric(tableau[,6]))*100
tableau = cbind(tableau,medecins.departements.pourcentage)


########### Paramètres ########### 

#Donne min/max, 1er et 3eme Qu, moyenne et médiane ; ici moyenne et médiane sont similaire donc la prévalence du diabète est symétrique
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

#L'age moyen en fonction de la prévalence du diabète
plot(tableau$AGE.moyen.en.2019.par.département, tableau$prevalence.diabete.pourcentage, type = "p")

#L'age moyen en fonction de la prévalence des Arteriopathies
plot(tableau$AGE.moyen.en.2019.par.département, tableau$prevalence.Arteriopathies.pourcentage, type = "p")

#L'age moyen en fonction de la prévalence des AVC
plot(tableau$AGE.moyen.en.2019.par.département, tableau$prevalence.AVC.pourcentage, type = "p")

#L'age moyen en fonction de la prévalence des cardiopathies
plot(tableau$AGE.moyen.en.2019.par.département, tableau$prevalence.cardiopathies.pourcentage, type = "p")

#Le nombre de médecins en fonction de la prévalence du diabète 
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.diabete.pourcentage, type = "p")

#Le nombre de médecins en fonction de la prévalence des Arteriopathies
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.Arteriopathies.pourcentage, type = "p")

#Le nombre de médecins en fonction de la prévalence des AVC
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.AVC.pourcentage, type = "p")

#Le nombre de médecins en fonction de la prévalence des cardiopathies
plot(tableau$medecins.departements.pourcentage, tableau$prevalence.cardiopathies.pourcentage, type = "p")


##Nombre de médecins généralistes en fonction du département 
plot(tableau$Départements, tableau$medecins.departements.pourcentage, type = "p")
boxplot(tableau$medecins.departements.pourcentage)
boxplot(tableau$prevalence.diabete.pourcentage~tableau$medecins.departements.pourcentage)




########### Tests de corrélation de Kendall : ###########

### Age moyen ###

#Entre diabète et age moyen
cor(tableau$prevalence.diabete.pourcentage,tableau$AGE.moyen.en.2019.par.département)
cor.test(tableau$prevalence.diabete.pourcentage,tableau$AGE.moyen.en.2019.par.département, method="kendall")

#Entre Arteriopathies et age moyen 
cor(tableau$prevalence.Arteriopathies.pourcentage,tableau$AGE.moyen.en.2019.par.département)
cor.test(tableau$prevalence.Arteriopathies.pourcentage,tableau$AGE.moyen.en.2019.par.département, method="kendall")

#Entre AVC et age moyen 
cor(tableau$prevalence.AVC.pourcentage,tableau$AGE.moyen.en.2019.par.département)
cor.test(tableau$prevalence.AVC.pourcentage,tableau$AGE.moyen.en.2019.par.département, method="kendall")

#Entre cardiopathies et age moyen
cor(tableau$prevalence.cardiopathies.pourcentage,tableau$AGE.moyen.en.2019.par.département)
cor.test(tableau$prevalence.cardiopathies.pourcentage,tableau$AGE.moyen.en.2019.par.département, method="kendall")

### Nombre de médecins ###

#Entre diabète et nombre de médecins 
cor(tableau$prevalence.diabete.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.diabete.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre Arteriopathies et nombre de médecins 
cor(tableau$prevalence.Arteriopathies.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.Arteriopathies.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre AVC et nombre de médecins 
cor(tableau$prevalence.AVC.pourcentage,tableau$medecins.departements.pourcentage)
cor.test(tableau$prevalence.AVC.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")

#Entre cardiopathies et nombre de médecins 
cor.test(tableau$prevalence.cardiopathies.pourcentage,tableau$medecins.departements.pourcentage, method="kendall")
