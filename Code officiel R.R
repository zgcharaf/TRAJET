#######################################insall packages#############################################


install.packages(dplyr)

install.packages(e1071)
install.packages(rpart)
install.packages(ggplot2)
install.packages(glmnet)
install.packages(caret)
install.packages(rpart.plot)
install.packages(knitr)
install.packages(kableExtra)
install.packages(cowplot)
install.packages(questionr)
install.packages(mfx)
install.packages(MASS)

#importer les packages 

library('dplyr')

library('e1071')
library('ggplot2')
library('glmnet')
library('caret')
library('rpart.plot')
library('knitr')
library('kableExtra')
library('cowplot')
library("questionr")
library("mfx")
library("MASS") 


#import the data 
df = load("C:/Users/zgcha/Downloads/individus_ct2013b.Rdata")# 
#placer le fichier .Rdata dans le même dossier que le présent fichier 
df= dfload("/individus_ct2013b.Rdata")
#tester la bonne importation 
print(mydata)

#recenser les NA par colonnes 
na_counts_per_column = colSums(is.na(df))


df = data.frame(mydata)

############################################################construire la variable qualitative###################################
#créer la variable qualitative

NDF = filter(df,df$TRAJET !='' & df$TRAJET !=997)
#enlever les variables avec une seule modalité
NDF <- subset(NDF, select = c(-LIEN_19, -LIEN_20, -IPRAN, -TRAJET_DRAP)) 
#vérifier le bon numéro de lignes
nrow(NDF)
NDF$Trjt = rep(0,32208)
NDF$Trjt[NDF$TRAJET>=quantile(NDF$TRAJET)[4]] =  #1 SI TRAJET >=30
NDF$Trjt[NDF$TRAJET<quantile(NDF$TRAJET)[4]] =  # 0 SINON

##########################################################Renommer les modalités####################################################
NDF$SEXE = factor(NDF$SEXE, levels=c(1,2),labels=c("_HOMME", "_FEMME"))

NDF$DIPLOME = factor(NDF$DIPLOME, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 99, 98),   labels = c("_Aucun diplôme","_CEP", "_Brevet","_CAP", "_BACto", "_Bac Géné", "_Bac + 2", "_Bac +3/4", "_>bac+4", "_NSP", "_REFUS"))

NDF$IMMI = factor(NDF$IMMI, levels = c(0, 1), labels = c("_Non Immigré", "_Immigré"))
NDF$NATIO1NA= factor(NDF$NATIO1NA, levels = c(1,2,3,4,9,8), labels = c('_Français', "_Français par naturalisation","_Etranger","_Apatride",'_NSP','_REFUS'))
NDF$BSANTE = factor(NDF$BSANTE, levels = c(1,2,3,4,5,9,8),label = c('_Très bon', '_Bon', '_Assez bon','_Mauvais','_Très mauvais', "_Ne sait pas", '_Refus'))
NDF$TENIR = factor(NDF$TENIR, levels = c(1,2,9,8),label = c('_Oui', '_Non', '_NSP','_REFUS'))
NDF$CONTRAD = factor(NDF$CONTRAD, levels = c(1,2,8,9), label = c('_Oui', '_Non','_Refus', '_NSP'))

NDF$region = factor(NDF$region, levels = c('42', '72', '83', '25', '26', '53', '24', '21', '94', '43', '01', '03', '23', '11', '04', '91', '74', '41', '02', '73', '31', '52', '22', '54', '93', '82'), labels = c("_Alsace", "_Aquitaine", "_Auvergne", "_Basse-Normandie", "_Bourgogne", "_Bretagne", "_Centre", "_Champagne-Ardenne", "_Corse", "_Franche-Comté", "_Guadeloupe", "_Guyane", "_Haute-Normandie", "_Île-de-France", "_La Réunion", "_Languedoc-Roussillon", "_Limousin", "_Lorraine", "_Martinique", "_Midi-Pyrénées", "_Nord-Pas-de-Calais", "_Pays de la Loire", "_Picardie", "Poitou-Charentes", "_Provence-Alpes-Côte d'Azur", "_Rhône-Alpes"))

NDF$STATUT = factor(NDF$STATUT, levels = c(1,2,3,4,5,6,7,8,9,10),
                    label = c('_Salarié ETAT','_Salarié Collectivité territoriale',
                              '_Salrié hôpital public','_Salarié d\'un établissement privé de santé',
                              '_Salarié du secteur public social et médico-social','_Salarié entreprise,
                              d\'artisan','_Salarié d\'un ou plusieurs particuliers',
                              '_Aide d\'un membre de famille sans rémunération', 'Chef d\'entreprise, PDG, gérant','_indépendant'))

NDF$AUTSAL = factor(NDF$AUTSAL, levels = c(1, 2), labels = c(" Avoir une activité en dehors de son emploi principal ", " Ne pas avoir un emploi en dehors de son activité principale"))
NDF$CVFVP = factor(NDF$CVFVP, levels = c(1,2,3,4,8,9), labels = c(" Les horaires du travaill s'accordent très bien avec ses engagements sociaux", 
                                                                   "Les horaires du travaill s'accordent bien avec ses engagements sociaux",
                                                                   "Les horaires du travaill ne s'accordent pas très bien avec ses engagements sociaux",
                                                                   "Les horaires du travaill ne s'accordent pas  du tout avec ses engagements sociaux",
                                                                   "REFUS","NSP"))
NDF$B5D = factor(NDF$B5D, levels = c(1,2,3,4,9,8, ''), labels = c(" Toujours avoir le sentiment d'être dépassé par les changements trop rapides",
                                                              "Avoir souvent le sentiment d'être dépassé par les changements trop rapides",
                                                              " Avoir parfois le sentiment d'être dépassé par les changements trop rapides",
                                                              " Jamais avoir le sentiment d'être dépassé par les changements trop rapides", 
                                                              ' NSP', ' REFUS', " Non réponse"))
NDF$MISSION = factor(NDF$MISSION , levels = c(1,2,3,4,8,9),labels = c(' Dormir en dehors de chez soi en raison du travail une fois par semaine ou plus',
                                                                       ' Dormir en dehors de chez soi en raison du travail une à trois fois par mois', 
                                                                       ' Dormir en dehors de chez soi en raison du travail moins souvent',
                                                                       ' Jamais Dormir en dehors de chez soi en raison du travail', ' REFUS', ' NSP'))
NDF$COUPLE = factor(NDF$COUPLE, levels = c(1,2,3), labels = c(" Oui avec une personne qui vit dans le logement"," Oui avec une personne qui ne vit pas dans le logement"," Ne pas être en couple"))
NDF$DETHOR = factor(NDF$DETHOR, levels = c(1,2,3,4,8,9), labels = c(" Horaires déterminés par l'entreprise ou l'adminstration sans possibilité de modification",
                                                                    " Possibilité de choisir entre plusieurs fixes proposés par l'entrerpise",
                                                                    " Vos horaires sont modifiables par vous même d'un jour à l'autre(système d'horaire à la carte)",
                                                                    " Vos horaires sont déterminées par vous même"
                                                                    ," REFUS",
                                                                    " NSP"))
                                                            




NDF$CWDEBOU = factor(NDF$CWDEBOU, levels = c(1,2,8,9), labels = c(" Rester debout longtemps"," Ne pas rester debout longtemps"," REFUS"," NSP"))
NDF$NBSALENTC = factor(NDF$NBSALENTC, levels = c(1,2,3,8,9), labels = c(" 1 à 49 salariés", " 50 à 499", " 500 et plus", " REFUS", " NSP"))
NDF$MAISON = factor(NDF$MAISON, levels = c(1,2,3,4,5,8,9), labels = c(' Apporter tous les jours ou presque le travail chez soi',
                                                                      ' Apporter souvent le travail chez soi',
                                                                      ' Apporter parfois le travail chez soi',
                                                                      ' Apporter jamais le travail chez soi',
                                                                      ' Sans objet (travail à domicile)',' REFUS',' NSP'))


NDF$TENSION3 = factor(NDF$TENSION3, levels = c(1,2,3,8,9), labels = c(" Avoir des rapports de tension avec ses collègues", " Ne pas avoir des rapports de tension avec ses collègues", ' Sans objet(sans collègue)',' Refus', ' NSP'))
NDF$OBJECTIF = factor(NDF$OBJECTIF, levels = c(1,2,8,9), labels = c(" Devoir atteindre des objectifs chiffrés précis", ' Ne pas devoir atteindre des objectifs précis', ' REFUS', ' NSP'))
NDF$PUBLIC = factor(NDF$PUBLIC, levels = c(1,2,8,9), labels = c(" Etre en contact avec le public"," Ne pas être en contact avec le public"," REFUS"," NSP"))
NDF$CONTROLE = factor(NDF$CONTROLE, levels = c(1,2,3,4,5,6,8,9), labels = c(
  " Etre soumis à aucun contrôle horaire", 
  " Etre soumis à un horloge par pointeuse, badge", 
  " Etre soumis à un contrôle par l'encadrement ", 
  " Etre soumis à un contrôle par d'autres personnes, par exemple vos collègues", 
  " Autre type de contrôle" , 
  " REFUS",
  " NSP"," NSP"))

#########################################################################le modèle glm###############################################
attach(NDF)

full_model = glm(NDF$Trjt~NDF$AGE + NDF$AUTSAL+NDF$CVFVP+ NDF$B5D+NDF$WHO+
                   NDF$SEXE+NDF$MISSION+NDF$MAISON+NDF$COUPLE+
                   NDF$DETHOR+NDF$CWDEBOU+NDF$NBSALENTC+NDF$TENSION3+
                   NDF$anciennete+NDF$OBJECTIF+ NDF$CONTROLE+
                   NDF$DIPLOME + NDF$IMMI + NDF$BSANTE +
                   NDF$region+ NDF$STATUT , data = NDF,  family = binomial(link = "probit"))

summary(full_model)
#viusalisation
results_df <-(summary(full_model))
sumi <- kable(results_df$coefficients, align = "c", caption = "Summary",
              table.attr = "style='border-collapse: collapse;'")
print(sumi)
#exporter en csv
write.csv(results_df$coefficients, "mdydCSV.csv")
print(results_df)

############################################################################Pseudo R########################################################"

#pseudo R
ll.null = full_model$null.deviance/-2
ll.proposed = full_model$deviance/-2
pseudoR = (ll.null-ll.proposed)/ll.null

pseudoR

pvalue = 1 - pchisq(2*(ll.proposed-ll.null),df=(length(full_model$coefficients)-1))

##################################################deviance & AIC & BIC##########################################3
deviance <- deviance(full_model)
print(deviance)
# Print AIC
print(AIC(full_model))

# Print BIC
print(BIC(full_model))

###################################################odds ratio#############################################
#cela risque de prendre un peu de temps en raison du nombre de variables, personnellement cela m'avait pris 4 minutes 
#première méthode
odds = odds.ratio(full_model)
formatted_table <- kable(odds, align = "c")


print(formatted_table)

#deuxième méthode
odds <- exp(coef(full_model))

library(knitr)
formatted_table <- kable(odds, align = "c", caption = "Odds Ratio Table",
                      
                         table.attr = "style='border-collapse: collapse;'")

print(formatted_table)
#exporter le tout en pdf
write.csv(odds, "odds.csv")


#################################################marginal effects#######################################

marginaleffects = probitmfx(NDF$Trjt~NDF$AGE +NDF$CONGE+ NDF$SEXE+NDF$MISSION+NDF$MAISON+NDF$COUPLE+NDF$DETHOR+NDF$CWDEBOU+NDF$NBSALENTC+NDF$TENSION3+
                              NDF$anciennete+NDF$OBJECTIF+NDF$PUBLIC+ NDF$CONTROLE+ NDF$DIPLOME +NDF$STATUT+ NDF$IMMI + NDF$BSANTE + NDF$TENIR+NDF+NDF$region, data = NDF)
margintable <- kable(marginaleffects, align = "c")
print(margintable)

margindf <- data.frame(marginaleffects$mfxest)

library(knitr)
formatted_table <- kable(margindf, align = "c", caption = "Marginal Effects Table",
                    
                         row.names = FALSE, # Exclude row names from the table
                         format.args = list(digits = 3),
                         table.attr = "style='border-collapse: collapse;'")

print(formatted_table)
write.csv(margindf, "MARGINAL.csv")



print(formatted_table)



##############################################################################iterations pour trouver la combinaison de variables explicatives avec le AIC le moins faible

fit_probit_model <- function(formula, data) {
  model <- glm(formula = formula, data = data, family = binomial(link = "probit"))

  aic = AIC(model)
  bic =BIC(model)
  
  return(c(aic,bic))
}


fit_probit_model(Trjt~CONTROLE+MAISON, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR, NDF)

fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC+WHO, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC+AUTSAL, NDF)
fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC+AUTSAL+CVFVP, NDF)

fit_probit_model(Trjt~CONTROLE+MAISON+BSANTE +AGE+SEXE+STATUT+COUPLE+region+DIPLOME+CWDEBOU+IMMI+OBJECTIF+MISSION+PUBLIC+anciennete+TENSION3+TENIR+DETHOR+NBSALENTC+AUTSAL+CVFVP+B5D, NDF)
l
