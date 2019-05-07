library(data.table)

#set working directory
setwd('/Users/analutzky/Desktop/extracommunautaires_2017/69_univ')

#lire le fichier des effectifs d'étudiants inscrits
Table67_DN=fread('67univ_dipl_nat.csv')
Table67_DE=fread('67univ_dipl_etab.csv')
TableDauphine_DNDE=fread('dauphine.csv')
TableLorraine_DNDE=fread('Lorraine.csv')
TableComue_DNDE=fread('comue.csv')
Table5Comue_DNDE=TableComue_DNDE[
							(Etablissement=="Université Paris-Saclay"&(CURSUS_LMD=="M"|CURSUS_LMD=="D"))
							|(Etablissement=="Université Côte d'Azur"&CURSUS_LMD=="D")
							|(Etablissement=="Université Grenoble Alpes"&CURSUS_LMD=="D")
							|(Etablissement=="Université Bourgogne - Franche-Comté"&CURSUS_LMD=="D")
							|(Etablissement=="Université Paris-Est"&CURSUS_LMD=="D"),]

# rbind ça concatene des matrices (A) et (B) en faisant
# (A)
# (B)
# i.e. ça concatène des lignes de tables ayant les memes colonnes

# cbind ça concatene des matrices (A) et (B) en faisant
# (A B)
# i.e. ça concatène des colonnes de tables ayant les memes lignes

Table=rbind(Table67_DN,Table67_DE,TableDauphine_DNDE,TableLorraine_DNDE, Table5Comue_DNDE)
rm(Table67_DN)
rm(Table67_DE)
rm(TableDauphine_DNDE)
rm(TableLorraine_DNDE)
rm (Table5Comue_DNDE)

# Combien d'établissement uniques ?
length(unique(Table$ETABLISSEMENT))

### regarder l'objet
# afficher les premières et dernières ligne de apb (pour des objets de type data.table)
Table
# afficher les noms de colonnes
colnames(Table)
# dans R studio (pour des tableaux pas trop grands) 
View(Table)
# -> affichage excel-like

var.names=colnames(Table)

### supprimer les espaces et caractères spéciaux des noms de colonnes
colnames(Table)=make.names(var.names)

# Combien d'inscrits ?
Table[,.(inscrits=sum(Nombre.d.étudiants.inscrits..inscriptions.principales.))]

# afficher les nouveaux noms de colonnes
colnames(Table)

# créer un vecteur avec les nomsde colonnes qui m'intéressent
MYVARS=c('Attractivité.internationale','Etablissement','Diplôme','CURSUS_LMD','Grande.discipline','Secteur.disciplinaire','Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.')

#### Etablissement aggregé

#mget ça transforme un vecteur de chaînes de caractère en vecteur de variables (sinon y'a get si c'est pour un seul string). Attention : datatable uniquement
# on fait tout deux fois: 
subTable_DN=Table[DN_DE=="DN",mget(MYVARS)]
subTable_DNDE=Table[,mget(MYVARS)]

# pour info table(Table$DN_DE) ça fait un tableau croisé dynamique sur une colonne (un peu comme un filtre excel, ça compte les occurrences d'une colonne)
# table(Table$Diplôme)

# dcast ça permet de remettre des lignes en colonnes
subTable_DN=dcast(subTable_DN,
	Etablissement + CURSUS_LMD  ~ Attractivité.internationale, 
	value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
	fun.aggregate=sum)

subTable_DNDE=dcast(subTable_DNDE,
                  Etablissement + CURSUS_LMD ~ Attractivité.internationale, 
                  value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
                  fun.aggregate=sum)

# afficher subTable
subTable_DN
subTable_DNDE

#renommer les colonnes de subTable
colnames(subTable_DN)=c("Etablissement", "CURSUS_LMD", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")
colnames(subTable_DNDE)=c("Etablissement", "CURSUS_LMD", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")

# créer une nouvelle colonne dans ma subTable
VARS_toKeep=c('Etablissement', 'CURSUS_LMD', 'Grande discipline', 'NB.ETR_NonUE','NB.TOT')
subTable_DN=subTable_DN[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DN=subTable_DN[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DN=subTable_DN[,mget(VARS_toKeep)]
subTable_DN[,DN_DE:='DN']

subTable_DNDE=subTable_DNDE[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DNDE=subTable_DNDE[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DNDE=subTable_DNDE[,mget(VARS_toKeep)]
subTable_DNDE[,DN_DE:='DN_DE']

# rbind ça concatène des lignes de tables ayant les memes colonnes
subTable=rbind(subTable_DN,subTable_DNDE)

subTable_LMD=subTable[,.(CURSUS_LMD='Tout (L,M,D)',NB.ETR_NonUE=sum(NB.ETR_NonUE),NB.TOT=sum(NB.TOT)),by=.(Etablissement,DN_DE)]
subTable_withLMD=rbind(subTable,subTable_LMD)

subTable_withLMD_DNDE=dcast(subTable_withLMD,Etablissement+CURSUS_LMD~DN_DE,value.var=c('NB.ETR_NonUE','NB.TOT'))

subTable_withLMD_DNDE=subTable_withLMD_DNDE[,Pct_ETR_NonUE_DN:=NB.ETR_NonUE_DN/NB.TOT_DN_DE*100]
subTable_withLMD_DNDE=subTable_withLMD_DNDE[,Pct_ETR_NonUE_DN_DE:=NB.ETR_NonUE_DN_DE/NB.TOT_DN_DE*100]

# csv2 c'est à la française (; en séparateur et , pour les décimales)
write.csv2(as.data.frame(subTable_withLMD_DNDE),file='69univ_5comue_Etablissement_LMD.csv',fileEncoding = "UTF8")

#### secteur disciplinaire aggregé

subTable_DN=Table[DN_DE=="DN",mget(MYVARS)]
subTable_DNDE=Table[,mget(MYVARS)]

# dcast ça permet de remettre des lignes en colonnes
subTable_DN=dcast(subTable_DN,
	Secteur.disciplinaire + CURSUS_LMD + Grande.discipline ~ Attractivité.internationale, 
	value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
	fun.aggregate=sum)

subTable_DNDE=dcast(subTable_DNDE,
                  Secteur.disciplinaire + CURSUS_LMD + Grande.discipline ~ Attractivité.internationale, 
                  value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
                  fun.aggregate=sum)

#renommer les colonnes de subTable
colnames(subTable_DN)=c("Secteur.disciplinaire", "CURSUS_LMD", "Grande.discipline", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")
colnames(subTable_DNDE)=c("Secteur.disciplinaire", "CURSUS_LMD", "Grande.discipline", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")

# créer une nouvelle colonne dans ma subTable
VARS_toKeep=c('Secteur.disciplinaire', 'CURSUS_LMD','Grande.discipline', 'NB.ETR_NonUE','NB.TOT')

subTable_DN=subTable_DN[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DN=subTable_DN[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DN=subTable_DN[,mget(VARS_toKeep)]
subTable_DN[,DN_DE:='DN']

subTable_DNDE=subTable_DNDE[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DNDE=subTable_DNDE[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DNDE=subTable_DNDE[,mget(VARS_toKeep)]
subTable_DNDE[,DN_DE:='DN_DE']

# rbind ça concatène des lignes de tables ayant les memes colonnes
subTable=rbind(subTable_DN,subTable_DNDE)

# on calcule pour L, M et D confondus
subTable_LMD=subTable[,.(CURSUS_LMD='Tout (L,M,D)',NB.ETR_NonUE=sum(NB.ETR_NonUE),NB.TOT=sum(NB.TOT)),by=.(Secteur.disciplinaire,DN_DE,Grande.discipline )]
# on l'ajoute à la subTable
subTable_withLMD=rbind(subTable,subTable_LMD)

# on remet les DN et les DNDE dans des colonnes différentes
subTable_withLMD_DNDE=dcast(subTable_withLMD,Secteur.disciplinaire+CURSUS_LMD+Grande.discipline ~DN_DE,value.var=c('NB.ETR_NonUE','NB.TOT'))

# on calcule la part des extraUE
subTable_withLMD_DNDE=subTable_withLMD_DNDE[,Pct_ETR_NonUE_DN:=NB.ETR_NonUE_DN/NB.TOT_DN_DE*100]
subTable_withLMD_DNDE=subTable_withLMD_DNDE[,Pct_ETR_NonUE_DN_DE:=NB.ETR_NonUE_DN_DE/NB.TOT_DN_DE*100]

# csv2 c'est à la française (; en séparateur et , pour les décimales)
write.csv2(as.data.frame(subTable_withLMD_DNDE),file='69univ_5comue_Discipline_GdeDisc_LMD.csv',fileEncoding = "UTF8")

#### secteur disciplinaire detail

subTable_DN=Table[DN_DE=="DN",mget(MYVARS)]
subTable_DNDE=Table[,mget(MYVARS)]

# dcast ça permet de remettre des lignes en colonnes
subTable_DN=dcast(subTable_DN,
	Etablissement + CURSUS_LMD + Secteur.disciplinaire ~ Attractivité.internationale, 
	value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
	fun.aggregate=sum)

subTable_DNDE=dcast(subTable_DNDE,
                  Etablissement + CURSUS_LMD + Secteur.disciplinaire ~ Attractivité.internationale, 
                  value.var=c('Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28','Nombre.d.étudiants.inscrits..inscriptions.principales.'),
                  fun.aggregate=sum)

# afficher subTable
subTable_DN
subTable_DNDE

#renommer les colonnes de subTable
colnames(subTable_DN)=c("Etablissement", "CURSUS_LMD", "Secteur.disciplinaire", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")
colnames(subTable_DNDE)=c("Etablissement", "CURSUS_LMD", "Secteur.disciplinaire", "NB.UE28_OTHER", "NB.UE28_ETR", "NB.TOT_OTHER", "NB.TOT_ETR")

# créer une nouvelle colonne dans ma subTable
VARS_toKeep=c('Etablissement', 'CURSUS_LMD', 'Secteur.disciplinaire', 'NB.ETR_NonUE','NB.TOT')
subTable_DN=subTable_DN[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DN=subTable_DN[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DN=subTable_DN[,mget(VARS_toKeep)]
subTable_DN[,DN_DE:='DN']

subTable_DNDE=subTable_DNDE[,NB.TOT:=NB.TOT_ETR+NB.TOT_OTHER]
subTable_DNDE=subTable_DNDE[,NB.ETR_NonUE:=NB.TOT_ETR-NB.UE28_ETR]
subTable_DNDE=subTable_DNDE[,mget(VARS_toKeep)]
subTable_DNDE[,DN_DE:='DN_DE']

# rbind ça concatène des lignes de tables ayant les memes colonnes
subTable=rbind(subTable_DN,subTable_DNDE)

# # on calcule pour L, M et D confondus
# subTable_LMD=subTable[,.(CURSUS_LMD='Tout (L,M,D)',NB.ETR_NonUE=sum(NB.ETR_NonUE),NB.TOT=sum(NB.TOT)),by=.(Secteur.disciplinaire,DN_DE)]
# # on l'ajoute à la subTable
# subTable_withLMD=rbind(subTable,subTable_LMD)

# on remet les DN et les DNDE dans des colonnes différentes
subTable_DNDE=dcast(subTable,Etablissement+CURSUS_LMD + Secteur.disciplinaire~DN_DE,value.var=c('NB.ETR_NonUE','NB.TOT'))

# on calcule la part des extraUE
subTable_DNDE=subTable_DNDE[,Pct_ETR_NonUE_DN:=NB.ETR_NonUE_DN/NB.TOT_DN_DE*100]
subTable_DNDE=subTable_DNDE[,Pct_ETR_NonUE_DN_DE:=NB.ETR_NonUE_DN_DE/NB.TOT_DN_DE*100]

# csv2 c'est à la française (; en séparateur et , pour les décimales)
write.csv2(as.data.frame(subTable_DNDE),file='69univ_Consolidation_Etablissement_Discipline_LMD.csv',fileEncoding = "UTF8")

##### PETITS CALCULS DE VERIF

Table[,.(NbInscrit=sum(Nombre.d.étudiants.inscrits..inscriptions.principales.)),by= Attractivité.internationale]
######### DN + DE ##########
#                                                 Attractivité.internationale      NbInscrit
# 1:                                                               Autres cas 		1336218
# 2: Etudiants de nationalité étrangère issus de systèmes éducatifs étrangers 		 161828

####### DN uniquement ######
#                                                Attractivité.internationale   NbInscrit
#1:                                                               Autres cas   1273584
#2: Etudiants de nationalité étrangère issus de systèmes éducatifs étrangers    133973

Table[,.(NbInscritUE=sum(Nombre.d.étudiants.inscrits.issus.des.pays.membres.de.l.UE28)),by= Attractivité.internationale]
############## DN + DE ############
#                                                 Attractivité.internationale      NbInscritUE
# 1:                                                               Autres cas 		1290905
# 2: Etudiants de nationalité étrangère issus de systèmes éducatifs étrangers  		 27253

####### DN uniquement ######
#                                                Attractivité.internationale     NbInscritUE
#1:                                                               Autres cas     1230482
#2: Etudiants de nationalité étrangère issus de systèmes éducatifs étrangers       18123
