
###### ------- SCRIPT R POUR PRODUIRE LES ESTIMATIONS D'EFFORT ET DE CAPTURES 

### --- Effacer les objets stock�s dans la m�moire R

rm(list=ls())

### --- S�lection du r�pertoire courant o� se situent les donn�es et r�f�rentiels, et o� seront stock�s les r�sultats

#R�pertoire courant A MODIFIER
folder="C:\\dev\\sih-sfa\\Lancement estimations R"
setwd(folder) 

### --- Choix de l'ann�e pour les estimations et des fichiers � traiter

annee=2017
fichier_flotte1           =paste0("Donn�es/fichier_flotte_",annee,".csv")
fichier_flotte2           ="Donn�es/fichier_flotte_complement.csv"
fichier_FINSS             ="Donn�es/ARTISANAL_FINSS.csv"
fichier_FINSS_from_OBSDEB ="Donn�es/DEDUCTED_ARTISANAL_FINSS.csv"
fichier_OBSDEB_CAPTURE    ="Donn�es/P03_OBSDEB_CAPTURE.csv"
fichier_OBSDEB_CAPTURE_LOT="Donn�es/P03_OBSDEB_CAPTURE_LOT.csv"
fichier_OBSDEB_MAREE      ="Donn�es/P03_OBSDEB_MAREE.csv"
fichier_SEMI_LL           ="Donn�es/SEMI_LL_DATA_2017.csv"
fichier_lobster           ="Donn�es/Lobster_20162017_Logbook.csv"
fichier_sea_cucumbers     ="Donn�es/Seacucumber_2016_2017.csv"
fichier_droplines         ="Donn�es/droplines_catches.csv"

fichier_algopesca_marees    ="Donn�es/SIH-GEOLOC-mar.txt"
fichier_algopesca_sequences ="Donn�es/SIH-GEOLOC-seq.txt"
fichier_algopesca_positions ="Donn�es/SIH-GEOLOC-positions.txt"

### --- Chargement des packages n�cessaires au lancement du script

source("Codes R/preparation_script.r")

#Cr�ation du r�pertoire contenant les r�sultats 
name_folder=paste(folder,"/Estimations/",annee,sep="") 
dir.create(name_folder,showWarnings=FALSE,recursive=T)

### --- Lancement des estimations

source("Codes R/preparation_donnees.r")
source("Codes R/affectation_metier_marees_OBSDEB.r")
source("Codes R/calendrier_activite_par_navire.r")
source("Codes R/estimation_effort_captures.r")
source("Codes R/sauvegarde_resultats.r")




