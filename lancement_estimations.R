
###### ------- SCRIPT R POUR PRODUIRE LES ESTIMATIONS D'EFFORT ET DE CAPTURES 

### --- Effacer les objets stockés dans la mémoire R

rm(list=ls())

### --- Sélection du répertoire courant où se situent les données et référentiels, et où seront stockés les résultats

#Répertoire courant A MODIFIER
folder="C:\\dev\\sih-sfa\\Lancement estimations R"
setwd(folder) 

### --- Choix de l'année pour les estimations et des fichiers à traiter

annee=2017
fichier_flotte1           =paste0("Données/fichier_flotte_",annee,".csv")
fichier_flotte2           ="Données/fichier_flotte_complement.csv"
fichier_FINSS             ="Données/ARTISANAL_FINSS.csv"
fichier_FINSS_from_OBSDEB ="Données/DEDUCTED_ARTISANAL_FINSS.csv"
fichier_OBSDEB_CAPTURE    ="Données/P03_OBSDEB_CAPTURE.csv"
fichier_OBSDEB_CAPTURE_LOT="Données/P03_OBSDEB_CAPTURE_LOT.csv"
fichier_OBSDEB_MAREE      ="Données/P03_OBSDEB_MAREE.csv"
fichier_SEMI_LL           ="Données/SEMI_LL_DATA_2017.csv"
fichier_lobster           ="Données/Lobster_20162017_Logbook.csv"
fichier_sea_cucumbers     ="Données/Seacucumber_2016_2017.csv"
fichier_droplines         ="Données/droplines_catches.csv"

fichier_algopesca_marees    ="Données/SIH-GEOLOC-mar.txt"
fichier_algopesca_sequences ="Données/SIH-GEOLOC-seq.txt"
fichier_algopesca_positions ="Données/SIH-GEOLOC-positions.txt"

### --- Chargement des packages nécessaires au lancement du script

source("Codes R/preparation_script.r")

#Création du répertoire contenant les résultats 
name_folder=paste(folder,"/Estimations/",annee,sep="") 
dir.create(name_folder,showWarnings=FALSE,recursive=T)

### --- Lancement des estimations

source("Codes R/preparation_donnees.r")
source("Codes R/affectation_metier_marees_OBSDEB.r")
source("Codes R/calendrier_activite_par_navire.r")
source("Codes R/estimation_effort_captures.r")
source("Codes R/sauvegarde_resultats.r")




