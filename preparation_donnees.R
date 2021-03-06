
### --- Importation du fichier flotte
fichier_flotte=read.csv(fichier_flotte1,h=T,sep=";")

### --- Importation du compl�ment de fichier flotte (notamment pour les sea cucumbers)
fichier_flotte_complement=read.csv(fichier_flotte2,h=T,sep=";")

fichier_flotte=rbind(fichier_flotte,fichier_flotte_complement)
fichier_flotte=fichier_flotte[match(unique(fichier_flotte$NAVIRE),fichier_flotte$NAVIRE),]

#fichier_flotte[fichier_flotte$NAVIRE=="SZ1751",]
#fichier_flotte[fichier_flotte$NAVIRE=="SZ1365",]

### --- Importation du r�f�rentiel des ports avec hi�rarchie

ref_ports=read.csv("R�f�rentiels/ports.csv",sep=";",h=T)
ref_ports$LABEL=as.character(ref_ports$LABEL)
ref_ports$NAME=as.character(ref_ports$NAME)

### --- Importation des fichiers d'agr�gation m�tiers/esp�ces/engins

ref_agregation_metiers=read.csv("R�f�rentiels/agregation_metiers.csv",h=T,sep=";",na.strings="")
ref_agregation_engins=read.csv("R�f�rentiels/agregation_engins.csv",h=T,sep=";",na.strings="")
ref_agregation_especes=read.csv("R�f�rentiels/agregation_especes.csv",h=T,sep=";",na.strings="")

### --- Importation du fichier FINSS

if (file.exists(fichier_FINSS)) {
  FINSS=read.csv(fichier_FINSS,h=T,sep=",")
  FINSS=unique(FINSS[FINSS$TripYear==annee,])
  FINSS$DepartureDate=as.POSIXct(FINSS$DepartureDate,"%d/%m/%Y",tz="Europe/Paris")
  FINSS$ArrivalDate=as.POSIXct(FINSS$ArrivalDate,"%d/%m/%Y",tz="Europe/Paris")
  FINSS$LogDate=as.POSIXct(FINSS$LogDate,"%d/%m/%Y",tz="Europe/Paris")
  
  # Inclusion des ports SIH dans le fichier
  FINSS_REF_SITES=read.csv("R�f�rentiels/tbl_A1_ArtVesselMonitoringREF_SITES.csv",h=T,sep=";")
  FINSS_REF_SITES=unique(setNames(FINSS_REF_SITES[,c("FINSSID","SIH_LOCATION_LABEL")],c("FINSSID","SIH_LOCATION_COD")))
  FINSS_REF_SITES$SIH_LOCATION_COD=as.character(FINSS_REF_SITES$SIH_LOCATION_COD)
  FINSS=merge(FINSS,FINSS_REF_SITES,by.x="DeparturePortID",by.y="FINSSID")
  FINSS=merge(FINSS,setNames(ref_ports[,c("LABEL","NAME")],c("SIH_LOCATION_COD","SIH_LOCATION_LIB")),all.x=T)
  FINSS$SIH_LOCATION_COD=as.character(FINSS$SIH_LOCATION_COD)
  
  ## S�lection des navires du fichier flotte
  
  #Recoder les SZ sans �ventuels espaces
  FINSS$SZ=gsub(' ','',as.character(FINSS$SZ))
  FINSS=merge(FINSS,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by.x="SZ",by.y="NAVIRE",all.x=T)
  FINSS$GR_VESSEL_TYPE=as.character(FINSS$GR_VESSEL_TYPE)
  FINSS=unique(FINSS[order(FINSS$SZ,FINSS$TripYear,FINSS$Weeknumber,FINSS$TripID,FINSS$LogDate),c("SZ","GR_VESSEL_TYPE","TripYear","TripMonth","Weeknumber","TripID","DepartureDate","ArrivalDate","LogDate","LogEvent","SIH_LOCATION_COD","SIH_LOCATION_LIB")])
}

if (file.exists(fichier_FINSS_from_OBSDEB)) {
  FINSS_deducted=read.csv(fichier_FINSS_from_OBSDEB,h=T,sep=";")
  FINSS_deducted=unique(FINSS_deducted[FINSS_deducted$TripYear==annee,])
  FINSS_deducted$DepartureDate=as.POSIXct(FINSS_deducted$DepartureDate,"%d/%m/%Y",tz="Europe/Paris")
  FINSS_deducted$ArrivalDate=as.POSIXct(FINSS_deducted$ArrivalDate,"%d/%m/%Y",tz="Europe/Paris")
  FINSS_deducted$LogDate=as.POSIXct(FINSS_deducted$LogDate,"%d/%m/%Y",tz="Europe/Paris")
  FINSS_deducted$GR_VESSEL_TYPE=as.character(FINSS_deducted$GR_VESSEL_TYPE)
  FINSS_deducted=unique(FINSS_deducted[order(FINSS_deducted$SZ,FINSS_deducted$TripYear,FINSS_deducted$Weeknumber,FINSS_deducted$TripID,FINSS_deducted$LogDate),c("SZ","GR_VESSEL_TYPE","TripYear","TripMonth","Weeknumber","TripID","DepartureDate","ArrivalDate","LogDate","LogEvent","SIH_LOCATION_COD","SIH_LOCATION_LIB")])
}

### --- Importation des �chantillons CAS

CaptureEch=read.csv(fichier_OBSDEB_CAPTURE,h=T,sep=";",dec=".",encoding="UTF-8")
CaptureEch=CaptureEch[CaptureEch$ANNEE==annee,]

# Ajout des captures par lot
CaptureEch2=read.csv(fichier_OBSDEB_CAPTURE_LOT,h=T,sep=";",dec=".",encoding="UTF-8")
CaptureEch2=CaptureEch2[CaptureEch$ANNEE==annee,]

# union
CaptureEch$NAVIRE=as.character(CaptureEch$NAVIRE)
CaptureEch2$NAVIRE=as.character(CaptureEch2$NAVIRE)
CaptureEch=rbind(CaptureEch, CaptureEch2)

sum(CaptureEch$QUANTITE_CAP_VIF[!is.na(CaptureEch$QUANTITE_CAP_VIF)])

MareeEch=read.csv(fichier_OBSDEB_MAREE,h=T,sep=";",dec=".",encoding="UTF-8")
MareeEch=MareeEch[MareeEch$ANNEE==annee,]
MareeEch$PORTDEB_COD=as.character(MareeEch$PORTDEB_COD)
MareeEch$DATE_DEPART=as.POSIXct(MareeEch$DATE_DEPART,"%d/%m/%Y",tz="Europe/Paris")
MareeEch$DATE_RETOUR=as.POSIXct(MareeEch$DATE_RETOUR,"%d/%m/%Y",tz="Europe/Paris")
MareeEch$DATE_DEBARQ=as.POSIXct(MareeEch$DATE_DEBARQ,"%d/%m/%Y",tz="Europe/Paris")

# le metier est bien connu dans Obsdeb
MareeEch$GR_METIER_COD=as.character(MareeEch$METIER_COD_M)
MareeEch$GR_METIER_LIB=as.character(MareeEch$METIER_LIB_M)

#Ajout de la typologie navire aux �chantillons

naviresECH=unique(MareeEch[,c("ID_MAREE","NAVIRE","IMMATRICULATION","TYPE_NAVIRE")])
#length(unique(naviresECH$IMMATRICULATION))
naviresECH=merge(naviresECH,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by.x="IMMATRICULATION",by.y="NAVIRE",all.x=T)
naviresECH$GR_VESSEL_TYPE=as.character(naviresECH$GR_VESSEL_TYPE)
naviresECH$TYPE_NAVIRE=as.character(naviresECH$TYPE_NAVIRE)
if(any(is.na(naviresECH$GR_VESSEL_TYPE))) naviresECH[is.na(naviresECH$GR_VESSEL_TYPE),]$GR_VESSEL_TYPE=naviresECH[is.na(naviresECH$GR_VESSEL_TYPE),]$TYPE_NAVIRE
nav_typo_inconnue=naviresECH[naviresECH$GR_VESSEL_TYPE=="" & grepl("UNK",naviresECH$NAVIRE),]
if(nrow(nav_typo_inconnue)>0){
  programme=data.frame(NAVIRE=nav_typo_inconnue$NAVIRE,PROGRAMME=substr(nav_typo_inconnue$NAVIRE,5,7),GR_VESSEL_TYPE="")
  programme$GR_VESSEL_TYPE=as.character(programme$GR_VESSEL_TYPE)
  programme$GR_VESSEL_TYPE=replace(programme$GR_VESSEL_TYPE,programme$PROGRAMME=="SCH","Schooner")
  programme$GR_VESSEL_TYPE=replace(programme$GR_VESSEL_TYPE,programme$PROGRAMME=="WHA","Whaler")
  programme=programme[programme$GR_VESSEL_TYPE!="",c("NAVIRE","GR_VESSEL_TYPE")]
  nav_typo_inconnue=merge(nav_typo_inconnue[,c("IMMATRICULATION","ID_MAREE","NAVIRE")],programme)[,c("IMMATRICULATION","ID_MAREE","NAVIRE","GR_VESSEL_TYPE")]
  naviresECH[naviresECH$GR_VESSEL_TYPE=="" & grepl("UNK",nav_typo_inconnue$NAVIRE),]=merge(naviresECH[naviresECH$GR_VESSEL_TYPE=="" & grepl("UNK",nav_typo_inconnue$NAVIRE),c("IMMATRICULATION","ID_MAREE","NAVIRE","TYPE_NAVIRE")],nav_typo_inconnue)
}
MareeEch=merge(MareeEch,naviresECH[,c("ID_MAREE","GR_VESSEL_TYPE")],by="ID_MAREE",all.x=T)
CaptureEch=merge(CaptureEch,unique(MareeEch[,c("ID_MAREE","GR_VESSEL_TYPE")]),all.x=T)

#length(unique(MareeEch$NAVIRE)) #385 navires �chantillonn�s en 2015, 306 en 2017
#length(unique(MareeEch$ID_MAREE)) #6406 mar�es observ�es en 2015, 2783 en 2017
#sum(CaptureEch$QUANTITE_CAP_VIF[!is.na(CaptureEch$QUANTITE_CAP_VIF)])
#round(sum(CaptureEch$QUANTITE_CAP_VIF)/1000) #833t observ�es au d�barquement, 34t en 2017

### --- Ajout des engins agr�g�s

engins_CAS_absents_ref_engin=setdiff(MareeEch$ENGIN_COD_M,ref_agregation_engins$GEAR_COD)
engins_CAS_absents_ref_engin=engins_CAS_absents_ref_engin[!is.na(engins_CAS_absents_ref_engin) & engins_CAS_absents_ref_engin!=""]
if(length(engins_CAS_absents_ref_engin)>0){
  nb_marees_concernees=length(unique(MareeEch[MareeEch$ENGIN_COD_M%in%engins_CAS_absents_ref_engin,]$ID_MAREE))
  print(paste(length(engins_CAS_absents_ref_engin),"engins CAS absents du r�f�rentiel engins :"))
  print(engins_CAS_absents_ref_engin)
  print(paste("Les ",nb_marees_concernees," mar�es concern�es seront conserv�es.",sep=""))
}
MareeEch=merge(MareeEch,setNames(ref_agregation_engins[,c("GEAR_COD","GEAR_LIB","GR_GEAR_COD","GR_GEAR_LIB")],c("ENGIN_COD","ENGIN_LIB","GR_ENGIN_COD","GR_ENGIN_LIB")),by.x="ENGIN_COD_M",by.y="ENGIN_COD",all.x=T)
MareeEch$ENGIN_COD_M=as.character(MareeEch$ENGIN_COD_M)
MareeEch$GR_ENGIN_COD=as.character(MareeEch$GR_ENGIN_COD)
MareeEch$GR_ENGIN_LIB=as.character(MareeEch$GR_ENGIN_LIB)
MareeEch$GR_ENGIN_COD=replace(MareeEch$GR_ENGIN_COD,is.na(MareeEch$GR_ENGIN_COD),MareeEch[is.na(MareeEch$GR_ENGIN_COD),]$ENGIN_COD_M)
CaptureEch=merge(CaptureEch,unique(MareeEch[,c("METIER_COD_M","GR_ENGIN_COD","GR_ENGIN_LIB")]),by.x="METIER_COD_O",by.y="METIER_COD_M")

### --- Ajout des esp�ces agr�g�es

CaptureEch=merge(CaptureEch[,c("ID_MAREE","ESP_COD_FAO","ESP_LIB_FAO","GR_ENGIN_COD","GR_ENGIN_LIB","QUANTITE_CAP_VIF")],ref_agregation_especes[,c("ESP_COD_FAO","GR_ESP_COD","GR_ESP_LIB")],all.x=T)
CaptureEch$GR_ESP_COD=as.character(CaptureEch$GR_ESP_COD)
CaptureEch$GR_ESP_LIB=as.character(CaptureEch$GR_ESP_LIB)
especes_CAS_absents_ref_especes=setdiff(CaptureEch$ESP_COD_FAO,ref_agregation_especes$ESP_COD_FAO)
if(length(especes_CAS_absents_ref_especes)>0){
  nb_marees_concernees=length(unique(CaptureEch[CaptureEch$ESP_COD_FAO%in%especes_CAS_absents_ref_especes,]$ID_MAREE))
  especes_concernnees=unique(CaptureEch[CaptureEch$ESP_COD_FAO%in%especes_CAS_absents_ref_especes,c("ESP_COD_FAO","ESP_LIB_FAO")])
  print("Esp�ces pr�sentes dans CAS mais absents du r�f�rentiel esp�ces : ")
  print(especes_concernnees)
  print(paste("Les ",nb_marees_concernees," mar�es concern�es seront conserv�es.",sep=""))
  print("2 options :")
  print("a) Mettre � jour le r�f�rentiel des agr�gations esp�ces et relancer.")
  print("b) Continuer le script : les agr�gations seront laiss�es par d�faut � l'esp�ce de base absente du r�f�rentiel.")
}

### --- Importation des mar�es VMS

source("Codes R/consolidation_marees_AlgoPesca.r")

### --- Importation des log-books des Semi-Longliners 

data_SEMI_LL=read.csv(fichier_SEMI_LL,sep=";",dec=".",h=T)
data_SEMI_LL=data_SEMI_LL[data_SEMI_LL$TripYear==annee,]
if (nrow(data_SEMI_LL) > 0) {
  data_SEMI_LL$NatRegNumber=gsub(' ','',as.character(data_SEMI_LL$NatRegNumber))
  data_SEMI_LL$FAOSpeciesCode=gsub(' ','',data_SEMI_LL$SpeciesAcode)
  #Ports de retour = toujours Victoria en 2015, avec un peu de Providence en 2016
  data_SEMI_LL$PORT_COD="LS-VC"
  data_SEMI_LL$PORT_LIB="Victoria"
  
  #Reconstitution des mar�es
  
  # NEW PROCESS
  data_SEMI_LL$DepartureDate=as.character(data_SEMI_LL$DepartureDate)
  data_SEMI_LL$ArrivalDate=as.character(data_SEMI_LL$ArrivalDate)
  data_SEMI_LL$DATE_DEPART=as.Date(format(as.POSIXct(data_SEMI_LL$DepartureDate,"%d/%m/%Y",tz="Europe/Paris"),"%d/%m/%Y"),"%d/%m/%Y")
  data_SEMI_LL$DATE_RETOUR=as.Date(format(as.POSIXct(data_SEMI_LL$ArrivalDate,"%d/%m/%Y",tz="Europe/Paris"),"%d/%m/%Y"),"%d/%m/%Y")
  data_SEMI_LL$Catch_Kg=data_SEMI_LL$GrLogWgtRFinal
  
  # OLD PROCESS
  # data_SEMI_LL$DepartureDate=as.character(data_SEMI_LL$DepartureDate)
  # data_SEMI_LL$ArrivalDate=as.character(data_SEMI_LL$ArrivalDate)
  # 
  # data_SEMI_LL$DepartureDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("f,vr","f?vr",data_SEMI_LL$DepartureDate[x]))
  # data_SEMI_LL$DepartureDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("d,c","d?c",data_SEMI_LL$DepartureDate[x]))
  # data_SEMI_LL$DepartureDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("ao-t","ao?t",data_SEMI_LL$DepartureDate[x]))
  # data_SEMI_LL$ArrivalDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("f,vr","f?vr",data_SEMI_LL$ArrivalDate[x]))
  # data_SEMI_LL$ArrivalDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("d,c","d?c",data_SEMI_LL$ArrivalDate[x]))
  # data_SEMI_LL$ArrivalDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("ao-t","ao?t",data_SEMI_LL$ArrivalDate[x]))
  # data_SEMI_LL$LogDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("f,vr","f?vr",data_SEMI_LL$LogDate[x]))
  # data_SEMI_LL$LogDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("d,c","d?c",data_SEMI_LL$LogDate[x]))
  # data_SEMI_LL$LogDate=sapply(1:nrow(data_SEMI_LL),function(x)gsub("ao-t","ao?t",data_SEMI_LL$LogDate[x]))
  
  # data_SEMI_LL$JOUR_DEPART=sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$DepartureDate[x], '-')[[1]][1])
  # data_SEMI_LL$JOUR_RETOUR=sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$ArrivalDate[x], '-')[[1]][1])
  # data_SEMI_LL$MOIS_DEPART=sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$DepartureDate[x], '-')[[1]][2])
  # data_SEMI_LL$MOIS_RETOUR=sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$ArrivalDate[x], '-')[[1]][2])
  # data_SEMI_LL$ANNEE_DEPART=paste("20",sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$DepartureDate[x], '-')[[1]][3]),sep="")
  # data_SEMI_LL$ANNEE_RETOUR=paste("20",sapply(1:nrow(data_SEMI_LL),function(x)strsplit(data_SEMI_LL$ArrivalDate[x], '-')[[1]][3]),sep="")
  # mois=data.frame(MOIS=c("janv","f?vr","mars","avr","mai","juin","juil","ao?t","sept","oct","nov","d?c"),NUM=1:12)
  # mois$NUM=ifelse(nchar(mois$NUM)==1,paste("0",mois$NUM,sep=""),mois$NUM)
  # data_SEMI_LL=merge(data_SEMI_LL,setNames(mois,c("MOIS","MOIS_DEP")),by.x="MOIS_DEPART",by.y="MOIS")
  # data_SEMI_LL=merge(data_SEMI_LL,setNames(mois,c("MOIS","MOIS_RET")),by.x="MOIS_RETOUR",by.y="MOIS")
  # data_SEMI_LL$DATE_DEPART=as.Date(format(as.POSIXct(paste(data_SEMI_LL$JOUR_DEPART,data_SEMI_LL$MOIS_DEP,data_SEMI_LL$ANNEE_DEPART,sep="/"),"%d/%m/%Y",tz="Europe/Paris"),"%d/%m/%Y"),"%d/%m/%Y")
  # data_SEMI_LL$DATE_RETOUR=as.Date(format(as.POSIXct(paste(data_SEMI_LL$JOUR_RETOUR,data_SEMI_LL$MOIS_RET,data_SEMI_LL$ANNEE_RETOUR,sep="/"),"%d/%m/%Y",tz="Europe/Paris"),"%d/%m/%Y"),"%d/%m/%Y")
  
  marees_SEMI_LL=unique(data_SEMI_LL[,c("TripHistoryID","NatRegNumber","DATE_DEPART","DATE_RETOUR","PORT_COD","PORT_LIB")])
  captures_SEMI_LL=summaryBy(data=data_SEMI_LL,Catch_Kg~TripHistoryID+FAOSpeciesCode+Species,FUN=function(x){if(all(is.na(x))) return(NA); sum(x,na.rm=T)},keep.names=T)
  captures_SEMI_LL$FAOSpeciesCode=as.character(captures_SEMI_LL$FAOSpeciesCode)
  captures_SEMI_LL=merge(captures_SEMI_LL,ref_agregation_especes[,c("ESP_COD_FAO","GR_ESP_COD","GR_ESP_LIB")],by.x="FAOSpeciesCode",by.y="ESP_COD_FAO",all.x=T)
  captures_SEMI_LL=summaryBy(data=captures_SEMI_LL,Catch_Kg~TripHistoryID+GR_ESP_COD+GR_ESP_LIB,FUN=function(x){if(all(is.na(x))) return(NA); sum(x,na.rm=T)},keep.names=T)
  captures_SEMI_LL=captures_SEMI_LL[!is.na(captures_SEMI_LL$GR_ESP_COD),]
  marees_SEMI_LL=merge(marees_SEMI_LL,captures_SEMI_LL,all.x=T)
  #Engin = toujours LLD (Drifting longlines, palangres d�rivantes)
  marees_SEMI_LL$GR_ENGIN_COD="LLD"
  marees_SEMI_LL$GR_ENGIN_LIB="Drifting Longlines"
  marees_SEMI_LL$GR_METIER_COD="LLD_GP"
  marees_SEMI_LL$GR_METIER_LIB="Drifting Longlines for Large Pelagics"
  marees_SEMI_LL$JDM_METIER=round(as.numeric(difftime(marees_SEMI_LL$DATE_RETOUR,marees_SEMI_LL$DATE_DEPART,units="days")))
  marees_SEMI_LL$JDM_METIER=replace(marees_SEMI_LL$JDM_METIER,marees_SEMI_LL$JDM_METIER<1,1)
  marees_SEMI_LL=merge(marees_SEMI_LL,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by.x="NatRegNumber",by.y="NAVIRE",all.x = T)
} else {
  marees_SEMI_LL=setNames(data.frame(matrix(ncol = 15, nrow = 0)), c("NatRegNumber","TripHistoryID","DATE_DEPART","DATE_RETOUR","PORT_COD","PORT_LIB","GR_ESP_COD","GR_ESP_LIB","Catch_Kg","GR_ENGIN_COD","GR_ENGIN_LIB","GR_METIER_COD","GR_METIER_LIB","JDM_METIER","GR_VESSEL_TYPE"))
}

sum(data_SEMI_LL$Catch_Kg)
sum(captures_SEMI_LL$Catch_Kg)
sum(marees_SEMI_LL$Catch_Kg)

### --- Importation des log-books des lobsters

#Lobster = du 01/12 au 29/02, m�tier = Snorkling for Lobsters

if (file.exists(fichier_lobster)) {
  logbooks_lobsters=read.csv(fichier_lobster,sep=";",h=T)
  logbooks_lobsters=logbooks_lobsters[logbooks_lobsters$Year==annee,]
  logbooks_lobsters$NAVIRE=paste("SZ",logbooks_lobsters$SZ,sep="")
  #Renommer quelques ports pour coh�rence avec r�f�rentiel
  logbooks_lobsters$Landing.Site=as.character(logbooks_lobsters$Landing.Site)
  logbooks_lobsters$Landing.Site=replace(logbooks_lobsters$Landing.Site,logbooks_lobsters$Landing.Site=="Cascade","Cascade /Se Island")
  logbooks_lobsters$Landing.Site=replace(logbooks_lobsters$Landing.Site,logbooks_lobsters$Landing.Site=="Grand Anse Praslin","Flying Deutchman/ Grand Anse")
  logbooks_lobsters$QUANTITE=apply(logbooks_lobsters,1,function(x)sum(as.numeric(x[c("Oumar_Rouz_No","Oumar_Ver_No","Grosse_Tete_No","Porcelaine_No")]),na.rm=T))
  logbooks_lobsters$FISHING_DATE=as.Date(as.character(logbooks_lobsters$Fishing_Date),"%d/%m/%Y")
  logbooks_lobsters=merge(logbooks_lobsters,ref_ports[,c("LABEL","NAME")],by.x="Landing.Site",by.y="NAME")
  
  marees_lobsters=setNames(logbooks_lobsters[,c("NAVIRE","FISHING_DATE","LABEL","Landing.Site","QUANTITE")],c("NAVIRE","FISHING_DATE","PORT_COD","PORT_LIB","QUANTITE"))
  marees_lobsters$METIER_COD="HGMVLO"
  marees_lobsters$METIER_LIB="Hand gathering mask and snorkel for Lobsters"
  marees_lobsters=summaryBy(data=marees_lobsters,QUANTITE~NAVIRE+FISHING_DATE+PORT_COD+PORT_LIB+METIER_COD+METIER_LIB,FUN=sum,keep.names=T)
  
  # !!!!! ici �a ne va pas parce qu'il n'y pas de navire
  marees_lobsters=merge(marees_lobsters,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by="NAVIRE")
  marees_lobsters=marees_lobsters[,c("NAVIRE","GR_VESSEL_TYPE","FISHING_DATE","PORT_COD","PORT_LIB","METIER_COD","METIER_LIB","QUANTITE")]
} else {
  marees_lobsters=setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("NAVIRE","GR_VESSEL_TYPE","FISHING_DATE","PORT_COD","PORT_LIB","METIER_COD","METIER_LIB","QUANTITE"))
}

### --- D�termination du port principal d'exploitation des navires : port plus fr�quemment vu selon les diff�rentes sources de donn�es

if (exists("FINSS")&&nrow(FINSS)>0) {
  port_plus_frequent_FINSS=setNames(summaryBy(SIH_LOCATION_COD~SZ,data=unique(FINSS[,c("SZ","SIH_LOCATION_COD","TripID")]),FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_FINSS","NB_PORT_FINSS"))
} else {
  port_plus_frequent_FINSS=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_FINSS","NB_PORT_FINSS"))
}
if (exists("FINSS_deducted")&&nrow(FINSS_deducted)>0) {
  port_plus_frequent_FINSS_deducted=setNames(summaryBy(SIH_LOCATION_COD~SZ,data=unique(FINSS_deducted[,c("SZ","SIH_LOCATION_COD","TripID")]),FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_FINSS","NB_PORT_FINSS"))
} else {
  port_plus_frequent_FINSS_deducted=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_FINSS","NB_PORT_FINSS"))
}
if (exists("MareeEch")&&nrow(MareeEch)>0) {
  port_plus_frequent_CAS=setNames(summaryBy(PORTDEB_COD~IMMATRICULATION,data=MareeEch,FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_CAS","NB_PORT_CAS"))
} else {
  port_plus_frequent_CAS=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_CAS","NB_PORT_CAS"))
}
if (exists("marees_VMS_finales")&&nrow(marees_VMS_finales)>0) {
  port_plus_frequent_VMS=setNames(summaryBy(PORT_RETOUR_COD~NAVIRE,data=marees_VMS_finales,FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_VMS","NB_PORT_VMS"))
} else {
  port_plus_frequent_VMS=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_VMS","NB_PORT_VMS"))
}
if (exists("marees_SEMI_LL") && nrow(marees_SEMI_LL)>0) {
  port_plus_frequent_SEMI_LL=setNames(summaryBy(PORT_COD~NatRegNumber,data=marees_SEMI_LL,FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_SEMI_LL","NB_PORT_SEMI_LL"))
} else {
  port_plus_frequent_SEMI_LL=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_SEMI_LL","NB_PORT_SEMI_LL"))
}
if (exists("marees_lobsters") && nrow(marees_lobsters)>0) {
  port_plus_frequent_LOBSTERS=setNames(summaryBy(PORT_COD~NAVIRE,data=marees_lobsters,FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","PORT_LOBSTERS","NB_PORT_LOBSTERS"))
} else {
  port_plus_frequent_LOBSTERS=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("NAVIRE","PORT_LOBSTERS","NB_PORT_LOBSTERS"))
}
#merge all
port_plus_frequent=merge(merge(merge(merge(merge(port_plus_frequent_FINSS,port_plus_frequent_FINSS_deducted,all=TRUE),port_plus_frequent_CAS,all=TRUE),port_plus_frequent_VMS,all=TRUE),port_plus_frequent_SEMI_LL,all=T),port_plus_frequent_LOBSTERS,all=T)
port_plus_frequent$NB_PORT_FINSS=as.numeric(as.character(port_plus_frequent$NB_PORT_FINSS))
port_plus_frequent$NB_PORT_CAS=as.numeric(as.character(port_plus_frequent$NB_PORT_CAS))
port_plus_frequent$NB_PORT_VMS=as.numeric(as.character(port_plus_frequent$NB_PORT_VMS))
port_plus_frequent$NB_PORT_SEMI_LL=as.numeric(as.character(port_plus_frequent$NB_PORT_SEMI_LL))
port_plus_frequent$NB_PORT_LOBSTERS=as.numeric(as.character(port_plus_frequent$NB_PORT_LOBSTERS))
port_plus_frequent$PORT_FINSS=as.character(port_plus_frequent$PORT_FINSS)
port_plus_frequent$PORT_CAS=as.character(port_plus_frequent$PORT_CAS)
port_plus_frequent$PORT_VMS=as.character(port_plus_frequent$PORT_VMS)
port_plus_frequent$PORT_SEMI_LL=as.character(port_plus_frequent$PORT_SEMI_LL)
port_plus_frequent$PORT_LOBSTERS=as.character(port_plus_frequent$PORT_LOBSTERS)
port_plus_frequent$NB_PORT_FINSS=replace(port_plus_frequent$NB_PORT_FINSS,is.na(port_plus_frequent$NB_PORT_FINSS),0)
port_plus_frequent$NB_PORT_CAS=replace(port_plus_frequent$NB_PORT_CAS,is.na(port_plus_frequent$NB_PORT_CAS),0)
port_plus_frequent$NB_PORT_VMS=replace(port_plus_frequent$NB_PORT_VMS,is.na(port_plus_frequent$NB_PORT_VMS),0)
port_plus_frequent$NB_PORT_SEMI_LL=replace(port_plus_frequent$NB_PORT_SEMI_LL,is.na(port_plus_frequent$NB_PORT_SEMI_LL),0)
port_plus_frequent$NB_PORT_LOBSTERS=replace(port_plus_frequent$NB_PORT_LOBSTERS,is.na(port_plus_frequent$NB_PORT_LOBSTERS),0)
port_plus_frequent$PORT_EXP=apply(port_plus_frequent,1,function(x){ind=names(which.max(x[c("NB_PORT_FINSS","NB_PORT_CAS","NB_PORT_VMS","NB_PORT_SEMI_LL","NB_PORT_LOBSTERS")])[1]);ifelse(ind=="NB_PORT_CAS",x["PORT_CAS"],ifelse(ind=="NB_PORT_FINSS",x["PORT_FINSS"],ifelse(ind=="NB_PORT_SEMI_LL",x["PORT_SEMI_LL"],ifelse(ind=="NB_PORT_LOBSTERS",x["PORT_LOBSTERS"],x["PORT_VMS"]))))})

### --- Importation des log-books des sea cucumbers 

#Sea cucumber = du 01/10 au 30/06, m?tier = Scuba diving for Sea cucumbers

if (file.exists(fichier_sea_cucumbers)) {
  logbooks_sea_cucumbers=read.csv(fichier_sea_cucumbers,sep=";",h=T)
  logbooks_sea_cucumbers=logbooks_sea_cucumbers[logbooks_sea_cucumbers$Year==annee,]
  logbooks_sea_cucumbers$NAVIRE=paste("SZ",logbooks_sea_cucumbers$SZ.Number,sep="")
  
  # date mal format�e
  #logbooks_sea_cucumbers$FishingDate=as.character(logbooks_sea_cucumbers$FishingDate)
  #logbooks_sea_cucumbers$FISHING_DAY=sapply(1:nrow(logbooks_sea_cucumbers),function(x)strsplit(logbooks_sea_cucumbers$FishingDate[x], '-')[[1]][1])
  #logbooks_sea_cucumbers$FISHING_MONTH=sapply(1:nrow(logbooks_sea_cucumbers),function(x)strsplit(logbooks_sea_cucumbers$FishingDate[x], '-')[[1]][2])
  #logbooks_sea_cucumbers$FISHING_YEAR=paste("20",sapply(1:nrow(logbooks_sea_cucumbers),function(x)strsplit(logbooks_sea_cucumbers$FishingDate[x], '-')[[1]][3]),sep="")
  #mois=data.frame(MOIS=c("janv","f?vr","mars","avr","mai","juin","juil","ao?t","sept","oct","nov","d?c"),NUM=1:12)
  #mois$NUM=ifelse(nchar(mois$NUM)==1,paste("0",mois$NUM,sep=""),mois$NUM)
  #logbooks_sea_cucumbers=merge(logbooks_sea_cucumbers,setNames(mois,c("MOIS","MOIS_PECHE")),by.x="FISHING_MONTH",by.y="MOIS")
  #logbooks_sea_cucumbers$FISHING_DATE=as.Date(format(as.POSIXct(paste(logbooks_sea_cucumbers$FISHING_DAY,logbooks_sea_cucumbers$MOIS_PECHE,logbooks_sea_cucumbers$FISHING_YEAR,sep="/"),"%d/%m/%Y",tz="Europe/Paris"),"%d/%m/%Y"),"%d/%m/%Y")
  
  # date bien format�e
  logbooks_sea_cucumbers$FISHING_DATE=as.Date(as.character(logbooks_sea_cucumbers$FishingDate),"%d/%m/%Y")
  
}
if (exists("logbooks_sea_cucumbers") && nrow(logbooks_sea_cucumbers)>0) {
  marees_sea_cucumbers=setNames(summaryBy(data=logbooks_sea_cucumbers,Total.Catch~NAVIRE+FISHING_DATE,FUN=sum,keep.names=T),c("NAVIRE","FISHING_DATE","QUANTITE"))
  marees_sea_cucumbers$METIER_COD="HGSCUX"
  marees_sea_cucumbers$METIER_LIB="Hand gathering scuba for Sea Cucumbers"
} else {
  marees_sea_cucumbers=setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("NAVIRE","FISHING_DATE","QUANTITE","METIER_COD","METIER_LIB"))
}
#Manque le port de d�barquement => affecter le port le plus fr�quent des navires 
marees_sea_cucumbers=merge(marees_sea_cucumbers,setNames(port_plus_frequent[,c("NAVIRE","PORT_EXP")],c("NAVIRE","PORT_COD")),all.x=T)
marees_sea_cucumbers=merge(marees_sea_cucumbers,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by="NAVIRE")

### --- Importation des �chantillons droplines

droplines=read.csv(fichier_droplines,sep=";",h=T)
droplines$NAVIRE=paste("SZ",droplines$VesselSZ,sep="")
droplines$DepartureDate=as.Date(as.character(droplines$DepartureDate),"%d/%m/%Y")
droplines$ArrivalDate=as.Date(as.character(droplines$ArrivalDate),"%d/%m/%Y")
droplines$ANNEE=format(droplines$ArrivalDate,"%Y")
droplines=droplines[droplines$ANNEE==annee,]
setdiff(droplines$NAVIRE,fichier_flotte$NAVIRE)
droplines=merge(droplines,ref_agregation_especes[,c("ESP_COD_FAO","GR_ESP_COD","GR_ESP_LIB")],by.x="SpeciesFAOCode",by.y="ESP_COD_FAO")
droplines$Gear1=as.character(droplines$Gear1)
droplines$Gear2=as.character(droplines$Gear2)

#Mar�es avec engin "NK" (inconnu) => affecter engin + fr�quent du navire
if("NK"%in%droplines$Gear1){
  nav_NK=data.frame(unique(droplines[droplines$Gear1%in%"NK",][,c("VesselSZ","TripID")]),GEAR=NA)
  nav_NK$GEAR=sapply(1:nrow(nav_NK),function(x)names(rev(sort(table(as.character(droplines[droplines$VesselSZ==nav_NK$VesselSZ[x] & !droplines$Gear1%in%"NK",]$Gear1))))[1]))
  for(mareeNK in nav_NK$TripID) droplines[droplines$TripID==mareeNK,]$Gear1=rep(nav_NK[nav_NK$TripID==mareeNK,]$GEAR,length(droplines[droplines$TripID==mareeNK,]$Gear1))
}

#Ajout des groupes esp�ces et somme des quantit�s par mar�e
if (nrow(droplines)>0)
  droplines=summaryBy(data=droplines,CatchWeight~TripID+NAVIRE+DepartureDate+ArrivalDate+Gear1+Gear2+GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)

# A) Gestion des mar�es multi-engins (lorsque plusieurs engins par mar�e) => r�gle d'affectation des esp�ces aux engins :
#    1) LH + LLB => s�parer captures en 2 et les r�affecter � chaque engin
#    2) GHT + LLB (ou LH) => affecte automatiquement RAQ (crabe) + MZZ de l'ensemble de la mar�e � GHT. Isoler ce m�tier GHTRAQ de tout le reste = m�tier LLBDEM (poissons sans MZZ)
# B) Gestion des mar�es mono-engin : rester au niveau de l'engin d�clar� : LLB ou LH
marees_monoengin=droplines[droplines$Gear2=="",c("TripID","NAVIRE","DepartureDate","ArrivalDate","Gear1","GR_ESP_COD","GR_ESP_LIB","CatchWeight")]
marees_multiengin=droplines[droplines$Gear2!="",]
id_marees_multiengin=unique(marees_multiengin$TripID)
marees_multiengin_new=c()
for(maree in id_marees_multiengin){
  tmp=marees_multiengin[marees_multiengin$TripID==maree,]
  gears=c(unique(tmp$Gear1),unique(tmp$Gear2))
  if(all(c("LLB","LH")%in%gears)){
    marees_multiengin_new=rbind(marees_multiengin_new,rbind(data.frame(TripID=tmp$TripID,NAVIRE=tmp$NAVIRE,DepartureDate=tmp$DepartureDate,ArrivalDate=tmp$ArrivalDate,Gear1="LLB",GR_ESP_COD=tmp$GR_ESP_COD,GR_ESP_LIB=tmp$GR_ESP_LIB,CatchWeight=tmp$CatchWeight/2),data.frame(TripID=tmp$TripID,NAVIRE=tmp$NAVIRE,DepartureDate=tmp$DepartureDate,ArrivalDate=tmp$ArrivalDate,Gear1="LH",GR_ESP_COD=tmp$GR_ESP_COD,GR_ESP_LIB=tmp$GR_ESP_LIB,CatchWeight=tmp$CatchWeight/2)))
    next
  }
  if("GHT"%in%gears){
    tmp1=tmp[tmp$GR_ESP_COD%in%c("RAQ","MZZ"),]
    tmp2=tmp[!tmp$GR_ESP_COD%in%c("RAQ","MZZ"),]
    marees_multiengin_new=rbind(marees_multiengin_new,rbind(data.frame(TripID=tmp$TripID[1],NAVIRE=tmp$NAVIRE[1],DepartureDate=tmp$DepartureDate[1],ArrivalDate=tmp$ArrivalDate[1],Gear1="GHT",GR_ESP_COD=tmp1$GR_ESP_COD,GR_ESP_LIB=tmp1$GR_ESP_LIB,CatchWeight=tmp1$CatchWeight),data.frame(TripID=tmp$TripID[1],NAVIRE=tmp$NAVIRE[1],DepartureDate=tmp$DepartureDate[1],ArrivalDate=tmp$ArrivalDate[1],Gear1="LLB",GR_ESP_COD=tmp2$GR_ESP_COD,GR_ESP_LIB=tmp2$GR_ESP_LIB,CatchWeight=tmp2$CatchWeight)))
    next
  }
}
marees_droplines=rbind(marees_monoengin,marees_multiengin_new)
#Ajout du m�tier regroup�
marees_droplines=merge(marees_droplines,ref_agregation_metiers[,c("GR_GEAR_COD","GR_ESP_COD","GR_METIER_COD","GR_METIER_LIB")],by.x=c("Gear1","GR_ESP_COD"),by.y=c("GR_GEAR_COD","GR_ESP_COD"),all.x=T)
if(any(is.na(marees_droplines$GR_METIER_COD))){
  print("Log-books Droplines : combinaisons engin/esp�ces inconnues dans le r�f�rentiel des m�tiers agr�g�s :")
  print(unique(marees_droplines[is.na(marees_droplines$GR_METIER_COD),c("Gear1","GR_ESP_COD","GR_ESP_LIB")]))
  print("Mettre � jour le r�f�rentiel d'agr�gation des m�tiers.")
}
#Manque le port de d�barquement => affecter le port le plus fr�quent des navires 
marees_droplines=merge(marees_droplines,setNames(port_plus_frequent[,c("NAVIRE","PORT_EXP")],c("NAVIRE","PORT_COD")),all.x=T)

#Affectation des JDM � chaque m�tier : au prorata des captures par mar�e
if (nrow(marees_droplines)>0) {
  marees_droplines$JDM=as.numeric(marees_droplines$ArrivalDate-marees_droplines$DepartureDate+1)
  repartition_JDM_droplines=merge(setNames(summaryBy(data=marees_droplines,CatchWeight~TripID+JDM+GR_METIER_COD,FUN=sum,na.rm=T,keep.names=T),c("TripID","JDM","GR_METIER_COD","Q_MET")),setNames(summaryBy(data=marees_droplines,CatchWeight~TripID,FUN=sum,na.rm=T,keep.names=T),c("TripID","Q_TOT")))
  repartition_JDM_droplines$JDM_METIER=round(repartition_JDM_droplines$JDM*repartition_JDM_droplines$Q_MET/repartition_JDM_droplines$Q_TOT)
  if(any(repartition_JDM_droplines$JDM_METIER==0)) repartition_JDM_droplines[repartition_JDM_droplines$JDM_METIER==0,]$JDM_METIER=1
  marees_droplines=merge(marees_droplines,repartition_JDM_droplines[,c("TripID","GR_METIER_COD","JDM_METIER")],all.x=T)

  marees_droplines=marees_droplines[,c("TripID","NAVIRE","DepartureDate","ArrivalDate","PORT_COD","GR_METIER_COD","GR_METIER_LIB","JDM_METIER","Gear1","GR_ESP_COD","GR_ESP_LIB","CatchWeight")]
}

marees_droplines=merge(marees_droplines,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by="NAVIRE")

### --- Confrontation des navires en pr�sence par rapport au fichier flotte

if (exists("marees_droplines") && nrow(marees_droplines)>0) {
  navires_droplines=data.frame(NAVIRE=unique(marees_droplines$NAVIRE),SOURCE_DROPLINES=1)
} else {
  navires_droplines=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_DROPLINES"))
}
if (exists("logbooks_lobsters") && nrow(logbooks_lobsters)>0) {
  navires_lobsters=data.frame(NAVIRE=unique(logbooks_lobsters$NAVIRE),SOURCE_LOBSTERS=1)
} else {
  navires_lobsters=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_LOBSTERS"))
}
if (exists("logbooks_sea_cucumbers") && nrow(logbooks_sea_cucumbers)>0) {
  navires_sea_cucumbers=data.frame(NAVIRE=unique(logbooks_sea_cucumbers$NAVIRE),SOURCE_SEA_CUCUMBER=1)
} else {
  navires_sea_cucumbers=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_SEA_CUCUMBER"))
}
if (exists("marees_SEMI_LL") && nrow(marees_SEMI_LL)>0) {
  navires_SEMI_LL=data.frame(NAVIRE=unique(marees_SEMI_LL$NatRegNumber),SOURCE_SEMI_LL=1)
} else {
  navires_SEMI_LL=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_SEMI_LL"))
}
if (exists("naviresECH")&&nrow(naviresECH)>0) {
  navires_CAS=data.frame(NAVIRE=unique(naviresECH[grepl("SZ",naviresECH$IMMATRICULATION),]$IMMATRICULATION),SOURCE_CAS=1)
} else {
  navires_CAS=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_CAS"))
}
if (exists("FINSS")&&nrow(FINSS)>0) {
  navires_FINSS=data.frame(NAVIRE=unique(FINSS$SZ),SOURCE_FINSS=1)
} else {
  navires_FINSS=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_FINSS"))
}
if (exists("FINSS_deducted")&&nrow(FINSS_deducted)>0) {
  navires_FINSS_deducted=data.frame(NAVIRE=unique(FINSS_deducted$SZ),SOURCE_FINSS=1)
} else {
  navires_FINSS_deducted=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_FINSS"))
}
if (exists("marees_VMS_finales")&&nrow(marees_VMS_finales)>0) {
  navires_VMS=data.frame(NAVIRE=unique(marees_VMS_finales$NAVIRE),SOURCE_VMS=1)
} else {
  navires_VMS=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("NAVIRE","SOURCE_VMS"))
}

navire_all_sources=merge(merge(merge(merge(merge(merge(merge(navires_lobsters,navires_sea_cucumbers,by="NAVIRE",all=T),navires_SEMI_LL,by="NAVIRE",all=T),navires_CAS,by="NAVIRE",all=T),navires_FINSS,by="NAVIRE",all=T),navires_VMS,all=T),navires_droplines,all=T),navires_FINSS_deducted,all=T)
navires_absents_fichier_flotte=navire_all_sources[navire_all_sources$NAVIRE%in%setdiff(navire_all_sources$NAVIRE,fichier_flotte$NAVIRE),]
if(nrow(navires_absents_fichier_flotte)>0){
  print(paste(nrow(navires_absents_fichier_flotte),"navires sont absents du fichier flotte apr�s concat�nation des sources de donn�es :"))
  print(navires_absents_fichier_flotte)
  print("Les navires concern�s seront supprim�s si le fichier flotte n'est pas actualis� pour les r�int�grer.")
}


