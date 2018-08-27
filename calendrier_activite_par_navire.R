
### --- Reconstruction journalière de l'activité

navires=fichier_flotte$NAVIRE

#JDM VMS par navire

if (nrow(marees_VMS_finales)>0) {
  dates_marees_VMS=c()
  for(i in 1:nrow(marees_VMS_finales)){
    tmp=marees_VMS_finales[i,]
    dates_marees_VMS=rbind(dates_marees_VMS,unique(data.frame(NAVIRE=tmp$NAVIRE,DATE_JDM_VMS=format(seq(tmp$DATE_DEPART,tmp$DATE_RETOUR,by="days"),"%Y-%m-%d"),PORT_VMS=tmp$PORT_RETOUR_COD,MAREE_EFFORT_VMS=tmp$MAREE_EFFORT)))
  }
  dates_marees_VMS$DATE_JDM_VMS=as.Date(as.character(dates_marees_VMS$DATE_JDM_VMS))
  dates_marees_VMS$NAVIRE=as.character(dates_marees_VMS$NAVIRE)
  dates_marees_VMS$JDM_VMS=1
  dates_marees_VMS$ID=paste(dates_marees_VMS$NAVIRE,dates_marees_VMS$DATE_JDM_VMS,sep="_")
  dates_marees_VMS=dates_marees_VMS[-which(duplicated(dates_marees_VMS$ID)),c("NAVIRE","DATE_JDM_VMS","JDM_VMS","PORT_VMS","MAREE_EFFORT_VMS")]
} else {
  dates_marees_VMS=setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("NAVIRE","DATE_JDM_VMS","JDM_VMS","PORT_VMS","MAREE_EFFORT_VMS"))
}

#JDM FINSS par navire

if (nrow(FINSS)>0) {
  #Hypothèse d'attribution des jours de mer (JDM) : +1 JDM si LogEvent = LAND, OneDayTrip, OUT, P ARRIVAL, P DEPART (on a 1 journée par LogEvent renseigné)
  FINSS$JDM=0
  FINSS[FINSS$LogEvent%in%c("LAND","OneDayTrip","OUT","P ARRIVAL","P DEPART"),]$JDM=1
  
  dates_marees_FINSS=setNames(unique(FINSS[,c("SZ","LogDate","SIH_LOCATION_COD","JDM")]),c("NAVIRE","DATE_JDM_FINSS","PORT_FINSS","JDM_FINSS")) 
  dates_marees_FINSS$DATE_JDM_FINSS=as.Date(format(dates_marees_FINSS$DATE_JDM_FINSS,"%Y-%m-%d"))
  dates_marees_FINSS=unique(dates_marees_FINSS)
  dates_marees_FINSS$ID=paste(dates_marees_FINSS$NAVIRE,dates_marees_FINSS$DATE_JDM_FINSS,sep="_")
  ind_replicated=dates_marees_FINSS[duplicated(dates_marees_FINSS$ID),]$ID
  dates_marees_FINSS[dates_marees_FINSS$ID%in%ind_replicated,]$JDM_FINSS=1
  dates_marees_FINSS=unique(dates_marees_FINSS[,c("NAVIRE","DATE_JDM_FINSS","JDM_FINSS","PORT_FINSS")])
} else {
  dates_marees_FINSS=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("NAVIRE","DATE_JDM_FINSS","JDM_FINSS","PORT_FINSS"))
}

#JDM FINSS 'reconstitué' par navire

if (nrow(FINSS_deducted)>0) {
  #Hypothèse d'attribution des jours de mer (JDM) : +1 JDM si LogEvent = DL, DP, LD, OUT (on a 1 journée par LogEvent renseigné)
  FINSS_deducted$JDM=0
  FINSS_deducted[FINSS_deducted$LogEvent%in%c("DL","DP","LD","OUT"),]$JDM=1
  
  dates_marees_FINSS_deducted=setNames(unique(FINSS_deducted[,c("SZ","LogDate","SIH_LOCATION_COD","JDM")]),c("NAVIRE","DATE_JDM_FINSS","PORT_FINSS","JDM_FINSS")) 
  dates_marees_FINSS_deducted$DATE_JDM_FINSS=as.Date(format(dates_marees_FINSS_deducted$DATE_JDM_FINSS,"%Y-%m-%d"))
  dates_marees_FINSS_deducted=unique(dates_marees_FINSS_deducted)
  dates_marees_FINSS_deducted$ID=paste(dates_marees_FINSS_deducted$NAVIRE,dates_marees_FINSS_deducted$DATE_JDM_FINSS,sep="_")
  ind_replicated=dates_marees_FINSS_deducted[duplicated(dates_marees_FINSS_deducted$ID),]$ID
  dates_marees_FINSS_deducted[dates_marees_FINSS_deducted$ID%in%ind_replicated,]$JDM_FINSS=1
  dates_marees_FINSS_deducted=unique(dates_marees_FINSS_deducted[,c("NAVIRE","DATE_JDM_FINSS","JDM_FINSS","PORT_FINSS")])
} else {
  dates_marees_FINSS_deducted=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("NAVIRE","DATE_JDM_FINSS","JDM_FINSS","PORT_FINSS"))
}

#JDM CAS par navire
if (exists("MareeEch")&&nrow(MareeEch)>0) {
  dates_marees_CAS0=setNames(unique(MareeEch[,c("IMMATRICULATION","DATE_DEPART","DATE_RETOUR","PORTDEB_COD")]),c("NAVIRE","DATE_DEPART","DATE_RETOUR","PORTDEB_COD"))
  dates_marees_CAS=c()
  for(i in 1:nrow(dates_marees_CAS0)){
    tmp=dates_marees_CAS0[i,]
    dates_marees_CAS=rbind(dates_marees_CAS,data.frame(NAVIRE=tmp$NAVIRE,DATE_JDM_CAS=seq(as.Date(tmp$DATE_DEPART),as.Date(tmp$DATE_RETOUR),by=1),PORT_CAS=tmp$PORTDEB_COD))
  }
  dates_marees_CAS$DATE_JDM_CAS=as.Date(as.character(dates_marees_CAS$DATE_JDM_CAS))
  dates_marees_CAS$NAVIRE=as.character(dates_marees_CAS$NAVIRE)
  dates_marees_CAS$JDM_CAS=1
  dates_marees_CAS$ID=paste(dates_marees_CAS$NAVIRE,dates_marees_CAS$DATE_JDM_CAS,sep="_")
  dates_marees_CAS=dates_marees_CAS[-which(duplicated(dates_marees_CAS$ID)),]
  dates_marees_CAS=unique(dates_marees_CAS[,c("NAVIRE","DATE_JDM_CAS","JDM_CAS","PORT_CAS")])
} else {
  dates_marees_CAS=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("NAVIRE","DATE_JDM_CAS","JDM_CAS","PORT_CAS"))
}

### --- Ajout des log-books des Semi-Longliners 

if (exists("marees_SEMI_LL")&&nrow(marees_SEMI_LL)>0) {
  dates_marees_SEMI_LL=c()
  for(i in 1:nrow(marees_SEMI_LL)){
    tmp=marees_SEMI_LL[i,]
    dates_marees_SEMI_LL=rbind(dates_marees_SEMI_LL,data.frame(NAVIRE=tmp$NatRegNumber,DATE_JDM_SEMI_LL=format(seq(tmp$DATE_DEPART,tmp$DATE_RETOUR,by="days"),"%Y-%m-%d"),PORT_SEMI_LL=tmp$PORT_COD))
  }
  dates_marees_SEMI_LL$DATE_JDM_SEMI_LL=as.Date(as.character(dates_marees_SEMI_LL$DATE_JDM_SEMI_LL))
  dates_marees_SEMI_LL$NAVIRE=as.character(dates_marees_SEMI_LL$NAVIRE)
  dates_marees_SEMI_LL$JDM_SEMI_LL=1
  dates_marees_SEMI_LL$ID=paste(dates_marees_SEMI_LL$NAVIRE,dates_marees_SEMI_LL$DATE_JDM_SEMI_LL,sep="_")
  dates_marees_SEMI_LL=dates_marees_SEMI_LL[-which(duplicated(dates_marees_SEMI_LL$ID)),]
  dates_marees_SEMI_LL=unique(dates_marees_SEMI_LL[,c("NAVIRE","DATE_JDM_SEMI_LL","JDM_SEMI_LL","PORT_SEMI_LL")])
} else {
  dates_marees_SEMI_LL=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("NAVIRE","DATE_JDM_SEMI_LL","JDM_SEMI_LL","PORT_SEMI_LL"))
}

### --- Ajout des log-books Lobsters

if (exists("marees_lobsters")&&nrow(marees_lobsters)>0) {
  dates_marees_LOBSTERS=setNames(unique(marees_lobsters[,c("NAVIRE","FISHING_DATE","PORT_COD")]),c("NAVIRE","DATE_JDM_LOBSTERS","PORT_LOBSTERS"))
  dates_marees_LOBSTERS$DATE_JDM_LOBSTERS=as.Date(as.character(dates_marees_LOBSTERS$DATE_JDM_LOBSTERS))
  dates_marees_LOBSTERS$NAVIRE=as.character(dates_marees_LOBSTERS$NAVIRE)
  dates_marees_LOBSTERS$JDM_LOBSTERS=1
} else {
  dates_marees_LOBSTERS=setNames(data.frame(matrix(ncol = 4, nrow = 0)),c("NAVIRE","DATE_JDM_LOBSTERS","JDM_LOBSTERS","PORT_LOBSTERS"))
}

### --- Ajout des log-books Sea Cucumbers

if (exists("marees_sea_cucumbers")&&nrow(marees_sea_cucumbers)>0) {
  dates_marees_SEA_CUCUMBER=setNames(unique(marees_sea_cucumbers[,c("NAVIRE","FISHING_DATE","PORT_COD")]),c("NAVIRE","DATE_JDM_SEA_CUCUMBER","PORT_SEA_CUCUMBER"))
  dates_marees_SEA_CUCUMBER$DATE_JDM_SEA_CUCUMBER=as.Date(as.character(dates_marees_SEA_CUCUMBER$DATE_JDM_SEA_CUCUMBER))
  dates_marees_SEA_CUCUMBER$NAVIRE=as.character(dates_marees_SEA_CUCUMBER$NAVIRE)
  dates_marees_SEA_CUCUMBER$JDM_SEA_CUCUMBER=1
} else {
  dates_marees_SEA_CUCUMBER=setNames(data.frame(matrix(ncol = 4, nrow = 0)),c("NAVIRE","DATE_JDM_SEA_CUCUMBER","JDM_SEA_CUCUMBER","PORT_SEA_CUCUMBER"))
}

### --- Ajout des log-books Droplines
if (exists("marees_droplines")&&nrow(marees_droplines)>0) {
  marees_droplines0=unique(marees_droplines[,c("TripID","NAVIRE","DepartureDate","ArrivalDate","PORT_COD")])
  dates_marees_DROPLINES=c()
  for(i in 1:nrow(marees_droplines0)){
    tmp=marees_droplines0[i,]
    dates_marees_DROPLINES=rbind(dates_marees_DROPLINES,data.frame(NAVIRE=tmp$NAVIRE,DATE_JDM_DROPLINES=format(seq(tmp$DepartureDate,tmp$ArrivalDate,by="days"),"%Y-%m-%d"),PORT_DROPLINES=tmp$PORT_COD))
  }
  dates_marees_DROPLINES=unique(dates_marees_DROPLINES)
  dates_marees_DROPLINES$DATE_JDM_DROPLINES=as.Date(as.character(dates_marees_DROPLINES$DATE_JDM_DROPLINES))
  dates_marees_DROPLINES$NAVIRE=as.character(dates_marees_DROPLINES$NAVIRE)
  dates_marees_DROPLINES$JDM_DROPLINES=1
} else {
  dates_marees_DROPLINES=setNames(data.frame(matrix(ncol = 4, nrow = 0)),c("NAVIRE","DATE_JDM_DROPLINES","JDM_DROPLINES","PORT_DROPLINES"))
}

# on remplace les dates de marées FINSS par celle de FINSS_DEDUCTED
# si FINSS est vide et pas FINSS_DEDUCTED
# TODO: comment faire pour merge les 2 si non vides ?
if (nrow(dates_marees_FINSS)==0 && nrow(dates_marees_FINSS_deducted)>0) {
  dates_marees_FINSS=dates_marees_FINSS_deducted
}
if (nrow(FINSS)==0 && nrow(FINSS_deducted)>0) {
  FINSS=FINSS_deducted
}


### --- Concaténation des sources, par navire*jour

calendrier_journalier=merge(data.frame(NAVIRE=navires),data.frame(JOUR=seq(as.Date(paste(annee,"-01-01",sep="")),as.Date(paste(annee,"-12-31",sep="")),by="day")),all=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_VMS,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_VMS"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_CAS,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_CAS"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_FINSS,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_FINSS"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_SEMI_LL,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_SEMI_LL"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_LOBSTERS,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_LOBSTERS"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_SEA_CUCUMBER,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_SEA_CUCUMBER"),all.x=T)
calendrier_journalier=merge(calendrier_journalier,dates_marees_DROPLINES,by.x=c("NAVIRE","JOUR"),by.y=c("NAVIRE","DATE_JDM_DROPLINES"),all.x=T)

calendrier_journalier$MOIS=as.numeric(format(calendrier_journalier$JOUR,"%m"))
calendrier_journalier$JDM_FINAL=1



# -- Hypothèse structurante sur les jours d'inactivité : inactivité un jour donné si :
# a) ni FINSS ni CAS ni VMS ni JDM_SEMI_LL ni JDM_LOBSTERS ni JDM_SEA_CUCUMBER ni JDM_DROPLINES
# b) FINSS = 0, ni CAS ni VMS ni JDM_SEMI_LL ni JDM_LOBSTERS ni JDM_SEA_CUCUMBER ni JDM_DROPLINES
# c) marée VMS sans effort, ni FINSS ni CAS ni JDM_SEMI_LL ni JDM_LOBSTERS ni JDM_SEA_CUCUMBER ni JDM_DROPLINES
# d) marée VMS sans effort, FINSS = 0 et pas de CAS ni JDM_SEMI_LL ni JDM_LOBSTERS ni JDM_SEA_CUCUMBER ni JDM_DROPLINES

ind_inactivite=which((is.na(calendrier_journalier$JDM_VMS) & is.na(calendrier_journalier$JDM_CAS) & is.na(calendrier_journalier$JDM_FINSS) & is.na(calendrier_journalier$JDM_SEMI_LL) & is.na(calendrier_journalier$JDM_LOBSTERS) & is.na(calendrier_journalier$JDM_SEA_CUCUMBER) & is.na(calendrier_journalier$JDM_DROPLINES)) | 
                     (is.na(calendrier_journalier$JDM_VMS) & is.na(calendrier_journalier$JDM_CAS) & is.na(calendrier_journalier$JDM_SEMI_LL) & is.na(calendrier_journalier$JDM_LOBSTERS) & is.na(calendrier_journalier$JDM_SEA_CUCUMBER) & is.na(calendrier_journalier$JDM_DROPLINES) & !is.na(calendrier_journalier$JDM_FINSS) & calendrier_journalier$JDM_FINSS==0) |
                     (!is.na(calendrier_journalier$JDM_VMS) & is.na(calendrier_journalier$JDM_CAS) & is.na(calendrier_journalier$JDM_FINSS) & is.na(calendrier_journalier$JDM_SEMI_LL) & is.na(calendrier_journalier$JDM_LOBSTERS) & is.na(calendrier_journalier$JDM_SEA_CUCUMBER) & is.na(calendrier_journalier$JDM_DROPLINES) & calendrier_journalier$JDM_VMS==1 & calendrier_journalier$MAREE_EFFORT_VMS==0) |
                     (!is.na(calendrier_journalier$JDM_VMS) & is.na(calendrier_journalier$JDM_CAS) & is.na(calendrier_journalier$JDM_SEMI_LL) & is.na(calendrier_journalier$JDM_LOBSTERS) & is.na(calendrier_journalier$JDM_SEA_CUCUMBER) & is.na(calendrier_journalier$JDM_DROPLINES) & !is.na(calendrier_journalier$JDM_FINSS) & calendrier_journalier$JDM_FINSS==0 & calendrier_journalier$JDM_VMS==1 & calendrier_journalier$MAREE_EFFORT_VMS==0))

calendrier_journalier[ind_inactivite,]$JDM_FINAL=0

calendrier_journalier$PORT_FINSS=as.character(calendrier_journalier$PORT_FINSS)
calendrier_journalier$PORT_CAS=as.character(calendrier_journalier$PORT_CAS)
calendrier_journalier$PORT_VMS=as.character(calendrier_journalier$PORT_VMS)
calendrier_journalier$PORT_SEMI_LL=as.character(calendrier_journalier$PORT_SEMI_LL)
calendrier_journalier$PORT_LOBSTERS=as.character(calendrier_journalier$PORT_LOBSTERS)
calendrier_journalier$PORT_SEA_CUCUMBER=as.character(calendrier_journalier$PORT_SEA_CUCUMBER)
calendrier_journalier$PORT_DROPLINES=as.character(calendrier_journalier$PORT_DROPLINES)

### --- Affectation d'un port à chaque journée avec info : LB_SEMI_LL, DROPLINES, LB_LOBSTERS, CAS, FINSS, VMS, LB_SEA_CUCUMBER

calendrier_journalier$PORT=ifelse(!is.na(calendrier_journalier$PORT_SEMI_LL),calendrier_journalier$PORT_SEMI_LL,ifelse(!is.na(calendrier_journalier$PORT_DROPLINES),calendrier_journalier$PORT_DROPLINES,ifelse(!is.na(calendrier_journalier$PORT_LOBSTERS),calendrier_journalier$PORT_LOBSTERS,ifelse(!is.na(calendrier_journalier$PORT_CAS),calendrier_journalier$PORT_CAS,ifelse(!is.na(calendrier_journalier$PORT_FINSS),calendrier_journalier$PORT_FINSS,ifelse(!is.na(calendrier_journalier$PORT_VMS),calendrier_journalier$PORT_VMS,calendrier_journalier$PORT_SEA_CUCUMBER))))))
#Si pas d'info via FINSS ni LB_SEMI_LL ni DROPLINES ni LB_SEA_CUCUMBER ni LB_LOBSTERS mais info via CAS et VMS où port_CAS inconnu, alors prendre port VMS
ind=which(is.na(calendrier_journalier$PORT_FINSS) & is.na(calendrier_journalier$PORT_SEMI_LL) & is.na(calendrier_journalier$PORT_DROPLINES) & is.na(calendrier_journalier$PORT_SEA_CUCUMBER) & is.na(calendrier_journalier$PORT_LOBSTERS) & !is.na(calendrier_journalier$PORT_CAS) & calendrier_journalier$PORT_CAS=="LS-??" & !is.na(calendrier_journalier$PORT_VMS))
calendrier_journalier[ind,]$PORT=calendrier_journalier[ind,]$PORT_VMS

### --- Ajout de l'info si le seul JDM présent est un JDM issu d'une source exhaustive : VMS ou log-books (SEMI_LL, LOBSTERS, SEA_CUCUMBER) (car l'extrapolation des JDM en dépend) 

calendrier_journalier$JDM_SOURCE_EXHAUSTIVE=FALSE
ind_only_JDM_SOURCE_EXHAUSTIIVE=which((!is.na(calendrier_journalier$JDM_VMS) | !is.na(calendrier_journalier$JDM_SEMI_LL) | !is.na(calendrier_journalier$JDM_LOBSTERS)  | !is.na(calendrier_journalier$JDM_SEA_CUCUMBER)) & calendrier_journalier$JDM_FINAL==1)
if (length(ind_only_JDM_SOURCE_EXHAUSTIIVE)>0) {
  calendrier_journalier[ind_only_JDM_SOURCE_EXHAUSTIIVE,]$JDM_SOURCE_EXHAUSTIVE=TRUE
}
          
### --- Ajout métiers + quantités capturées des marées CAS échantillonnées

if (exists("marees_CAS")&&nrow(marees_CAS)>0) {
  captures_CAS=summaryBy(data=marees_CAS,QUANTITE_CAP_VIF~ID_MAREE+IMMATRICULATION+DATE_RETOUR+GR_METIER_COD+GR_METIER_LIB+JDM_METIER,FUN=sum,na.rm=T,keep.names=T)
  captures_CAS=setNames(captures_CAS[,c("IMMATRICULATION","DATE_RETOUR","GR_METIER_COD","GR_METIER_LIB","QUANTITE_CAP_VIF","JDM_METIER")],c("NAVIRE","JOUR","METIER_CAS_COD","METIER_CAS_LIB","QUANTITE_CAS","JDM_METIER_CAS"))
} else {
  captures_CAS=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("NAVIRE","JOUR","METIER_CAS_COD","METIER_CAS_LIB","QUANTITE_CAS","JDM_METIER_CAS"))
}
calendrier_journalier=merge(calendrier_journalier,captures_CAS,by=c("NAVIRE","JOUR"),all.x=T)
calendrier_journalier$METIER_CAS_COD=as.character(calendrier_journalier$METIER_CAS_COD)
calendrier_journalier$METIER_CAS_LIB=as.character(calendrier_journalier$METIER_CAS_LIB)

### --- Ajout métiers + quantités capturées des Droplines échantillonnés

if (exists("marees_droplines")&&nrow(marees_droplines)>0) {
  marees_captures_DROPLINES=summaryBy(data=marees_droplines,CatchWeight~TripID+NAVIRE+ArrivalDate+GR_METIER_COD+GR_METIER_LIB+JDM_METIER,FUN=sum,keep.names=T)
  marees_captures_DROPLINES=setNames(marees_captures_DROPLINES[,c("NAVIRE","ArrivalDate","GR_METIER_COD","GR_METIER_LIB","JDM_METIER","CatchWeight")],c("NAVIRE","JOUR","METIER_DROPLINES_COD","METIER_DROPLINES_LIB","JDM_METIER_DROPLINES","QUANTITE_DROPLINES"))
} else {
  marees_captures_DROPLINES=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("NAVIRE","JOUR","METIER_DROPLINES_COD","METIER_DROPLINES_LIB","JDM_METIER_DROPLINES","QUANTITE_DROPLINES"))
}
calendrier_journalier=merge(calendrier_journalier,marees_captures_DROPLINES,by=c("NAVIRE","JOUR"),all.x=T)
calendrier_journalier$METIER_DROPLINES_COD=as.character(calendrier_journalier$METIER_DROPLINES_COD)
calendrier_journalier$METIER_DROPLINES_LIB=as.character(calendrier_journalier$METIER_DROPLINES_LIB)

### --- Ajout métiers + quantités capturées des log-books Semi Longliners

if (exists("marees_SEMI_LL")&&nrow(marees_SEMI_LL)>0) {
  marees_captures_SEMI_LL=summaryBy(data=marees_SEMI_LL,Catch_Kg~TripHistoryID+NatRegNumber+DATE_RETOUR+GR_METIER_COD+GR_METIER_LIB+JDM_METIER,FUN=function(x){if(all(is.na(x))) return(NA); sum(x,na.rm=T)},keep.names = T)
  marees_captures_SEMI_LL=setNames(marees_captures_SEMI_LL[,c("NatRegNumber","DATE_RETOUR","GR_METIER_COD","GR_METIER_LIB","Catch_Kg","JDM_METIER")],c("NAVIRE","JOUR","METIER_SEMI_LL_COD","METIER_SEMI_LL_LIB","QUANTITE_SEMI_LL","JDM_METIER_SEMI_LL"))
} else {
  marees_captures_SEMI_LL=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("NAVIRE","JOUR","METIER_SEMI_LL_COD","METIER_SEMI_LL_LIB","QUANTITE_SEMI_LL","JDM_METIER_SEMI_LL"))
}
calendrier_journalier=merge(calendrier_journalier,marees_captures_SEMI_LL,by=c("NAVIRE","JOUR"),all.x=T)
calendrier_journalier$METIER_SEMI_LL_COD=as.character(calendrier_journalier$METIER_SEMI_LL_COD)
calendrier_journalier$METIER_SEMI_LL_LIB=as.character(calendrier_journalier$METIER_SEMI_LL_LIB)

### --- Ajout métiers + quantités capturées des log-books Lobsters

if (exists("marees_lobsters")&&nrow(marees_lobsters)>0) {
  marees_captures_LOBSTERS=setNames(marees_lobsters[,c("NAVIRE","FISHING_DATE","METIER_COD","METIER_LIB","QUANTITE")],c("NAVIRE","JOUR","METIER_LOBSTERS_COD","METIER_LOBSTERS_LIB","QUANTITE_LOBSTERS"))
} else {
  marees_captures_LOBSTERS=setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("NAVIRE","JOUR","METIER_LOBSTERS_COD","METIER_LOBSTERS_LIB","QUANTITE_LOBSTERS"))
}
calendrier_journalier=merge(calendrier_journalier,marees_captures_LOBSTERS,by=c("NAVIRE","JOUR"),all.x=T)
calendrier_journalier$METIER_LOBSTERS_COD=as.character(calendrier_journalier$METIER_LOBSTERS_COD)
calendrier_journalier$METIER_LOBSTERS_LIB=as.character(calendrier_journalier$METIER_LOBSTERS_LIB)

### --- Ajout métiers + quantités capturées des log-books Sea Cucumbers

if (exists("marees_sea_cucumbers")&&nrow(marees_sea_cucumbers)>0) {
  marees_captures_SEA_CUCUMBER=setNames(marees_sea_cucumbers[,c("NAVIRE","FISHING_DATE","METIER_COD","METIER_LIB","QUANTITE")],c("NAVIRE","JOUR","METIER_SEA_CUCUMBER_COD","METIER_SEA_CUCUMBER_LIB","QUANTITE_SEA_CUCUMBER"))
} else {
  marees_captures_SEA_CUCUMBER=setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("NAVIRE","JOUR","METIER_SEA_CUCUMBER_COD","METIER_SEA_CUCUMBER_LIB","QUANTITE_SEA_CUCUMBER"))
}
calendrier_journalier=merge(calendrier_journalier,marees_captures_SEA_CUCUMBER,by=c("NAVIRE","JOUR"),all.x=T)
calendrier_journalier$METIER_SEA_CUCUMBER_COD=as.character(calendrier_journalier$METIER_SEA_CUCUMBER_COD)
calendrier_journalier$METIER_SEA_CUCUMBER_LIB=as.character(calendrier_journalier$METIER_SEA_CUCUMBER_LIB)

### --- Ajout de la flottille du navire

calendrier_journalier=merge(calendrier_journalier,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],all.x=T)

### --- Ajout de l'info si port primaire ou secondaire

calendrier_journalier=merge(calendrier_journalier,setNames(ref_ports[,c("LABEL","Type")],c("PORT_COD","TYPE_PORT")),by.x="PORT",by.y="PORT_COD",all.x=T)

### --- Ajout de la région

calendrier_journalier=merge(calendrier_journalier,setNames(ref_ports[,c("LABEL","Region")],c("PORT_COD","REGION")),by.x="PORT",by.y="PORT_COD",all.x=T)

### --- Ajout du numéro de la semaine

calendrier_journalier$WEEK_NUMBER=as.numeric(format(calendrier_journalier$JOUR+3,"%U"))
calendrier_journalier=calendrier_journalier[,c("NAVIRE","GR_VESSEL_TYPE","MOIS","WEEK_NUMBER","JOUR","JDM_FINSS","JDM_CAS","JDM_SEMI_LL","JDM_DROPLINES","JDM_LOBSTERS","JDM_SEA_CUCUMBER","JDM_VMS","MAREE_EFFORT_VMS","JDM_FINAL","JDM_SOURCE_EXHAUSTIVE","PORT_FINSS","PORT_CAS","PORT_SEMI_LL","PORT_DROPLINES","PORT_LOBSTERS","PORT_SEA_CUCUMBER","PORT_VMS","PORT","TYPE_PORT","REGION","METIER_CAS_COD","METIER_CAS_LIB","QUANTITE_CAS","JDM_METIER_CAS","METIER_DROPLINES_COD","METIER_DROPLINES_LIB","JDM_METIER_DROPLINES","QUANTITE_DROPLINES","METIER_SEMI_LL_COD","METIER_SEMI_LL_LIB","QUANTITE_SEMI_LL","JDM_METIER_SEMI_LL","METIER_LOBSTERS_COD","METIER_LOBSTERS_LIB","QUANTITE_LOBSTERS","METIER_SEA_CUCUMBER_COD","METIER_SEA_CUCUMBER_LIB","QUANTITE_SEA_CUCUMBER")]
calendrier_journalier=unique(calendrier_journalier[order(calendrier_journalier$NAVIRE,calendrier_journalier$JOUR),])

calendrier_journalier$METIER_COD=sapply(1:nrow(calendrier_journalier),function(x)ifelse(all(is.na(calendrier_journalier[x,c("METIER_CAS_COD","METIER_DROPLINES_COD","METIER_SEMI_LL_COD","METIER_LOBSTERS_COD","METIER_SEA_CUCUMBER_COD")])),NA,ifelse(!is.na(calendrier_journalier[x,"METIER_CAS_COD"]),calendrier_journalier[x,"METIER_CAS_COD"],ifelse(!is.na(calendrier_journalier[x,"METIER_DROPLINES_COD"]),calendrier_journalier[x,"METIER_DROPLINES_COD"],ifelse(!is.na(calendrier_journalier[x,"METIER_SEMI_LL_COD"]),calendrier_journalier[x,"METIER_SEMI_LL_COD"],ifelse(!is.na(calendrier_journalier[x,"METIER_LOBSTERS_COD"]),calendrier_journalier[x,"METIER_LOBSTERS_COD"],ifelse(!is.na(calendrier_journalier[x,"METIER_SEA_CUCUMBER_COD"]),calendrier_journalier[x,"METIER_SEA_CUCUMBER_COD"],NA)))))))
calendrier_journalier$METIER_LIB=sapply(1:nrow(calendrier_journalier),function(x)ifelse(all(is.na(calendrier_journalier[x,c("METIER_CAS_LIB","METIER_DROPLINES_LIB","METIER_SEMI_LL_LIB","METIER_LOBSTERS_LIB","METIER_SEA_CUCUMBER_LIB")])),NA,ifelse(!is.na(calendrier_journalier[x,"METIER_CAS_LIB"]),calendrier_journalier[x,"METIER_CAS_LIB"],ifelse(!is.na(calendrier_journalier[x,"METIER_DROPLINES_LIB"]),calendrier_journalier[x,"METIER_DROPLINES_LIB"],ifelse(!is.na(calendrier_journalier[x,"METIER_SEMI_LL_LIB"]),calendrier_journalier[x,"METIER_SEMI_LL_LIB"],ifelse(!is.na(calendrier_journalier[x,"METIER_LOBSTERS_LIB"]),calendrier_journalier[x,"METIER_LOBSTERS_LIB"],ifelse(!is.na(calendrier_journalier[x,"METIER_SEA_CUCUMBER_LIB"]),calendrier_journalier[x,"METIER_SEA_CUCUMBER_LIB"],NA)))))))
calendrier_journalier$QUANTITE=sapply(1:nrow(calendrier_journalier),function(x)ifelse(all(is.na(calendrier_journalier[x,c("QUANTITE_CAS","QUANTITE_DROPLINES","QUANTITE_SEMI_LL","QUANTITE_LOBSTERS","QUANTITE_SEA_CUCUMBER")])),NA,ifelse(!is.na(calendrier_journalier[x,"QUANTITE_CAS"]),calendrier_journalier[x,"QUANTITE_CAS"],ifelse(!is.na(calendrier_journalier[x,"QUANTITE_DROPLINES"]),calendrier_journalier[x,"QUANTITE_DROPLINES"],ifelse(!is.na(calendrier_journalier[x,"QUANTITE_SEMI_LL"]),calendrier_journalier[x,"QUANTITE_SEMI_LL"],ifelse(!is.na(calendrier_journalier[x,"QUANTITE_LOBSTERS"]),calendrier_journalier[x,"QUANTITE_LOBSTERS"],ifelse(!is.na(calendrier_journalier[x,"QUANTITE_SEA_CUCUMBER"]),calendrier_journalier[x,"QUANTITE_SEA_CUCUMBER"],NA)))))))

### --- Construction du calendrier d'activité mensuelle

calendrier_journalier1=setNames(calendrier_journalier[,c("NAVIRE","GR_VESSEL_TYPE","MOIS","JOUR","JDM_FINAL","PORT","REGION","METIER_COD","METIER_LIB","QUANTITE")],c("NAVIRE","GR_VESSEL_TYPE","MOIS","JOUR","JOURS_DE_MER","PORT","REGION","METIER_COD","METIER_LIB","QUANTITE"))
a=summaryBy(data=calendrier_journalier1,JOURS_DE_MER~NAVIRE+GR_VESSEL_TYPE+MOIS,FUN=sum,na.rm=T,keep.names=T)
b=na.omit(summaryBy(data=calendrier_journalier1,METIER_COD+METIER_LIB~NAVIRE+GR_VESSEL_TYPE+MOIS+METIER_COD+METIER_LIB,FUN=NROW))[,c("NAVIRE","GR_VESSEL_TYPE","MOIS","METIER_COD","METIER_LIB")]
c=na.omit(summaryBy(data=calendrier_journalier1,QUANTITE~NAVIRE+GR_VESSEL_TYPE+MOIS+METIER_COD+METIER_LIB,FUN=sum,na.rm=T,keep.names=T))
calendrier_mensuel=merge(a,merge(b,c),all.x=T)
sum(unique(calendrier_mensuel[,c("NAVIRE","GR_VESSEL_TYPE","MOIS","JOURS_DE_MER")])$JOURS_DE_MER)
# Port mensuel d'exploitation : le plus fréquent chaque mois
port_mensuel=setNames(summaryBy(PORT~NAVIRE+MOIS,data=calendrier_journalier1,FUN=function(x)c(names(rev(sort(table(as.character(x)))))[1],rev(sort(table(as.character(x))))[1])),c("NAVIRE","MOIS","PORT","NB_PORT"))
calendrier_mensuel=merge(calendrier_mensuel,port_mensuel[,c("NAVIRE","MOIS","PORT")],all.x=T)[,c("NAVIRE","GR_VESSEL_TYPE","MOIS","PORT","JOURS_DE_MER","METIER_COD","METIER_LIB","QUANTITE")]
# Si pas d'infos sur le port mensuel pour un mois donné, reprendre le port d'exploitation annuel
calendrier_mensuel=merge(calendrier_mensuel,port_plus_frequent[,c("NAVIRE","PORT_EXP")],all.x=T)
calendrier_mensuel$PORT=as.character(calendrier_mensuel$PORT)
calendrier_mensuel$PORT_EXP=as.character(calendrier_mensuel$PORT_EXP)
calendrier_mensuel[is.na(calendrier_mensuel$PORT),]$PORT=calendrier_mensuel[is.na(calendrier_mensuel$PORT),]$PORT_EXP
calendrier_mensuel=calendrier_mensuel[order(calendrier_mensuel$NAVIRE,calendrier_mensuel$MOIS),c("NAVIRE","GR_VESSEL_TYPE","MOIS","PORT","JOURS_DE_MER","METIER_COD","METIER_LIB","QUANTITE")]

write.table(calendrier_journalier,paste(name_folder,"/calendrier_journalier_",annee,".csv",sep=""),row.names=F,sep=";",na="")
write.table(calendrier_mensuel,paste(name_folder,"/calendrier_mensuel_",annee,".csv",sep=""),row.names=F,sep=";",na="")


