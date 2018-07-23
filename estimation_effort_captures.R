
#Fontion pour arrondir des chiffres décimaux tout en préservant leur somme de départ
smart_round=function(x,digits=0){
  up<-10^digits
  x<-x*up
  y<-floor(x)
  indices<-tail(order(x-y),round(sum(x))-sum(y))
  y[indices]<-y[indices]+1
  return(y/up)
}

### --- Estimation de l'effort : JDM par région*flottille*métier (niveau région bien plus robuste que port)

#Nombre de jours d'enquêtes FINSS par région*semaine*flottille
nb_par_region_semaine_flottille=setNames(summaryBy(JOUR~GR_VESSEL_TYPE+REGION+WEEK_NUMBER,data=unique(calendrier_journalier[!is.na(calendrier_journalier$JDM_FINSS),c("GR_VESSEL_TYPE","REGION","WEEK_NUMBER","JOUR","JDM_FINSS")]),FUN=function(x)length(unique(x))),c("GR_VESSEL_TYPE","REGION","WEEK_NUMBER","NB_VISITES_FINSS"))
nb_par_region_flottille=summaryBy(NB_VISITES_FINSS~REGION+GR_VESSEL_TYPE,data=nb_par_region_semaine_flottille,FUN=mean)
taux_enquetes_par_region_flottille_FINSS=nb_par_region_flottille[order(nb_par_region_flottille$REGION),]

#Calcul du taux moyen de sortie hebdo FINSS par région*flottille : 6 jours potentiels d'activité par semaine (pas de pêche le dimanche)
taux_enquetes_par_region_flottille_FINSS$TAUX=taux_enquetes_par_region_flottille_FINSS$NB_VISITES_FINSS.mean/6 #6 jours de pêche par semaine
taux_enquetes_par_region_flottille_FINSS$TAUX=replace(taux_enquetes_par_region_flottille_FINSS$TAUX,taux_enquetes_par_region_flottille_FINSS$TAUX>1,1) #Si taux > 1, forcer à 1 = exhaustivité de la source

#Séparation des JDM en 2 sources : 
# 1) Sources exhaustives (log-books + VMS) => sommer sans extrapolation derrière
# 2) Sources échantillonnées (FINSS + CAS + DROPLINES) => sommer JDM renseignés dans le calendrier journalier, puis extrapoler par le taux moyen de sortie hebdo région*flottille
calendrier_journalier_source_exhaustive=calendrier_journalier[calendrier_journalier$JDM_SOURCE_EXHAUSTIVE & !is.na(calendrier_journalier$REGION),]
if (nrow(calendrier_journalier_source_exhaustive)>0) {
  N_source_exhaustive=setNames(summaryBy(JDM_FINAL~REGION+GR_VESSEL_TYPE,data=calendrier_journalier_source_exhaustive,FUN=sum),c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_EXHAUSTIVE"))  
} else {
  N_source_exhaustive=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_EXHAUSTIVE"))
}
calendrier_journalier_source_non_exhaustive=calendrier_journalier[!calendrier_journalier$JDM_SOURCE_EXHAUSTIVE & !is.na(calendrier_journalier$REGION),]
if (nrow(calendrier_journalier_source_non_exhaustive)>0) {
  N_source_echantillon=setNames(summaryBy(JDM_FINAL~REGION+GR_VESSEL_TYPE,data=calendrier_journalier_source_non_exhaustive,FUN=sum),c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_ECHANTILLON"))  
} else {
  N_source_echantillon=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_ECHANTILLON"))
}
estimation_N=merge(N_source_exhaustive,N_source_echantillon,all=T)
estimation_N=merge(estimation_N,taux_enquetes_par_region_flottille_FINSS,by=c("REGION","GR_VESSEL_TYPE"),all.x=T)
estimation_N$JDM_SOURCE_ECHANTILLON_ESTIME=round(estimation_N$JDM_SOURCE_ECHANTILLON/estimation_N$TAUX)
estimation_N$JDM_SOURCE_ECHANTILLON_ESTIME=replace(estimation_N$JDM_SOURCE_ECHANTILLON_ESTIME,is.na(estimation_N$JDM_SOURCE_ECHANTILLON_ESTIME),0)
estimation_N$JDM_SOURCE_EXHAUSTIVE=replace(estimation_N$JDM_SOURCE_EXHAUSTIVE,is.na(estimation_N$JDM_SOURCE_EXHAUSTIVE),0)
estimation_N$JDM_FINAL_ESTIME=estimation_N$JDM_SOURCE_EXHAUSTIVE+estimation_N$JDM_SOURCE_ECHANTILLON_ESTIME
estimation_N=estimation_N[estimation_N$JDM_FINAL_ESTIME!=0,]

# Ventilation du N par région*flottille*métier

data_N_metier_CAS=calendrier_journalier[!is.na(calendrier_journalier$JDM_METIER_CAS),c("NAVIRE","GR_VESSEL_TYPE","REGION","JOUR","METIER_CAS_COD","METIER_CAS_LIB","QUANTITE_CAS","JDM_METIER_CAS")]
if (nrow(data_N_metier_CAS)>0) {
  N_metier_CAS=data.frame(setNames(summaryBy(JDM_METIER_CAS~REGION+GR_VESSEL_TYPE+METIER_CAS_COD+METIER_CAS_LIB,data=data_N_metier_CAS,FUN=sum,keep.names=T),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER")),SOURCE="ECHANTILLON")
} else {
  N_metier_CAS=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER","SOURCE"))
}

data_N_metier_DROPLINES=calendrier_journalier[!is.na(calendrier_journalier$JDM_METIER_DROPLINES),c("NAVIRE","GR_VESSEL_TYPE","REGION","JOUR","METIER_DROPLINES_COD","METIER_DROPLINES_LIB","QUANTITE_DROPLINES","JDM_METIER_DROPLINES")]
if (nrow(data_N_metier_DROPLINES)>0) {
  N_metier_DROPLINES=data.frame(setNames(summaryBy(JDM_METIER_DROPLINES~REGION+GR_VESSEL_TYPE+METIER_DROPLINES_COD+METIER_DROPLINES_LIB,data=data_N_metier_DROPLINES,FUN=sum,keep.names=T),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER")),SOURCE="ECHANTILLON")
} else {
  N_metier_DROPLINES=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER","SOURCE"))
}

data_N_metier_semi_LL=calendrier_journalier[!is.na(calendrier_journalier$JDM_METIER_SEMI_LL),c("NAVIRE","GR_VESSEL_TYPE","REGION","JOUR","METIER_SEMI_LL_COD","METIER_SEMI_LL_LIB","QUANTITE_SEMI_LL","JDM_METIER_SEMI_LL")]
if (nrow(data_N_metier_semi_LL)>0) {
  N_metier_semi_LL=data.frame(setNames(summaryBy(JDM_METIER_SEMI_LL~REGION+GR_VESSEL_TYPE+METIER_SEMI_LL_COD+METIER_SEMI_LL_LIB,data=data_N_metier_semi_LL,FUN=sum,keep.names=T),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER")),SOURCE="EXHAUSTIVE")  
} else {
  N_metier_semi_LL=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER","SOURCE"))
}

data_N_metier_LOBSTERS=calendrier_journalier[!is.na(calendrier_journalier$JDM_LOBSTERS),c("NAVIRE","GR_VESSEL_TYPE","REGION","JOUR","METIER_LOBSTERS_COD","METIER_LOBSTERS_LIB","QUANTITE_LOBSTERS")]
if (nrow(data_N_metier_LOBSTERS)>0) {
  N_metier_LOBSTERS=data.frame(setNames(summaryBy(JOUR~REGION+GR_VESSEL_TYPE+METIER_LOBSTERS_COD+METIER_LOBSTERS_LIB,data=data_N_metier_LOBSTERS,FUN=NROW,keep.names=T),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER")),SOURCE="EXHAUSTIVE")
} else {
  N_metier_LOBSTERS=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER","SOURCE"))
}

data_N_metier_SEA_CUCUMBER=calendrier_journalier[!is.na(calendrier_journalier$JDM_SEA_CUCUMBER),c("NAVIRE","GR_VESSEL_TYPE","REGION","JOUR","METIER_SEA_CUCUMBER_COD","METIER_SEA_CUCUMBER_LIB","QUANTITE_SEA_CUCUMBER")]
if (nrow(data_N_metier_SEA_CUCUMBER)>0) {
  N_metier_SEA_CUCUMBER=data.frame(setNames(summaryBy(JOUR~REGION+GR_VESSEL_TYPE+METIER_SEA_CUCUMBER_COD+METIER_SEA_CUCUMBER_LIB,data=data_N_metier_SEA_CUCUMBER,FUN=NROW,keep.names=T),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER")),SOURCE="EXHAUSTIVE")
} else {
  N_metier_SEA_CUCUMBER=setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER","SOURCE"))
}

N_metier=rbind(N_metier_CAS,N_metier_DROPLINES,N_metier_semi_LL,N_metier_LOBSTERS,N_metier_SEA_CUCUMBER)
N_metier=summaryBy(data=N_metier,JDM_METIER~REGION+GR_VESSEL_TYPE+METIER_COD+METIER_LIB+SOURCE,FUN=sum,keep.names=T)
N_metier=merge(N_metier,estimation_N[,c("REGION","GR_VESSEL_TYPE","JDM_FINAL_ESTIME")])
if (nrow(N_metier[N_metier$SOURCE=="EXHAUSTIVE",])>0) {
  N_metier=merge(N_metier,setNames(summaryBy(data=N_metier[N_metier$SOURCE=="EXHAUSTIVE",],JDM_METIER~REGION+GR_VESSEL_TYPE,FUN=sum),c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_EXHAUSTIVE")),all=T)
} else {
  N_metier=merge(N_metier,setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("REGION","GR_VESSEL_TYPE","JDM_SOURCE_EXHAUSTIVE")),all=T)
}
if (length(is.na(N_metier$JDM_SOURCE_EXHAUSTIVE))>0) {
  N_metier$JDM_SOURCE_EXHAUSTIVE=replace(N_metier$JDM_SOURCE_EXHAUSTIVE,is.na(N_metier$JDM_SOURCE_EXHAUSTIVE),0) 
}

N_metier$JDM_SOURCE_ECHANTILLON=N_metier$JDM_FINAL_ESTIME-N_metier$JDM_SOURCE_EXHAUSTIVE

N_metier=merge(N_metier,setNames(summaryBy(data=N_metier[N_metier$SOURCE=="ECHANTILLON",],JDM_METIER~REGION+GR_VESSEL_TYPE,FUN=sum),c("REGION","GR_VESSEL_TYPE","JDM_ECHANTILLON")),all=T)
N_metier$POURCENT_JDM_ECH=NA
N_metier[N_metier$SOURCE=="ECHANTILLON",]$POURCENT_JDM_ECH=N_metier[N_metier$SOURCE=="ECHANTILLON",]$JDM_METIER/N_metier[N_metier$SOURCE=="ECHANTILLON",]$JDM_ECHANTILLON
N_metier$REGION_TYPE=paste(N_metier$REGION,N_metier$GR_VESSEL_TYPE,sep="_")
JDM_metier_FINAL=c()
# Par région*flottille, calcul du pourcentage des JDM de chaque métier par rapport au JDM total estimé
for(comb in unique(N_metier$REGION_TYPE)){
  tmp=N_metier[N_metier$REGION_TYPE==comb,]
  tmp$JDM_METIER_FINAL=NA
  if("EXHAUSTIVE" %in% tmp$SOURCE) tmp[tmp$SOURCE=="EXHAUSTIVE",]$JDM_METIER_FINAL=tmp[tmp$SOURCE=="EXHAUSTIVE",]$JDM_METIER
  if("ECHANTILLON" %in% tmp$SOURCE) tmp[tmp$SOURCE=="ECHANTILLON",]$JDM_METIER_FINAL=smart_round(tmp[tmp$SOURCE=="ECHANTILLON",]$POURCENT_JDM_ECH*tmp[tmp$SOURCE=="ECHANTILLON",]$JDM_SOURCE_ECHANTILLON)
  JDM_metier_FINAL=rbind(JDM_metier_FINAL,tmp[,c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","JDM_METIER_FINAL")])
}

sum(JDM_metier_FINAL$JDM_METIER_FINAL)
JDM_metier_FINAL=JDM_metier_FINAL[order(JDM_metier_FINAL$REGION,JDM_metier_FINAL$GR_VESSEL_TYPE,-JDM_metier_FINAL$JDM_METIER_FINAL),]

### --- Passage des JDM des échantillons aux nombre de marées N : via la durée des marées pour les JDM échantillonnés + somme des marées pour les logbooks

if (exists("marees_CAS")&&nrow(marees_CAS)>0) {
  marees_CAS$DUREE_MAREE=as.numeric(difftime(marees_CAS$DATE_RETOUR,marees_CAS$DATE_DEPART,units="days"))
  durees_marees_CAS=setNames(summaryBy(data=unique(marees_CAS[,c("ID_MAREE","GR_VESSEL_TYPE","GR_METIER_COD","DUREE_MAREE")]),DUREE_MAREE~GR_METIER_COD+GR_VESSEL_TYPE,FUN=mean),c("METIER_COD","GR_VESSEL_TYPE","DUREE_MAREE"))
  durees_marees_CAS$COEFF=ifelse(durees_marees_CAS$DUREE_MAREE<1,1,1/durees_marees_CAS$DUREE_MAREE)
} else {
  durees_marees_CAS=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("METIER_COD","GR_VESSEL_TYPE","DUREE_MAREE","COEFF"))
}
if (exists("marees_droplines")&&nrow(marees_droplines)>0) {
  marees_droplines$DUREE_MAREE=as.numeric(difftime(marees_droplines$ArrivalDate,marees_droplines$DepartureDate,units="days"))
  durees_marees_DROPLINES=setNames(summaryBy(data=unique(marees_droplines[,c("TripID","GR_METIER_COD","GR_VESSEL_TYPE","DUREE_MAREE")]),DUREE_MAREE~GR_METIER_COD+GR_VESSEL_TYPE,FUN=mean),c("METIER_COD","GR_VESSEL_TYPE","DUREE_MAREE"))
  durees_marees_DROPLINES$COEFF=ifelse(durees_marees_DROPLINES$DUREE_MAREE<1,1,1/durees_marees_DROPLINES$DUREE_MAREE)
} else {
  durees_marees_DROPLINES=setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("METIER_COD","GR_VESSEL_TYPE","DUREE_MAREE","COEFF"))
}

durees_marees=rbind(durees_marees_CAS,durees_marees_DROPLINES)
durees_marees=summaryBy(data=durees_marees,DUREE_MAREE+COEFF~METIER_COD+GR_VESSEL_TYPE,FUN=mean,keep.names=T)
N_metier_FINAL=merge(JDM_metier_FINAL,durees_marees,by=c("METIER_COD","GR_VESSEL_TYPE"),all.x=T)
N_metier_FINAL$N_METIER=NA
#Aux JDM issus des échantillons 
N_metier_FINAL[!is.na(N_metier_FINAL$COEFF),]$N_METIER=round(N_metier_FINAL[!is.na(N_metier_FINAL$COEFF),]$JDM_METIER_FINAL*N_metier_FINAL[!is.na(N_metier_FINAL$COEFF),]$COEFF)                   
N_metier_FINAL[is.na(N_metier_FINAL$COEFF),]$N_METIER=N_metier_FINAL[is.na(N_metier_FINAL$COEFF),]$JDM_METIER_FINAL
n_marees_SEMI_LL=setNames(summaryBy(data=unique(marees_SEMI_LL[,c("TripHistoryID","GR_METIER_COD","GR_VESSEL_TYPE")]),TripHistoryID~GR_METIER_COD+GR_VESSEL_TYPE,FUN=NROW,keep.names=T),c("METIER_COD","GR_VESSEL_TYPE","NB_MAREE_SEMI_LL"))
N_metier_FINAL=merge(N_metier_FINAL,n_marees_SEMI_LL,all.x=T)
N_metier_FINAL[!is.na(N_metier_FINAL$NB_MAREE_SEMI_LL),]$N_METIER=N_metier_FINAL[!is.na(N_metier_FINAL$NB_MAREE_SEMI_LL),]$NB_MAREE_SEMI_LL
N_metier_FINAL=N_metier_FINAL[order(N_metier_FINAL$REGION,N_metier_FINAL$GR_VESSEL_TYPE,-N_metier_FINAL$N_METIER),c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","N_METIER")]
N_metier_FINAL$FLOTTILLE_METIER=paste(N_metier_FINAL$GR_VESSEL_TYPE,N_metier_FINAL$METIER_COD,sep="_")
sum(N_metier_FINAL$N_METIER)

### --- Evaluation des captures par marée*métier

if (nrow(marees_lobsters)>0) {
  marees_lobsters$ID_MAREE=paste(marees_lobsters$NAVIRE,marees_lobsters$FISHING_DATE,sep="_")
  marees_lobsters$GR_ESP_COD="VLO"
  marees_lobsters$GR_ESP_LIB="Lobsters"
} else {
  marees_lobsters=merge(marees_lobsters,setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ID_MAREE","GR_ESP_COD","GR_ESP_LIB")),all.x=T)
}
if (nrow(marees_sea_cucumbers)>0) {
  marees_sea_cucumbers$ID_MAREE=paste(marees_sea_cucumbers$NAVIRE,marees_sea_cucumbers$FISHING_DATE,sep="_")
  marees_sea_cucumbers$GR_ESP_COD="CUX"
  marees_sea_cucumbers$GR_ESP_LIB="Sea Cucumbers"
} else {
  marees_sea_cucumbers=merge(marees_sea_cucumbers,setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")),all.x=T)
}

# quels sont les metiers inconnus
nrow(marees_CAS[is.na(marees_CAS$GR_METIER_COD),]) #94
nrow(marees_droplines[is.na(marees_droplines$GR_METIER_COD),]) #0
nrow(marees_SEMI_LL[is.na(marees_SEMI_LL$GR_METIER_COD),]) #0
nrow(marees_lobsters[is.na(marees_lobsters$GR_METIER_COD),]) #0
nrow(marees_sea_cucumbers[is.na(marees_sea_cucumbers$GR_METIER_COD),]) #0

# on retire les 94 marées avec metiers inconnu sur les 3213 marées CAS
marees_CAS=marees_CAS[!is.na(marees_CAS$GR_METIER_COD),]

marees_x_bar=rbind(setNames(unique(marees_CAS[,c("ID_MAREE","GR_VESSEL_TYPE","GR_METIER_COD","GR_METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF")]),c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")),
                   setNames(unique(marees_droplines[,c("TripID","GR_VESSEL_TYPE","GR_METIER_COD","GR_METIER_LIB","GR_ESP_COD","GR_ESP_LIB","CatchWeight")]),c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")),
                   setNames(unique(marees_SEMI_LL[,c("TripHistoryID","GR_VESSEL_TYPE","GR_METIER_COD","GR_METIER_LIB","GR_ESP_COD","GR_ESP_LIB","Catch_Kg")]),c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")),
                   setNames(unique(marees_lobsters[,c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")]),c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")),
                   setNames(unique(marees_sea_cucumbers[,c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")]),c("ID_MAREE","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE")))
marees_x_bar$FLOTTILLE_METIER=paste(marees_x_bar$GR_VESSEL_TYPE,marees_x_bar$METIER_COD,sep="_")


x_bar=c()
flottilles_metiers=unique(marees_x_bar$FLOTTILLE_METIER)
for(flottille_metier in flottilles_metiers){
  data=marees_x_bar[marees_x_bar$FLOTTILLE_METIER==flottille_metier,]
  #Identification des espèces non-accessoires : vues dans au moins 20% des marées échantillonnées du métier
  #Test statistique (test binomial) qu'on a vu espèces dans au moins 20% des marées échantillonnées du métier : ok si p_val_test_H0>=0.05
  n_especes=setNames(summaryBy(data=data,ID_MAREE~GR_ESP_COD,FUN=function(x)length(unique(x))),c("GR_ESP_COD","n_especes"))
  n_especes=merge(n_especes,data.frame(n=length(unique(data$ID_MAREE))))
  n_especes$p_val_test_H0=as.numeric(apply(n_especes[,c("n_especes","n")],1,function(x)binom.test(x[1],x[2],p=0.2,alt="less")$p.value))
  especes=n_especes[n_especes$p_val_test_H0>=0.05,]
  data=data[data$GR_ESP_COD%in%especes$GR_ESP_COD,] #Filtre des marées sur ces espèces non-accessoires
  
  #Proportion de chaque espèce dans les captures (en quantités)
  proportion_especes=data.frame(summaryBy(data=data,QUANTITE~GR_ESP_COD+GR_ESP_LIB,FUN=sum,na.rm=T,keep.names=T),QUANTITE_TOTALE=sum(data$QUANTITE,na.rm=T))
  proportion_especes$PROP=proportion_especes$QUANTITE/proportion_especes$QUANTITE_TOTALE
  #Estimation du x_bar de la marée (non par marée*espèces) avec précision
  x_bar_maree=summaryBy(data=data,QUANTITE~ID_MAREE,FUN=sum,na.rm=T,keep.names=T)
  x_bar_maree=data.frame(GR_VESSEL_TYPE=data$GR_VESSEL_TYPE[1],METIER_COD=data$METIER_COD[1],METIER_LIB=data$METIER_LIB[1],MEAN=mean(x_bar_maree$QUANTITE),Q025=quantile(x_bar_maree$QUANTITE,0.025),Q975=quantile(x_bar_maree$QUANTITE,0.975))
  x_bar_maree=merge(x_bar_maree,proportion_especes)
  x_bar=rbind(x_bar,x_bar_maree[,c("GR_VESSEL_TYPE","METIER_COD","METIER_LIB","MEAN","Q025","Q975","GR_ESP_COD","GR_ESP_LIB","PROP")])
}
#Précision de l'intervalle de confiance à 95% des captures moyennes par marées
x_bar$PRECISION_CI_95=round(100*(x_bar$Q975-x_bar$Q025)/(2*x_bar$MEAN))

# --- Captures annuelles

estimations_finales=merge(N_metier_FINAL,x_bar,all.x=T)
estimations_finales$Q=round(estimations_finales$N_METIER*estimations_finales$MEAN*estimations_finales$PROP,-1)
estimations_finales$Q_INF_95=round(estimations_finales$N_METIER*estimations_finales$Q025*estimations_finales$PROP,-1)
estimations_finales$Q_SUP_95=round(estimations_finales$N_METIER*estimations_finales$Q975*estimations_finales$PROP,-1)
estimations_finales=estimations_finales[estimations_finales$Q>0,]
estimations_finales=merge(estimations_finales,setNames(unique(ref_ports[,c("Region","libelle.région")]),c("REGION","REGION_LIB")),by="REGION")
estimations_finales=merge(estimations_finales,setNames(JDM_metier_FINAL,c("REGION","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA")))
estimations_finales=setNames(estimations_finales[order(estimations_finales$REGION,estimations_finales$GR_VESSEL_TYPE,estimations_finales$METIER_COD,-estimations_finales$Q),c("REGION","REGION_LIB","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA","N_METIER","GR_ESP_COD","GR_ESP_LIB","Q","Q_INF_95","Q_SUP_95","PRECISION_CI_95")],c("REGION_COD","REGION_LIB","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","GR_ESP_COD","GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95","PRECISION_CI_95"))

estimations_all=data.frame(DAYS_AT_SEA=sum(unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA")])$DAYS_AT_SEA),NB_FISHING_TRIPS=sum(unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","NB_FISHING_TRIPS")])$NB_FISHING_TRIPS),CATCHES=round(sum(estimations_finales$CATCHES)),CATCHES_INF_95=round(sum(estimations_finales$CATCHES_INF_95)),CATCHES_SUP_95=round(sum(estimations_finales$CATCHES_SUP_95)))
estimations_all$PRECISION_CI_95=paste(round(100*(estimations_all$CATCHES_SUP_95-estimations_all$CATCHES_INF_95)/(2*estimations_all$CATCHES)),"%",sep="")
estimations_all$MEAN_CATCH_PER_TRIP=round(estimations_all$CATCHES/estimations_all$NB_FISHING_TRIPS)
estimations_all$MEAN_CATCH_PER_DAY_AT_SEA=round(estimations_all$CATCHES/estimations_all$DAYS_AT_SEA)

estimations_regions_flottilles_metiers=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~REGION_COD+REGION_LIB+GR_VESSEL_TYPE+METIER_COD+METIER_LIB,FUN=sum,keep.names=T)
estimations_regions_flottilles_metiers=merge(estimations_regions_flottilles_metiers,summaryBy(data=unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA","NB_FISHING_TRIPS")]),DAYS_AT_SEA+NB_FISHING_TRIPS~REGION_COD+GR_VESSEL_TYPE+METIER_COD,FUN=sum,keep.names=T))
estimations_regions_flottilles_metiers=estimations_regions_flottilles_metiers[order(-estimations_regions_flottilles_metiers$CATCHES),c("REGION_COD","REGION_LIB","GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]

estimations_metiers_especes=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~METIER_COD+METIER_LIB+GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)
estimations_metiers_especes=estimations_metiers_especes[order(estimations_metiers_especes$METIER_COD,-estimations_metiers_especes$CATCHES),]
estimations_metiers_especes[nrow(estimations_metiers_especes)+1,c("GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_metiers_especes$CATCHES,na.rm=T),sum(estimations_metiers_especes$CATCHES_INF_95,na.rm=T),sum(estimations_metiers_especes$CATCHES_SUP_95,na.rm=T))

estimations_flottilles_especes=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~GR_VESSEL_TYPE+GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)
estimations_flottilles_especes=estimations_flottilles_especes[order(estimations_flottilles_especes$GR_VESSEL_TYPE,-estimations_flottilles_especes$CATCHES),]
estimations_flottilles_especes[nrow(estimations_flottilles_especes)+1,c("GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_flottilles_especes$CATCHES,na.rm=T),sum(estimations_flottilles_especes$CATCHES_INF_95,na.rm=T),sum(estimations_flottilles_especes$CATCHES_SUP_95,na.rm=T))

estimations_flottille_metiers=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~GR_VESSEL_TYPE+METIER_COD+METIER_LIB,FUN=sum,keep.names=T)
estimations_flottille_metiers=merge(estimations_flottille_metiers,summaryBy(data=unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA","NB_FISHING_TRIPS")]),DAYS_AT_SEA+NB_FISHING_TRIPS~GR_VESSEL_TYPE+METIER_COD,FUN=sum,keep.names=T))
estimations_flottille_metiers=estimations_flottille_metiers[order(estimations_flottille_metiers$GR_VESSEL_TYPE,-estimations_flottille_metiers$CATCHES),c("GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]

estimations_especes=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)
estimations_especes=estimations_especes[order(-estimations_especes$CATCHES),]
estimations_especes$PRECISION_CI_95=paste(round(100*(estimations_especes$CATCHES_SUP_95-estimations_especes$CATCHES_INF_95)/(2*estimations_especes$CATCHES)),"%",sep="")
estimations_especes[nrow(estimations_especes)+1,c("GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_especes$CATCHES,na.rm=T),sum(estimations_especes$CATCHES_INF_95,na.rm=T),sum(estimations_especes$CATCHES_SUP_95,na.rm=T))
estimations_especes[c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(estimations_especes[,c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
estimations_especes$PRECISION_CI_95[nrow(estimations_especes)]=paste(round(100*((estimations_especes$CATCHES_SUP_95-estimations_especes$CATCHES_INF_95)/(2*estimations_especes$CATCHES))[nrow(estimations_especes)]),"%",sep="")

estimations_metiers=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~METIER_COD+METIER_LIB,FUN=sum,keep.names=T)
estimations_metiers=merge(estimations_metiers,summaryBy(data=unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA","NB_FISHING_TRIPS")]),DAYS_AT_SEA+NB_FISHING_TRIPS~METIER_COD+METIER_LIB,FUN=sum,keep.names=T))
estimations_metiers=estimations_metiers[order(-estimations_metiers$CATCHES),c("METIER_COD","METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]
estimations_metiers$PRECISION_CI_95=paste(round(100*(estimations_metiers$CATCHES_SUP_95-estimations_metiers$CATCHES_INF_95)/(2*estimations_metiers$CATCHES)),"%",sep="")
estimations_metiers$MEAN_CATCH_PER_TRIP=round(estimations_metiers$CATCHES/estimations_metiers$NB_FISHING_TRIPS)
estimations_metiers$MEAN_CATCH_PER_DAY_AT_SEA=round(estimations_metiers$CATCHES/estimations_metiers$DAYS_AT_SEA)
estimations_metiers[nrow(estimations_metiers)+1,c("METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_metiers$DAYS_AT_SEA),sum(estimations_metiers$NB_FISHING_TRIPS),sum(estimations_metiers$CATCHES),sum(estimations_metiers$CATCHES_INF_95),sum(estimations_metiers$CATCHES_SUP_95))
estimations_metiers[c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(estimations_metiers[,c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
estimations_metiers$PRECISION_CI_95[nrow(estimations_metiers)]=paste(round(100*((estimations_metiers$CATCHES_SUP_95-estimations_metiers$CATCHES_INF_95)/(2*estimations_metiers$CATCHES))[nrow(estimations_metiers)]),"%",sep="")

estimations_flottilles=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~GR_VESSEL_TYPE,FUN=sum,keep.names=T)
estimations_flottilles=merge(estimations_flottilles,summaryBy(data=unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA","NB_FISHING_TRIPS")]),DAYS_AT_SEA+NB_FISHING_TRIPS~GR_VESSEL_TYPE,FUN=sum,keep.names=T))
estimations_flottilles=estimations_flottilles[order(-estimations_flottilles$CATCHES),c("GR_VESSEL_TYPE","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]
estimations_flottilles$PRECISION_CI_95=paste(round(100*(estimations_flottilles$CATCHES_SUP_95-estimations_flottilles$CATCHES_INF_95)/(2*estimations_flottilles$CATCHES)),"%",sep="")
estimations_flottilles$MEAN_CATCH_PER_TRIP=round(estimations_flottilles$CATCHES/estimations_flottilles$NB_FISHING_TRIPS)
estimations_flottilles$MEAN_CATCH_PER_DAY_AT_SEA=round(estimations_flottilles$CATCHES/estimations_flottilles$DAYS_AT_SEA)
estimations_flottilles$GR_VESSEL_TYPE=as.character(estimations_flottilles$GR_VESSEL_TYPE)
estimations_flottilles[nrow(estimations_flottilles)+1,c("GR_VESSEL_TYPE","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_flottilles$DAYS_AT_SEA),sum(estimations_flottilles$NB_FISHING_TRIPS),sum(estimations_flottilles$CATCHES),sum(estimations_flottilles$CATCHES_INF_95),sum(estimations_flottilles$CATCHES_SUP_95))
estimations_flottilles[c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(estimations_flottilles[,c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
estimations_flottilles$PRECISION_CI_95[nrow(estimations_flottilles)]=paste(round(100*((estimations_flottilles$CATCHES_SUP_95-estimations_flottilles$CATCHES_INF_95)/(2*estimations_flottilles$CATCHES))[nrow(estimations_flottilles)]),"%",sep="")

estimations_regions=summaryBy(data=estimations_finales,CATCHES+CATCHES_INF_95+CATCHES_SUP_95~REGION_COD+REGION_LIB,FUN=sum,keep.names=T)
estimations_regions=merge(estimations_regions,summaryBy(data=unique(estimations_finales[,c("REGION_COD","GR_VESSEL_TYPE","METIER_COD","DAYS_AT_SEA","NB_FISHING_TRIPS")]),DAYS_AT_SEA+NB_FISHING_TRIPS~REGION_COD,FUN=sum,keep.names=T))
estimations_regions=estimations_regions[order(-estimations_regions$CATCHES),c("REGION_COD","REGION_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]
estimations_regions$PRECISION_CI_95=paste(round(100*(estimations_regions$CATCHES_SUP_95-estimations_regions$CATCHES_INF_95)/(2*estimations_regions$CATCHES)),"%",sep="")
estimations_regions$MEAN_CATCH_PER_TRIP=round(estimations_regions$CATCHES/estimations_regions$NB_FISHING_TRIPS)
estimations_regions$MEAN_CATCH_PER_DAY_AT_SEA=round(estimations_regions$CATCHES/estimations_regions$DAYS_AT_SEA)
estimations_regions$REGION_LIB=as.character(estimations_regions$REGION_LIB)
estimations_regions[nrow(estimations_regions)+1,c("REGION_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(estimations_regions$DAYS_AT_SEA),sum(estimations_regions$NB_FISHING_TRIPS),sum(estimations_regions$CATCHES),sum(estimations_regions$CATCHES_INF_95),sum(estimations_regions$CATCHES_SUP_95))
estimations_regions[c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(estimations_regions[,c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
estimations_regions$PRECISION_CI_95[nrow(estimations_regions)]=paste(round(100*((estimations_regions$CATCHES_SUP_95-estimations_regions$CATCHES_INF_95)/(2*estimations_regions$CATCHES))[nrow(estimations_regions)]),"%",sep="")

