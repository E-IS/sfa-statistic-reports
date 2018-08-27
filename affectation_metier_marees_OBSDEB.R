
### --- Définir une règle d'affectation du métier à chaque marée

# Cas 0 = engin connu lors de la marée (17%)
# Affectation de l'engin à la marée, si engin pas renseigné (83% des marées) : 
#   - Cas 1 = si flottille + espèce connue : sachant espèce agrégée et flottille du navire, prendre engin le plus probable => engin_sachant_flottille_espece (46% des marées)
#   - Cas 2 = si (flottille*espèce) absent du référentiel engin_sachant_flottille_espece OU si pas de flottille connue + espèce connue : sachant espèce, prendre engin le plus probable => engin_sachant_espece (2% des marées)
#   - Cas 3 = si flottille connue mais espèce inconnue des réfentiels (ou pas de captures) : sachant flottille, prendre engin le plus probable => engin_sachant_flottille (2% des marées)
# Cas 4 = impossible d'associer un engin à la marée (flottille et espèce absentes des référentiels de base) (33% des marées, tous les UNKNOWN ou quelques pirogues)

if (exists("CaptureEch")&&nrow(CaptureEch)>0) {
  captures=summaryBy(QUANTITE_CAP_VIF~ID_MAREE+GR_ESP_COD+GR_ESP_LIB,data=CaptureEch,FUN=sum,na.rm=T,keep.names=T)
  #Par marée, on retient l'espèce principale (quantité max)
  qte_max=summaryBy(QUANTITE_CAP_VIF~ID_MAREE,data=captures,FUN=max,na.rm=T)
  captures=merge(captures,qte_max,all.x=T)
  captures=captures[captures$QUANTITE_CAP_VIF==captures$QUANTITE_CAP_VIF.max,c("ID_MAREE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF")]
  #Cas où max identique sur 2 groupes espèces différents : n'en retenir qu'une espèce (arbitrairement)
  if(sum(duplicated(captures$ID_MAREE))>0){
    marees_doublon=captures$ID_MAREE[duplicated(captures$ID_MAREE)]
    tmp=captures[captures$ID_MAREE%in%marees_doublon,]
    ind=rownames(tmp)
    tmp_bis=c()
    for(i in 1:length(marees_doublon)) tmp_bis=rbind(tmp_bis,tmp[tmp$ID_MAREE==marees_doublon[i],][1,])
    captures=captures[-which(rownames(captures)%in%ind),]
    captures=rbind(captures,tmp_bis)
  }
  
  marees_captures=unique(merge(MareeEch[,c("ID_MAREE","GR_VESSEL_TYPE","GR_METIER_COD","GR_METIER_LIB","GR_ENGIN_COD","GR_ENGIN_LIB")],captures,all.x=TRUE))
  marees_captures[marees_captures==""]=NA
  marees_captures$GR_METIER_COD=as.character(marees_captures$GR_METIER_COD) # metier connu depuis obsdeb
  marees_captures$GR_METIER_LIB=as.character(marees_captures$GR_METIER_LIB) # metier connu depuis obsdeb
  marees_captures$GR_ENGIN_COD=as.character(marees_captures$GR_ENGIN_COD)
  marees_captures$GR_ENGIN_LIB=as.character(marees_captures$GR_ENGIN_LIB)
  #str(marees_captures)
  
  #data.frame FLOTTILLE/ESPECE => ENGIN + PROBABLE (pour marées sans engin, avec flottille + espèce connues)
  marees_pour_ref_flottille_espece=marees_captures[is.na(marees_captures$GR_METIER_COD) & !is.na(marees_captures$GR_ENGIN_COD) & !is.na(marees_captures$GR_ESP_COD) & !is.na(marees_captures$GR_VESSEL_TYPE),]
  if (nrow(marees_pour_ref_flottille_espece)>0) {
    engin_sachant_flottille_espece=setNames(as.data.frame(table(marees_pour_ref_flottille_espece$GR_ENGIN_COD,marees_pour_ref_flottille_espece$GR_ESP_COD,marees_pour_ref_flottille_espece$GR_VESSEL_TYPE)),c("GR_ENGIN_COD","GR_ESP_COD","TYPO","NB"))
    engin_sachant_flottille_espece=engin_sachant_flottille_espece[engin_sachant_flottille_espece$NB>0,]
    engin_sachant_flottille_espece=merge(engin_sachant_flottille_espece,summaryBy(NB~TYPO+GR_ESP_COD,data=engin_sachant_flottille_espece,FUN=function(x)max(x)[1],keep.names=T))
    engin_sachant_flottille_espece=setNames(engin_sachant_flottille_espece[order(engin_sachant_flottille_espece$TYPO,engin_sachant_flottille_espece$GR_ESP_COD),c("TYPO","GR_ESP_COD","GR_ENGIN_COD")],c("TYPO","GR_ESP_COD","ENGIN_ESTIME"))
    if(sum(duplicated(engin_sachant_flottille_espece[,c("TYPO","GR_ESP_COD")]))>0) engin_sachant_flottille_espece=engin_sachant_flottille_espece[-which(duplicated(engin_sachant_flottille_espece[,c("TYPO","GR_ESP_COD")])),]
  } else {
    engin_sachant_flottille_espece=setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("TYPO","GR_ESP_COD","ENGIN_ESTIME"))
  }
  
  #data.frame ESPECE => ENGIN + PROBABLE (pour marées sans engin, avec espèce connue + flottille inconnue)
  marees_pour_ref_espece=marees_captures[is.na(marees_captures$GR_METIER_COD) & !is.na(marees_captures$GR_ENGIN_COD) & !is.na(marees_captures$GR_ESP_COD),]
  if (nrow(marees_pour_ref_espece)>0) {
    engin_sachant_espece=setNames(as.data.frame(table(marees_pour_ref_espece$GR_ENGIN_COD,marees_pour_ref_espece$GR_ESP_COD)),c("GR_ENGIN_COD","GR_ESP_COD","NB"))
    engin_sachant_espece=engin_sachant_espece[engin_sachant_espece$NB>0,]
    engin_sachant_espece=merge(engin_sachant_espece,summaryBy(NB~GR_ESP_COD,data=engin_sachant_espece,FUN=function(x)max(x)[1],keep.names=T))
    engin_sachant_espece=setNames(engin_sachant_espece[order(engin_sachant_espece$GR_ESP_COD),c("GR_ESP_COD","GR_ENGIN_COD")],c("GR_ESP_COD","ENGIN_ESTIME"))
    if(sum(duplicated(engin_sachant_espece[,c("GR_ESP_COD")]))>0) engin_sachant_espece=engin_sachant_espece[-which(duplicated(engin_sachant_espece[,c("GR_ESP_COD")])),]
  } else {
    engin_sachant_espece=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("GR_ESP_COD","ENGIN_ESTIME"))
  }
  
  #data.frame FLOTTILLE => ENGIN + PROBABLE (pour marées sans engin, avec espèce inconnue + flottille connue)
  marees_pour_ref_flottille=marees_captures[is.na(marees_captures$GR_METIER_COD) & !is.na(marees_captures$GR_ENGIN_COD) & !is.na(marees_captures$GR_VESSEL_TYPE),]
  if (nrow(marees_pour_ref_flottille)>0) {
    engin_sachant_flottille=setNames(as.data.frame(table(marees_pour_ref_flottille$GR_ENGIN_COD,marees_pour_ref_flottille$GR_VESSEL_TYPE)),c("GR_ENGIN_COD","TYPO","NB"))
    engin_sachant_flottille=engin_sachant_flottille[engin_sachant_flottille$NB>0,]
    engin_sachant_flottille=merge(engin_sachant_flottille,summaryBy(NB~TYPO,data=engin_sachant_flottille,FUN=function(x)max(x)[1],keep.names=T))
    engin_sachant_flottille=setNames(engin_sachant_flottille[order(engin_sachant_flottille$TYPO),c("TYPO","GR_ENGIN_COD")],c("TYPO","ENGIN_ESTIME"))
    if(sum(duplicated(engin_sachant_flottille[,c("TYPO")]))>0) engin_sachant_flottille=engin_sachant_flottille[-which(duplicated(engin_sachant_flottille[,c("TYPO")])),]
  } else {
    engin_sachant_flottille=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("TYPO","ENGIN_ESTIME"))
  }
  
  marees_avec_metier=marees_captures[!is.na(marees_captures$GR_METIER_COD),]
  marees_avec_metier$GR_GEAR_COD=as.character(marees_avec_metier$GR_ENGIN_COD)
  marees_avec_metier$GR_GEAR_LIB=as.character(marees_avec_metier$GR_ENGIN_LIB)
  # str(marees_avec_metier)
  
  marees_sans_engin=marees_captures[is.na(marees_captures$GR_ENGIN_COD),]
  marees_cas_0=marees_captures[is.na(marees_captures$GR_METIER_COD) & !is.na(marees_captures$GR_ENGIN_COD),]
  marees_cas_0$ENGIN_ESTIME=marees_cas_0$GR_ENGIN_COD
  if (nrow(marees_cas_0)>0) {
    marees_cas_0$AFFECTATION_ENGIN="CAS_0"
  } else {
    marees_cas_0$AFFECTATION_ENGIN=character(0)  
  }
  marees_cas_0=marees_cas_0[,c("ID_MAREE","GR_VESSEL_TYPE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF","ENGIN_ESTIME","AFFECTATION_ENGIN")]
  marees_cas_1=merge(marees_sans_engin,engin_sachant_flottille_espece,by.x=c("GR_VESSEL_TYPE","GR_ESP_COD"),by.y=c("TYPO","GR_ESP_COD"))
  if(nrow(marees_cas_1)>0){
    marees_cas_1$AFFECTATION_ENGIN="CAS_1"
    marees_cas_1=marees_cas_1[,c("ID_MAREE","GR_VESSEL_TYPE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF","ENGIN_ESTIME","AFFECTATION_ENGIN")]
  }
  marees_cas_2=merge(marees_sans_engin[!marees_sans_engin$ID_MAREE%in%marees_cas_1$ID_MAREE & !is.na(marees_sans_engin$GR_ESP_COD),],engin_sachant_espece,by="GR_ESP_COD")
  if(nrow(marees_cas_2)>0){
    marees_cas_2$AFFECTATION_ENGIN="CAS_2"
    marees_cas_2=marees_cas_2[,c("ID_MAREE","GR_VESSEL_TYPE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF","ENGIN_ESTIME","AFFECTATION_ENGIN")]
  }
  marees_cas_3=merge(marees_sans_engin[!marees_sans_engin$ID_MAREE%in%c(marees_cas_1$ID_MAREE,marees_cas_2$ID_MAREE) & !is.na(marees_sans_engin$GR_VESSEL_TYPE),],engin_sachant_flottille,by.x="GR_VESSEL_TYPE",by.y="TYPO")
  if(nrow(marees_cas_3)>0){
    marees_cas_3$AFFECTATION_ENGIN="CAS_3"
    marees_cas_3=marees_cas_3[,c("ID_MAREE","GR_VESSEL_TYPE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF","ENGIN_ESTIME","AFFECTATION_ENGIN")]
  }

  # nouveau traitement
  marees_engins_estimes=rbind(marees_cas_0,marees_cas_1,marees_cas_2,marees_cas_3)
  marees_engins_estimes=setNames(marees_engins_estimes,c("ID_MAREE","GR_VESSEL_TYPE","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF","GR_GEAR_COD","AFFECTATION_ENGIN"))
  table(marees_engins_estimes$AFFECTATION_ENGIN)
  round(100*prop.table(table(marees_engins_estimes$AFFECTATION_ENGIN)),1)
  
  ### --- Ajout du métier regroupé final
  
  # new 
  marees_metiers=rbind(
    marees_avec_metier,
    merge(marees_engins_estimes,ref_agregation_metiers[,c("GR_GEAR_COD","GR_GEAR_LIB","GR_ESP_COD","GR_METIER_COD","GR_METIER_LIB")],by=c("GR_GEAR_COD","GR_ESP_COD"),all.x=T)
  )
  
  #On remet les bonnes captures aux marées
  captures=summaryBy(QUANTITE_CAP_VIF~ID_MAREE+GR_ESP_COD+GR_ESP_LIB,data=CaptureEch,FUN=sum,na.rm=T,keep.names=T)
  marees_metiers=merge(marees_metiers[,c("ID_MAREE","GR_VESSEL_TYPE","GR_GEAR_COD","GR_GEAR_LIB","GR_METIER_COD","GR_METIER_LIB")],captures,all.x=T)
  marees_metiers$QUANTITE_CAP_VIF=replace(marees_metiers$QUANTITE_CAP_VIF,is.na(marees_metiers$QUANTITE_CAP_VIF),0)
  marees_CAS=merge(unique(MareeEch[,c("ID_MAREE","IMMATRICULATION","DATE_DEPART","DATE_RETOUR","PORTDEB_COD","PORTDEB_LIB")]),marees_metiers)[,c("ID_MAREE","IMMATRICULATION","GR_VESSEL_TYPE","DATE_DEPART","DATE_RETOUR","PORTDEB_COD","PORTDEB_LIB","GR_GEAR_COD","GR_GEAR_LIB","GR_METIER_COD","GR_METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF")]
  marees_CAS$JDM_METIER=round(as.numeric(difftime(marees_CAS$DATE_RETOUR,marees_CAS$DATE_DEPART,units="days")))
  marees_CAS$JDM_METIER=replace(marees_CAS$JDM_METIER,marees_CAS$JDM_METIER<1,1)
  marees_CAS=marees_CAS[order(marees_CAS$ID_MAREE),]
  
  ### --- Quelques statistiques descriptives de l'échantillon
  # marees_CAS[marees_CAS$GR_METIER_COD=="FIXSPRC",]
  # marees_CAS[marees_CAS$GR_METIER_COD=="FIXSEMP",]
  
  occurrence_metiers=setNames(summaryBy(ID_MAREE~GR_METIER_COD+GR_METIER_LIB,data=marees_CAS,FUN=NROW),c("GR_METIER_COD","GR_METIER_LIB","NB_MAREES"))
  quantite_metiers=setNames(summaryBy(QUANTITE_CAP_VIF~GR_METIER_COD,data=marees_CAS,FUN=sum,na.rm=T),c("GR_METIER_COD","QUANTITE"))
  infos_metiers_reconstitues=merge(occurrence_metiers,quantite_metiers)
  infos_metiers_reconstitues=infos_metiers_reconstitues[order(-infos_metiers_reconstitues$QUANTITE),]
  
  quantites_par_especes=summaryBy(data=marees_CAS,QUANTITE_CAP_VIF~GR_ESP_COD+GR_ESP_LIB,FUN=sum,na.rm=T,keep.names=T) 
  quantites_par_especes=na.omit(quantites_par_especes[order(-quantites_par_especes$QUANTITE_CAP_VIF),])
} else {
  marees_CAS=setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("ID_MAREE","IMMATRICULATION","GR_VESSEL_TYPE","DATE_DEPART","DATE_RETOUR","PORTDEB_COD","PORTDEB_LIB","GR_GEAR_COD","GR_GEAR_LIB","GR_METIER_COD","GR_METIER_LIB","GR_ESP_COD","GR_ESP_LIB","QUANTITE_CAP_VIF"))
}
