
### --- Sauvegarde des résultats dans un fichier Excel

### ---  Graphiques

### ---Fontion pour arrondir des chiffres décimaux tout en préservant leur somme de départ
smart_round=function(x,digits=0){
  up<-10^digits
  x<-x*up
  y<-floor(x)
  indices<-tail(order(x-y),round(sum(x))-sum(y))
  y[indices]<-y[indices]+1
  return(y/up)
}

if(!file.exists(paste(name_folder,"/Graphiques", sep=""))) {
  dir.create(paste(name_folder,"/Graphiques", sep=""))
  dir.create(paste(name_folder,"/Graphiques/Fiches métiers", sep=""))
  dir.create(paste(name_folder,"/Graphiques/Fiches flottilles", sep=""))
}

#Effort et captures par flottille
N_Q_flottille_plot=estimations_flottilles[-nrow(estimations_flottilles),]
N_Q_flottille_plot$POURCENTAGE_MAREES=100*smart_round(N_Q_flottille_plot$NB_FISHING_TRIPS/sum(N_Q_flottille_plot$NB_FISHING_TRIPS),2)
N_Q_flottille_plot$POURCENTAGE_QUANTITES=100*smart_round(N_Q_flottille_plot$CATCHES/sum(N_Q_flottille_plot$CATCHES),2)
if(nrow(N_Q_flottille_plot)<=12){
  N_Q_flottille_plot$COLOR=brewer.pal(nrow(N_Q_flottille_plot),"Paired")[1:nrow(N_Q_flottille_plot)]
} else {
  N_Q_flottille_plot$COLOR=rep(brewer.pal(12,"Paired"),nrow(N_Q_flottille_plot))[1:nrow(N_Q_flottille_plot)]
}
N_Q_flottille_plot$LABEL_M=paste(N_Q_flottille_plot$GR_VESSEL_TYPE," (",N_Q_flottille_plot$POURCENTAGE_MAREES,"%)",sep="")
N_Q_flottille_plot$LABEL_Q=paste(N_Q_flottille_plot$GR_VESSEL_TYPE," (",round(N_Q_flottille_plot$CATCHES/1000),"t, ",N_Q_flottille_plot$POURCENTAGE_QUANTITES,"%)",sep="")
N_Q_flottille_plot=N_Q_flottille_plot[order(-N_Q_flottille_plot$POURCENTAGE_MAREES),]
N_Q_flottille_plot=N_Q_flottille_plot[N_Q_flottille_plot$POURCENTAGE_MAREES>0 | N_Q_flottille_plot$POURCENTAGE_QUANTITES>0,]
jpeg(paste(name_folder,"/Graphiques/N_Q_flottille.jpeg",sep=""),res=300,width=18,height=7,units="in")
par(mfrow=c(1,2))
pie(N_Q_flottille_plot$POURCENTAGE_MAREES,col=N_Q_flottille_plot$COLOR,labels=N_Q_flottille_plot$LABEL_M,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Nombre de marées par type de navire (%) \n (",sum(estimations_flottilles$NB_FISHING_TRIPS), " marées au total)",sep=""))
pie(N_Q_flottille_plot$POURCENTAGE_QUANTITES,col=N_Q_flottille_plot$COLOR,labels=N_Q_flottille_plot$LABEL_Q,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Quantités capturées par type de navire \n (",round(sum(estimations_flottilles[-nrow(estimations_flottilles),]$CATCHES)/1000), "t au total)",sep=""))
dev.off()

#Effort et captures par métier
N_Q_metier_plot=estimations_metiers[-nrow(estimations_metiers),]
N_Q_metier_plot$POURCENTAGE_MAREES=100*smart_round(N_Q_metier_plot$NB_FISHING_TRIPS/sum(N_Q_metier_plot$NB_FISHING_TRIPS),2)
N_Q_metier_plot$POURCENTAGE_QUANTITES=100*smart_round(N_Q_metier_plot$CATCHES/sum(N_Q_metier_plot$CATCHES),2)
if(nrow(N_Q_metier_plot)<=12){
  N_Q_metier_plot$COLOR=brewer.pal(nrow(N_Q_metier_plot),"Paired")[1:nrow(N_Q_metier_plot)]
} else {
  N_Q_metier_plot$COLOR=rep(brewer.pal(12,"Paired"),nrow(N_Q_metier_plot))[1:nrow(N_Q_metier_plot)]
}
N_Q_metier_plot$LABEL_M=paste(N_Q_metier_plot$METIER_LIB," (",N_Q_metier_plot$POURCENTAGE_MAREES,"%)",sep="")
N_Q_metier_plot$LABEL_Q=paste(N_Q_metier_plot$METIER_LIB," (",round(N_Q_metier_plot$CATCHES/1000),"t, ",N_Q_metier_plot$POURCENTAGE_QUANTITES,"%)",sep="")
N_Q_metier_plot=N_Q_metier_plot[order(-N_Q_metier_plot$POURCENTAGE_MAREES),]
N_Q_metier_plot=N_Q_metier_plot[N_Q_metier_plot$POURCENTAGE_MAREES>0 | N_Q_metier_plot$POURCENTAGE_QUANTITES>0,]
jpeg(paste(name_folder,"/Graphiques/N_Q_metier.jpeg",sep=""),res=300,width=18,height=7,units="in")
par(mfrow=c(1,2))
pie(N_Q_metier_plot$POURCENTAGE_MAREES,col=N_Q_metier_plot$COLOR,labels=N_Q_metier_plot$LABEL_M,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Nombre de marées par métier (%) \n (",sum(estimations_metiers$NB_FISHING_TRIPS), " marées au total)",sep=""))
pie(N_Q_metier_plot$POURCENTAGE_QUANTITES,col=N_Q_metier_plot$COLOR,labels=N_Q_metier_plot$LABEL_Q,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Quantités capturées par métier \n (",round(sum(estimations_metiers[-nrow(estimations_metiers),]$CATCHES)/1000), "t au total)",sep=""))
dev.off()

#Effort et captures par région
N_Q_region_plot=estimations_regions[-nrow(estimations_regions),]
N_Q_region_plot$POURCENTAGE_MAREES=100*smart_round(N_Q_region_plot$NB_FISHING_TRIPS/sum(N_Q_region_plot$NB_FISHING_TRIPS),2)
N_Q_region_plot$POURCENTAGE_QUANTITES=100*smart_round(N_Q_region_plot$CATCHES/sum(N_Q_region_plot$CATCHES),2)
if(nrow(N_Q_region_plot)<=12){
  N_Q_region_plot$COLOR=brewer.pal(nrow(N_Q_region_plot),"Paired")[1:nrow(N_Q_region_plot)]
} else {
  N_Q_region_plot$COLOR=rep(brewer.pal(12,"Paired"),nrow(N_Q_region_plot))[1:nrow(N_Q_region_plot)]
}
N_Q_region_plot$LABEL_M=paste(N_Q_region_plot$REGION_LIB," (",N_Q_region_plot$POURCENTAGE_MAREES,"%)",sep="")
N_Q_region_plot$LABEL_Q=paste(N_Q_region_plot$REGION_LIB," (",round(N_Q_region_plot$CATCHES/1000),"t, ",N_Q_region_plot$POURCENTAGE_QUANTITES,"%)",sep="")
N_Q_region_plot=N_Q_region_plot[order(-N_Q_region_plot$POURCENTAGE_MAREES),]
N_Q_region_plot=N_Q_region_plot[N_Q_region_plot$POURCENTAGE_MAREES>0 | N_Q_region_plot$POURCENTAGE_QUANTITES>0,]
jpeg(paste(name_folder,"/Graphiques/N_Q_region.jpeg",sep=""),res=300,width=18,height=7,units="in")
par(mfrow=c(1,2))
pie(N_Q_region_plot$POURCENTAGE_MAREES,col=N_Q_region_plot$COLOR,labels=N_Q_region_plot$LABEL_M,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Nombre de marées par région (%) \n (",sum(estimations_regions$NB_FISHING_TRIPS), " marées au total)",sep=""))
pie(N_Q_region_plot$POURCENTAGE_QUANTITES,col=N_Q_region_plot$COLOR,labels=N_Q_region_plot$LABEL_Q,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Quantités capturées par région \n (",round(sum(estimations_regions[-nrow(estimations_regions),]$CATCHES)/1000), "t au total)",sep=""))
dev.off()

#Composition spécifique globale : on ne garde que les 10 premières espèces (regroupement des suivantes dans "Autres espèces") et on transforme les tonnages en %
composition_specifique_global_plot=estimations_especes[-nrow(estimations_especes),c("GR_ESP_COD","GR_ESP_LIB","CATCHES")]
composition_specifique_global_plot$POURCENTAGE_QUANTITES=100*smart_round(composition_specifique_global_plot$CATCHES/sum(composition_specifique_global_plot$CATCHES),2)
composition_specifique_global_plot$GR_ESP_COD=as.character(composition_specifique_global_plot$GR_ESP_COD)
composition_specifique_global_plot$GR_ESP_COD=as.character(composition_specifique_global_plot$GR_ESP_LIB)
if (nrow(composition_specifique_global_plot)>=10) {
  tmp=composition_specifique_global_plot[1:10,]
  tmp[11,"GR_ESP_COD"]=""
  tmp[11,"GR_ESP_LIB"]="Autres espèces"
  tmp[11,"CATCHES"]=sum(composition_specifique_global_plot$CATCHES)-sum(tmp$CATCHES,na.rm=T)
  tmp[11,"POURCENTAGE_QUANTITES"]=100-sum(tmp$POURCENTAGE_QUANTITES,na.rm=T)
} else {
  tmp=composition_specifique_global_plot
}
composition_specifique_global_plot=tmp
composition_specifique_global_plot$COLOR=brewer.pal(nrow(composition_specifique_global_plot),"Paired")[1:nrow(composition_specifique_global_plot)]
composition_specifique_global_plot$LABEL=paste(tmp$GR_ESP_LIB," (",round(composition_specifique_global_plot$CATCHES/1000),"t, ",composition_specifique_global_plot$POURCENTAGE_QUANTITES,"%)",sep="")
jpeg(paste(name_folder,"/Graphiques/composition_specifique_globale.jpeg",sep=""),res=300,width=9,height=7,units="in")
pie(composition_specifique_global_plot$POURCENTAGE_QUANTITES,col=composition_specifique_global_plot$COLOR,labels=composition_specifique_global_plot$LABEL,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Composition spécifique globale \n (",round(sum(estimations_especes[-nrow(estimations_especes),]$CATCHES)/1000), "t au total)",sep=""))
dev.off()

#Composition spécifique par type de navire : on ne garde que les 10 premières espèces (regroupement des suivantes dans "Autres espèces") et on transforme les tonnages en %
# typos=unique(estimations_flottilles_especes$GR_VESSEL_TYPE)
# jpeg(paste(name_folder,"/Graphiques/Fiches flottilles/composition_specifique_type_navires.jpeg",sep=""),res=300,width=7,height=8,units="in")
# par(mfrow=n2mfrow(length(typos)))
# for(typo in typos){
#   tmp0=estimations_flottilles_especes[estimations_flottilles_especes$GR_VESSEL_TYPE==typo,]
#   cs_typo_plot=tmp0[,c("GR_ESP_COD","GR_ESP_LIB","CATCHES")]
#   cs_typo_plot$POURCENTAGE_QUANTITES=100*smart_round(cs_typo_plot$CATCHES/sum(cs_typo_plot$CATCHES),2)
#   cs_typo_plot$GR_ESP_COD=as.character(cs_typo_plot$GR_ESP_COD)
#   cs_typo_plot$GR_ESP_LIB=as.character(cs_typo_plot$GR_ESP_LIB)
#   cs_typo_plot=cs_typo_plot[order(-cs_typo_plot$CATCHES),]
#   if (nrow(cs_typo_plot)>=10) {
#     tmp=cs_typo_plot[1:10,]
#     tmp[11,"GR_ESP_COD"]=""
#     tmp[11,"GR_ESP_LIB"]="Autres espèces"
#     tmp[11,"CATCHES"]=sum(cs_typo_plot$CATCHES)-sum(tmp$CATCHES,na.rm=T)
#     tmp[11,"POURCENTAGE_QUANTITES"]=100-sum(tmp$POURCENTAGE_QUANTITES,na.rm=T)
#   } else {
#     tmp=cs_typo_plot
#   }
#   cs_typo_plot=tmp
#   cs_typo_plot$COLOR=brewer.pal(nrow(cs_typo_plot),"Paired")[1:nrow(cs_typo_plot)]
#   cs_typo_plot$LABEL=paste(tmp$GR_ESP_LIB," (",round(cs_typo_plot$CATCHES/1000),"t, ",cs_typo_plot$POURCENTAGE_QUANTITES,"%)",sep="")
#   pie(cs_typo_plot$POURCENTAGE_QUANTITES,col=cs_typo_plot$COLOR,labels=cs_typo_plot$LABEL,xpd=NA,cex=0.6,radius=0.7,init.angle=0,main=paste(tmp0$GR_VESSEL_TYPE[1],"\n Composition spécifique (",round(sum(cs_typo_plot$CATCHES)/1000), "t au total)",sep=""))
# }
# dev.off()

#Infos par métiers : répartition NB_FISHING_TRIPS + CATCHES par type de navires + composition spécifique du métier
metiers=unique(estimations_finales$METIER_COD)
for(metier in metiers){
  tmp=estimations_flottille_metiers[estimations_flottille_metiers$METIER_COD==metier,]
  tmp$POURCENTAGE_MAREES=100*smart_round(tmp$NB_FISHING_TRIPS/sum(tmp$NB_FISHING_TRIPS),2)
  tmp$POURCENTAGE_QUANTITES=100*smart_round(tmp$CATCHES/sum(tmp$CATCHES),2)
  tmp$COLOR=brewer.pal(nrow(tmp),"Paired")[1:nrow(tmp)]
  tmp$LABEL_M=paste(tmp$GR_VESSEL_TYPE," (",tmp$POURCENTAGE_MAREES,"%)",sep="")
  tmp$LABEL_Q=paste(tmp$GR_VESSEL_TYPE," (",round(tmp$CATCHES/1000),"t, ",tmp$POURCENTAGE_QUANTITES,"%)",sep="")
  tmp=tmp[order(-tmp$POURCENTAGE_MAREES),]
  tmp=tmp[tmp$POURCENTAGE_MAREES>0 | tmp$POURCENTAGE_QUANTITES>0,]
  tmp1=summaryBy(CATCHES~GR_ESP_COD+GR_ESP_LIB,data=estimations_finales[estimations_finales$METIER_COD==metier,],FUN=sum,keep.names=T)
  tmp1$POURCENTAGE_QUANTITES=100*smart_round(tmp1$CATCHES/sum(tmp1$CATCHES),2)
  tmp1$GRESP_COD=as.character(tmp1$GR_ESP_COD)
  tmp1$GRESP_LIB=as.character(tmp1$GR_ESP_LIB)
  tmp1=tmp1[order(-tmp1$CATCHES),]
  if (nrow(tmp1)>=10) {
    tmp2=tmp1[1:10,]
    tmp2[11,"GR_ESP_COD"]=""
    tmp2[11,"GR_ESP_LIB"]="Autres espèces"
    tmp2[11,"CATCHES"]=sum(tmp1$CATCHES)-sum(tmp2$CATCHES,na.rm=T)
    tmp2[11,"POURCENTAGE_QUANTITES"]=100-sum(tmp2$POURCENTAGE_QUANTITES,na.rm=T)
  } else {
    tmp2=tmp1
  }
  tmp1=tmp2
  tmp1$COLOR=brewer.pal(nrow(tmp1),"Paired")[1:nrow(tmp1)]
  tmp1$LABEL=paste(tmp1$GR_ESP_LIB," (",round(tmp1$CATCHES/1000),"t, ",tmp1$POURCENTAGE_QUANTITES,"%)",sep="")
  jpeg(paste(name_folder,"/Graphiques/Fiches métiers/",metier,".jpeg",sep=""),res=300,width=13,height=6,units="in")
  par(mfrow=c(1,3),oma=c(0,5,5,5))
  pie(tmp$POURCENTAGE_MAREES,col=tmp$COLOR,labels=tmp$LABEL_M,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Nombre de marées par type de navire (%) \n (",sum(tmp$NB_FISHING_TRIPS), " marées au total)",sep=""))
  pie(tmp$POURCENTAGE_QUANTITES,col=tmp$COLOR,labels=tmp$LABEL_Q,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Quantités capturées par type de navire \n (",round(sum(tmp$CATCHES)/1000), "t au total)",sep=""))
  pie(tmp1$POURCENTAGE_QUANTITES,col=tmp1$COLOR,labels=tmp1$LABEL,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main="Composition spécifique")
  title(paste(metier,tmp$METIER_LIB[1],sep="\n"),outer=TRUE)
  dev.off()
}

#Infos par flottilles : répartition NB_FISHING_TRIPS + CATCHES par métier + composition spécifique de la flottille
flottilles=unique(estimations_finales$GR_VESSEL_TYPE)
for(flottille in flottilles){
  tmp=estimations_flottille_metiers[estimations_flottille_metiers$GR_VESSEL_TYPE==flottille,]
  tmp$POURCENTAGE_MAREES=100*smart_round(tmp$NB_FISHING_TRIPS/sum(tmp$NB_FISHING_TRIPS),2)
  tmp$POURCENTAGE_QUANTITES=100*smart_round(tmp$CATCHES/sum(tmp$CATCHES),2)
  tmp$COLOR=brewer.pal(nrow(tmp),"Paired")[1:nrow(tmp)]
  tmp$LABEL_M=paste(tmp$METIER_LIB," (",tmp$POURCENTAGE_MAREES,"%)",sep="")
  tmp$LABEL_Q=paste(tmp$METIER_LIB," (",round(tmp$CATCHES/1000),"t, ",tmp$POURCENTAGE_QUANTITES,"%)",sep="")
  tmp=tmp[order(-tmp$POURCENTAGE_MAREES),]
  tmp=tmp[tmp$POURCENTAGE_MAREES>0 | tmp$POURCENTAGE_QUANTITES>0,]
  tmp1=summaryBy(CATCHES~GR_ESP_COD+GR_ESP_LIB,data=estimations_finales[estimations_finales$GR_VESSEL_TYPE==flottille,],FUN=sum,keep.names=T)
  tmp1$POURCENTAGE_QUANTITES=100*smart_round(tmp1$CATCHES/sum(tmp1$CATCHES),2)
  tmp1$GRESP_COD=as.character(tmp1$GR_ESP_COD)
  tmp1$GRESP_LIB=as.character(tmp1$GR_ESP_LIB)
  tmp1=tmp1[order(-tmp1$CATCHES),]
  if (nrow(tmp1)>=10) {
    tmp2=tmp1[1:10,]
    tmp2[11,"GR_ESP_COD"]=""
    tmp2[11,"GR_ESP_LIB"]="Autres espèces"
    tmp2[11,"CATCHES"]=sum(tmp1$CATCHES)-sum(tmp2$CATCHES,na.rm=T)
    tmp2[11,"POURCENTAGE_QUANTITES"]=100-sum(tmp2$POURCENTAGE_QUANTITES,na.rm=T)
  } else {
    tmp2=tmp1
  }
  tmp1=tmp2
  tmp1$COLOR=brewer.pal(nrow(tmp1),"Paired")[1:nrow(tmp1)]
  tmp1$LABEL=paste(tmp1$GR_ESP_LIB," (",round(tmp1$CATCHES/1000),"t, ",tmp1$POURCENTAGE_QUANTITES,"%)",sep="")
  jpeg(paste(name_folder,"/Graphiques/Fiches flottilles/",flottille,".jpeg",sep=""),res=300,width=13,height=6,units="in")
  par(mfrow=c(1,3),oma=c(0,5,5,5))
  pie(tmp$POURCENTAGE_MAREES,col=tmp$COLOR,labels=tmp$LABEL_M,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Nombre de marées par métier (%) \n (",sum(tmp$NB_FISHING_TRIPS), " marées au total)",sep=""))
  pie(tmp$POURCENTAGE_QUANTITES,col=tmp$COLOR,labels=tmp$LABEL_Q,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main=paste("Quantités capturées par métier \n (",round(sum(tmp$CATCHES)/1000), "t au total)",sep=""))
  pie(tmp1$POURCENTAGE_QUANTITES,col=tmp1$COLOR,labels=tmp1$LABEL,xpd=NA,cex=0.8,radius=0.7,init.angle=0,main="Composition spécifique")
  title(flottille,outer=TRUE)
  dev.off()
}

### --- Sauvegarde des résultats dans un fichier Excel

options(java.parameters = "-Xmx8000m")

name_file=paste(name_folder,"/estimations_",annee,".xlsx",sep="")
wb=createWorkbook(type="xlsx")
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) + Alignment(horizontal="ALIGN_JUSTIFY") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK")) 
cs=CellStyle(wb,dataFormat=DataFormat("#,##0")) + Alignment(horizontal="ALIGN_CENTER")   #Séparateur de milliers dans les chiffres + Tout centré
cs1=CellStyle(wb) + Alignment(horizontal="ALIGN_CENTER")   #Tout centré
texte_intro1=data.frame(TEXT=c(paste("Estimations of fishing effort and catches in Seychelles for ", annee,".",sep=""),"","Fleet register :"))

texte_intro2=data.frame(TEXT="Data source used :")

source_donnees=data.frame(DATA_SOURCE=c("WEEKLY_RECORD_OF_ACTIVITY","CAS","DROPLINES","LOGBOOKS_SEMI_INDUSTRIAL_LONGLINERS","LOGBOOKS_LOBSTERS","LOGBOOKS_SEA_CUCUMBERS","VMS"),
                          NB_VESSELS=c(length(unique(FINSS$SZ)),length(unique(marees_CAS$IMMATRICULATION)),length(unique(marees_droplines$NAVIRE)),length(unique(marees_SEMI_LL$NatRegNumber)),length(unique(marees_lobsters$NAVIRE)),length(unique(marees_sea_cucumbers$NAVIRE)),length(unique(marees_VMS_finales$NAVIRE))),
                          NB_OBSERVATIONS=c(length(unique(FINSS$TripID)),length(unique(marees_CAS$ID_MAREE)),length(unique(marees_droplines$TripID)),length(unique(marees_SEMI_LL$TripHistoryID)),nrow(marees_lobsters),nrow(marees_sea_cucumbers),nrow(marees_VMS_finales)),
                          QUANTITY=c(NA,round(sum(marees_CAS$QUANTITE_CAP_VIF,na.rm=T)),round(sum(marees_droplines$CatchWeight,na.rm=T)),round(sum(marees_SEMI_LL$Catch_Kg,na.rm=T)),round(sum(marees_lobsters$QUANTITE,na.rm=T)),round(sum(marees_sea_cucumbers$QUANTITE,na.rm=T)),NA))
source_donnees$DATA_SOURCE=as.character(source_donnees$DATA_SOURCE)
source_donnees[nrow(source_donnees)+1,c("DATA_SOURCE","NB_VESSELS","NB_OBSERVATIONS","QUANTITY")]=c("TOTAL","",sum(source_donnees$NB_OBSERVATIONS),sum(source_donnees$QUANTITY,na.rm=T))
source_donnees[c("NB_VESSELS","NB_OBSERVATIONS","QUANTITY")]=sapply(source_donnees[,c("NB_VESSELS","NB_OBSERVATIONS","QUANTITY")],as.numeric)

nb_navires_typo=setNames(summaryBy(data=fichier_flotte,NAVIRE~GR_VESSEL_TYPE,FUN=NROW),c("GR_VESSEL_TYPE","NB_VESSELS"))
nb_navires_typo=nb_navires_typo[order(-nb_navires_typo$NB_VESSELS),]
nb_navires_typo$GR_VESSEL_TYPE=as.character(nb_navires_typo$GR_VESSEL_TYPE)
nb_navires_typo[nrow(nb_navires_typo)+1,c("GR_VESSEL_TYPE","NB_VESSELS")]=c("TOTAL",sum(nb_navires_typo$NB_VESSELS))
nb_navires_typo$NB_VESSELS=as.numeric(nb_navires_typo$NB_VESSELS)

a=unique(na.omit(calendrier_mensuel[,c("NAVIRE","GR_VESSEL_TYPE","METIER_LIB")]))
a=addmargins(table(a$METIER_LIB,a$GR_VESSEL_TYPE),2)
nb_navires_flottille_metier=as.data.frame.matrix(a)
names(nb_navires_flottille_metier)[ncol(nb_navires_flottille_metier)]="TOTAL"

sheet=createSheet(wb,sheetName="Introduction")
addDataFrame(texte_intro1,sheet,row.names=FALSE,col.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
addDataFrame(nb_navires_typo,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE,startRow=nrow(texte_intro1)+1)
addDataFrame(texte_intro2,sheet,row.names=FALSE,col.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE,startRow=nrow(texte_intro1)+nrow(nb_navires_typo)+3)
addDataFrame(source_donnees,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE,startRow=nrow(texte_intro1)+nrow(nb_navires_typo)+nrow(texte_intro2)+3)
autoSizeColumn(sheet,colIndex=1:ncol(source_donnees))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Effectifs_navires_métiers")
addDataFrame(nb_navires_flottille_metier,sheet,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(nb_navires_flottille_metier))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="ref_agregation_lieux")
addDataFrame(ref_ports[,c("LABEL","NAME","District","libellédistrict","Region","libelle.région","Island")],sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(ref_ports[,c("LABEL","NAME","District","libellédistrict","Region","libelle.région","Island")]))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="ref_agregation_especes")
addDataFrame(ref_agregation_especes,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(ref_agregation_especes))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="ref_agregation_metiers")
addDataFrame(ref_agregation_metiers,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(ref_agregation_metiers))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

facteurs_extrapolation=setNames(taux_enquetes_par_region_flottille_FINSS,c("REGION","GR_VESSEL_TYPE","NB_ENQUETES_HEBDO_MOYEN","TAUX_COMPLETUDE"))
facteurs_extrapolation=merge(facteurs_extrapolation,setNames(unique(ref_ports[,c("Region","libelle.région")]),c("REGION","REGION_LIB")),by="REGION")[,c("REGION","REGION_LIB","GR_VESSEL_TYPE","NB_ENQUETES_HEBDO_MOYEN","TAUX_COMPLETUDE")]
facteurs_extrapolation$FACTEUR_EXTRAPOLATION=1/facteurs_extrapolation$TAUX_COMPLETUDE
facteurs_extrapolation[,c("NB_ENQUETES_HEBDO_MOYEN","TAUX_COMPLETUDE","FACTEUR_EXTRAPOLATION")]=round(facteurs_extrapolation[,c("NB_ENQUETES_HEBDO_MOYEN","TAUX_COMPLETUDE","FACTEUR_EXTRAPOLATION")],2)
sheet=createSheet(wb,sheetName="Facteurs_Extrapolation")
addDataFrame(facteurs_extrapolation,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(facteurs_extrapolation))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs1) 

sheet=createSheet(wb,sheetName="Estimations_ALL")
estimations_finales$PRECISION_CI_95=paste(estimations_finales$PRECISION_CI_95,"%",sep="")
addDataFrame(estimations_finales,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_finales))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Global")
addDataFrame(estimations_all,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_all))
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Flottilles")
addDataFrame(estimations_flottilles,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_flottilles))
addPicture(paste(name_folder,"/Graphiques/N_Q_flottille.jpeg",sep=""),sheet,scale=0.7,startRow=nrow(estimations_flottilles)+3)
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Métiers")
addDataFrame(estimations_metiers,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_metiers))
addPicture(paste(name_folder,"/Graphiques/N_Q_metier.jpeg",sep=""),sheet,scale=0.7,startRow=nrow(estimations_metiers)+3)
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Espèces")
addDataFrame(estimations_especes,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_especes))
addPicture(paste(name_folder,"/Graphiques/composition_specifique_globale.jpeg",sep=""),sheet,scale=0.8,startColumn=ncol(estimations_especes)+2)
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

sheet=createSheet(wb,sheetName="Régions")
addDataFrame(estimations_regions,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
autoSizeColumn(sheet,colIndex=1:ncol(estimations_regions))
addPicture(paste(name_folder,"/Graphiques/N_Q_region.jpeg",sep=""),sheet,scale=0.7,startRow=nrow(estimations_regions)+3)
cells=getCells(getRows(sheet))
for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 

for(flottille in unique(estimations_finales$GR_VESSEL_TYPE)){
  tmp=estimations_flottille_metiers[estimations_flottille_metiers$GR_VESSEL_TYPE==flottille,c("GR_VESSEL_TYPE","METIER_COD","METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]
  tmp=tmp[order(-tmp$CATCHES),]
  tmp$PRECISION_CI_95=paste(round(100*(tmp$CATCHES_SUP_95-tmp$CATCHES_INF_95)/(2*tmp$CATCHES)),"%",sep="")
  tmp$MEAN_CATCH_PER_TRIP=round(tmp$CATCHES/tmp$NB_FISHING_TRIPS)
  tmp$MEAN_CATCH_PER_DAY_AT_SEA=round(tmp$CATCHES/tmp$DAYS_AT_SEA)
  tmp[nrow(tmp)+1,c("METIER_LIB","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(tmp$DAYS_AT_SEA),sum(tmp$NB_FISHING_TRIPS),sum(tmp$CATCHES),sum(tmp$CATCHES_INF_95),sum(tmp$CATCHES_SUP_95))
  tmp[c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(tmp[,c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
  tmp$PRECISION_CI_95[nrow(tmp)]=paste(round(100*((tmp$CATCHES_SUP_95-tmp$CATCHES_INF_95)/(2*tmp$CATCHES))[nrow(tmp)]),"%",sep="")
  tmp1=summaryBy(data=estimations_finales[estimations_finales$GR_VESSEL_TYPE==flottille,],CATCHES+CATCHES_INF_95+CATCHES_SUP_95~GR_VESSEL_TYPE+GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)
  tmp1=tmp1[order(-tmp1$CATCHES),]
  tmp1$PRECISION_CI_95=paste(round(100*(tmp1$CATCHES_SUP_95-tmp1$CATCHES_INF_95)/(2*tmp1$CATCHES)),"%",sep="")
  tmp1[nrow(tmp1)+1,c("GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(tmp1$CATCHES,na.rm=T),sum(tmp1$CATCHES_INF_95,na.rm=T),sum(tmp1$CATCHES_SUP_95,na.rm=T))
  tmp1[c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(tmp1[,c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
  tmp1$PRECISION_CI_95[nrow(tmp1)]=paste(round(100*((tmp1$CATCHES_SUP_95-tmp1$CATCHES_INF_95)/(2*tmp1$CATCHES))[nrow(tmp1)]),"%",sep="")
  sheet=createSheet(wb,sheetName=paste("Flottille",flottille,sep="_"))
  addDataFrame(tmp,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
  addDataFrame(tmp1,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE,startRow=nrow(tmp)+3)
  autoSizeColumn(sheet,colIndex=1:ncol(tmp))
  cells=getCells(getRows(sheet))
  for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 
  addPicture(paste(name_folder,"/Graphiques/Fiches flottilles/",flottille,".jpeg",sep=""),sheet,scale=1,startRow=nrow(tmp)+nrow(tmp1)+5)
}

for(metier in unique(estimations_finales$METIER_COD)){
  tmp=estimations_flottille_metiers[estimations_flottille_metiers$METIER_COD==metier,c("METIER_COD","METIER_LIB","GR_VESSEL_TYPE","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]
  tmp=tmp[order(-tmp$CATCHES),]
  tmp$PRECISION_CI_95=paste(round(100*(tmp$CATCHES_SUP_95-tmp$CATCHES_INF_95)/(2*tmp$CATCHES)),"%",sep="")
  tmp$MEAN_CATCH_PER_TRIP=round(tmp$CATCHES/tmp$NB_FISHING_TRIPS)
  tmp$MEAN_CATCH_PER_DAY_AT_SEA=round(tmp$CATCHES/tmp$DAYS_AT_SEA)
  tmp$GR_VESSEL_TYPE=as.character(tmp$GR_VESSEL_TYPE)
  tmp[nrow(tmp)+1,c("GR_VESSEL_TYPE","DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(tmp$DAYS_AT_SEA),sum(tmp$NB_FISHING_TRIPS),sum(tmp$CATCHES),sum(tmp$CATCHES_INF_95),sum(tmp$CATCHES_SUP_95))
  tmp[c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(tmp[,c("DAYS_AT_SEA","NB_FISHING_TRIPS","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
  tmp$PRECISION_CI_95[nrow(tmp)]=paste(round(100*((tmp$CATCHES_SUP_95-tmp$CATCHES_INF_95)/(2*tmp$CATCHES))[nrow(tmp)]),"%",sep="")
  tmp1=summaryBy(data=estimations_finales[estimations_finales$METIER_COD==metier,],CATCHES+CATCHES_INF_95+CATCHES_SUP_95~METIER_COD+METIER_LIB+GR_ESP_COD+GR_ESP_LIB,FUN=sum,keep.names=T)
  tmp1=tmp1[order(-tmp1$CATCHES),]
  tmp1$PRECISION_CI_95=paste(round(100*(tmp1$CATCHES_SUP_95-tmp1$CATCHES_INF_95)/(2*tmp1$CATCHES)),"%",sep="")
  tmp1[nrow(tmp1)+1,c("GR_ESP_LIB","CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=c("TOTAL",sum(tmp1$CATCHES,na.rm=T),sum(tmp1$CATCHES_INF_95,na.rm=T),sum(tmp1$CATCHES_SUP_95,na.rm=T))
  tmp1[c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")]=sapply(tmp1[,c("CATCHES","CATCHES_INF_95","CATCHES_SUP_95")],as.numeric)
  tmp1$PRECISION_CI_95[nrow(tmp1)]=paste(round(100*((tmp1$CATCHES_SUP_95-tmp1$CATCHES_INF_95)/(2*tmp1$CATCHES))[nrow(tmp1)]),"%",sep="")
  sheet=createSheet(wb,sheetName=paste("Métier",metier,sep="_"))
  addDataFrame(tmp,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE)
  addDataFrame(tmp1,sheet,row.names=FALSE,colnamesStyle=TABLE_COLNAMES_STYLE,startRow=nrow(tmp)+3)
  autoSizeColumn(sheet,colIndex=1:ncol(tmp))
  cells=getCells(getRows(sheet))
  for (h in 1:length(cells)) setCellStyle(cells[[h]],cs) 
  addPicture(paste(name_folder,"/Graphiques/Fiches métiers/",metier,".jpeg",sep=""),sheet,scale=1,startRow=nrow(tmp)+nrow(tmp1)+5)
}


saveWorkbook(wb,name_file)


### --- Sauvegarde des données + résultats dans un fichier RData

rm(list=(ls()[!ls()%in%c("annee","calendrier_journalier","calendrier_mensuel","estimations_finales","estimations_especes","estimations_metiers","estimations_metiers_especes","estimations_regions","estimations_regions_flottilles_metiers","estimations_flottilles_especes","estimations_flottilles","facteurs_extrapolation","fichier_flotte","FINSS","marees_droplines","marees_lobsters","marees_CAS","marees_SEMI_LL","marees_sea_cucumbers","marees_VMS_finales","port_plus_frequent","ref_agregation_engins","ref_agregation_especes","ref_agregation_metiers","ref_ports","x_bar","name_folder")]))

save.image(paste(name_folder,"/estimations_",annee,".RData",sep=""))

