### --- Reconstruction du fichier MAREES_VMS_SORTIE_ALGOPESCA
source("Codes R/preparation_marees_AlgoPesca.r")

### --- Consolidation marées en sortie d'AlgoPesca : 
# 1) Application d'un seuil de tolérance sur la distance début marée et fin de marée par rapport au port
# 2) Raccrochage des morceaux de marées dans des marées consolidées

## -- Importation des marées VMS issues du traitement AlgoPesca 2014-2015

marees_VMS=read.csv("Données/MAREES_VMS_SORTIE_ALGOPESCA.csv",sep=";",header=T)
#Marées VMS en temps UTC. Seychelles = UTC+4 => ajouter 4h
marees_VMS$DEPARTURE_DATE_TIME=strptime(as.character(marees_VMS$DEPARTURE_DATE_TIME),"%Y-%m-%d %H:%M:%S")+4*3600
marees_VMS$RETURN_DATE_TIME=strptime(as.character(marees_VMS$RETURN_DATE_TIME),"%Y-%m-%d %H:%M:%S")+4*3600
marees_VMS$ANNEE=as.numeric(format(marees_VMS$DEPARTURE_DATE_TIME,"%Y"))
marees_VMS$MOIS=as.numeric(format(marees_VMS$RETURN_DATE_TIME,"%m"))
marees_VMS=marees_VMS[order(marees_VMS$VESSEL_FK,marees_VMS$DEPARTURE_DATE_TIME),]
marees_VMS=merge(marees_VMS,fichier_flotte[,c("NAVIRE","GR_VESSEL_TYPE")],by.x="VESSEL_FK",by.y="NAVIRE",all.x=TRUE)
marees_VMS$DEPARTURE_QUALITY=as.character(marees_VMS$DEPARTURE_QUALITY)
marees_VMS$RETURN_QUALITY=as.character(marees_VMS$RETURN_QUALITY)

#Si distance par rapport au port de départ ou de retour <= seuil_distance, alors on considère marée OK
seuil_distance=5
marees_VMS$DEPARTURE_QUALITY_NEW=marees_VMS$DEPARTURE_QUALITY
marees_VMS$RETURN_QUALITY_NEW=marees_VMS$RETURN_QUALITY
if (!is.null(marees_VMS$DEPARTURE_LOCATION_DIST)) {
  marees_VMS[!is.na(marees_VMS$DEPARTURE_LOCATION_DIST) & marees_VMS$DEPARTURE_LOCATION_DIST<=seuil_distance,]$DEPARTURE_QUALITY_NEW="OK"
}
if (!is.null(marees_VMS$RETURN_LOCATION_DIST)) {
  marees_VMS[!is.na(marees_VMS$RETURN_LOCATION_DIST) & marees_VMS$RETURN_LOCATION_DIST<=seuil_distance,]$RETURN_QUALITY_NEW="OK"
}

### --- Identifier et raccrocher les morceaux de marées
#Si port départ OK et port de retour!=OK, et tant que |date_depart_maree_m+1 - date_retour_maree_m|< tolerance_time jours (2j) & port_depart!=OK jusqu'à ce que port_retour=OK, alors fait partie de la même marée
tolerance_time=2 #si moins de 2 jours entre 2 marées consécutives, potentiellement raccrochables entre elles dans une seule marée
marees_VMS_navires=split(marees_VMS,marees_VMS$VESSEL_FK)
new_marees_VMS=c()
for(navire in names(marees_VMS_navires)){
  tmp=marees_VMS_navires[[navire]]
  tmp=tmp[order(tmp$VESSEL_FK,tmp$DEPARTURE_DATE_TIME),c("VESSEL_FK","GR_VESSEL_TYPE","ANNEE","MOIS","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","DEPARTURE_LOCATION_DIST","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","DEPARTURE_DATE_TIME","RETURN_DATE_TIME","DEPARTURE_QUALITY_NEW","RETURN_QUALITY_NEW","FISHING_TIME")]
  if(nrow(tmp)==1){
    new_marees_navire=tmp[,c("VESSEL_FK","GR_VESSEL_TYPE","ANNEE","MOIS","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","DEPARTURE_LOCATION_DIST","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","DEPARTURE_DATE_TIME","RETURN_DATE_TIME","DEPARTURE_QUALITY_NEW","RETURN_QUALITY_NEW","FISHING_TIME")]
  } else {
    tmp$DEBUT_POTENTIEL=tmp$DEPARTURE_QUALITY_NEW=="OK"
    tmp$FIN_POTENTIEL=tmp$RETURN_QUALITY_NEW=="OK"
    tmp$SAME_PORT=c(NA,as.character(tmp$DEPARTURE_LOCATION_LABEL[2:nrow(tmp)])==as.character(tmp$RETURN_LOCATION_LABEL[1:(nrow(tmp)-1)]))
    tmp$DIFF_TIME=c(NA,(tmp$DEPARTURE_DATE_TIME[2:nrow(tmp)]-tmp$RETURN_DATE_TIME[1:(nrow(tmp)-1)])/24)
    tmp$SAME_TIME=tmp$DIFF_TIME<tolerance_time
    new_marees_navire=c()
    i=1
    while(i<nrow(tmp)){
      if(tmp$DEBUT_POTENTIEL[i] & !tmp$FIN_POTENTIEL[i]){
        ind_new_maree_potentielle=i
        j=i+1
        while(!tmp$DEBUT_POTENTIEL[j] & !tmp$DEBUT_POTENTIEL[j] & tmp$SAME_TIME[j] & j<=nrow(tmp)){
          ind_new_maree_potentielle=c(ind_new_maree_potentielle,j)
          j=j+1
        }
        if(!tmp$DEBUT_POTENTIEL[j-1] & tmp$FIN_POTENTIEL[j-1]){
          new_maree0=tmp[ind_new_maree_potentielle,]
          new_maree=new_maree0[1,]
          new_maree[,c("RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","RETURN_DATE_TIME","RETURN_QUALITY_NEW","FISHING_TIME")]=new_maree0[nrow(new_maree0),c("RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","RETURN_DATE_TIME","RETURN_QUALITY_NEW","FISHING_TIME")]
          new_maree$FISHING_TIME=sum(new_maree0$FISHING_TIME)
          i=j
        } else {
          new_maree=tmp[i,]
          i=i+1
        }
      } else {
        new_maree=tmp[i,]
        i=i+1
      }
      new_marees_navire=rbind(new_marees_navire,new_maree[,c("VESSEL_FK","GR_VESSEL_TYPE","ANNEE","MOIS","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","DEPARTURE_LOCATION_DIST","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","DEPARTURE_DATE_TIME","RETURN_DATE_TIME","DEPARTURE_QUALITY_NEW","RETURN_QUALITY_NEW","FISHING_TIME")])
    }
    #Gestion de la dernière marée
    derniere_maree=tmp[nrow(tmp),]
    if(!derniere_maree$RETURN_DATE_TIME%in%new_marees_navire$RETURN_DATE_TIME) new_marees_navire=rbind(new_marees_navire,derniere_maree[,c("VESSEL_FK","GR_VESSEL_TYPE","ANNEE","MOIS","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","DEPARTURE_LOCATION_DIST","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","DEPARTURE_DATE_TIME","RETURN_DATE_TIME","DEPARTURE_QUALITY_NEW","RETURN_QUALITY_NEW","FISHING_TIME")])
  }
  new_marees_VMS=rbind(new_marees_VMS,new_marees_navire)
}

### --- Conservation des marées finales

# Seulement marées OK <=> OK
marees_VMS_finales=new_marees_VMS[new_marees_VMS$ANNEE==annee & new_marees_VMS$DEPARTURE_QUALITY_NEW=="OK" & new_marees_VMS$RETURN_QUALITY_NEW=="OK",]
marees_VMS_finales$RETURN_LOCATION_LABEL=as.character(marees_VMS_finales$RETURN_LOCATION_LABEL)

#Pointeur d'effort de pêche : si temps de pêche supérieur à 0h
if (nrow(marees_VMS_finales) > 0) {
  marees_VMS_finales$MAREE_EFFORT=0
  marees_VMS_finales[!is.na(marees_VMS_finales$FISHING_TIME) & marees_VMS_finales$FISHING_TIME>0,]$MAREE_EFFORT=1
  marees_VMS_finales=marees_VMS_finales[order(marees_VMS_finales$VESSEL_FK,marees_VMS_finales$RETURN_DATE_TIME),]
  marees_VMS_finales$ID_MAREE_VMS=1:nrow(marees_VMS_finales)
  marees_VMS_finales=setNames(marees_VMS_finales[,c("VESSEL_FK","GR_VESSEL_TYPE","ID_MAREE_VMS","ANNEE","MOIS","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","DEPARTURE_DATE_TIME","RETURN_DATE_TIME","MAREE_EFFORT")],c("NAVIRE","TYPE_NAVIRE","ID_MAREE_VMS","ANNEE","MOIS","PORT_DEPART_COD","PORT_DEPART_LIB","PORT_RETOUR_COD","PORT_RETOUR_LIB","DATE_DEPART","DATE_RETOUR","MAREE_EFFORT"))
}

#Conservations des marées pour les navires du fichier flotte
marees_VMS_finales=marees_VMS_finales[marees_VMS_finales$NAVIRE%in%fichier_flotte$NAVIRE,]

write.table(marees_VMS_finales,"Données/MAREES_VMS_CONSOLIDEES.csv",sep=";",row.names=F)


