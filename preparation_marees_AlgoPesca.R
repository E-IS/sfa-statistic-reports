# reconstruction des marées AlgoPesca

marees_AP=read.csv("Données/SIH-GEOLOC-mar.txt",sep="\t",col.names = c("ALGORITHM_VERSION","PROGRAM_FK","VESSEL_FK","DEPARTURE_LOCATION_LEVEL_FK","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","RETURN_LOCATION_LEVEL_FK","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","DEPARTURE_DATE_TIME","DEPARTURE_QUALITY","RETURN_DATE_TIME","RETURN_QUALITY","DT_AVG_POS","NB_INTERRUPTIONS","NB_STATIC_POSITIONS","NB_ERROR_POSITIONS","INITIAL_ROUTE_TIME_DURATION","FINAL_ROUTE_TIME_DURATION"))
marees_AP$VESSEL_FK=as.character(marees_AP$VESSEL_FK)
marees_AP$DEPARTURE_DATE_TIME=strptime(as.character(marees_AP$DEPARTURE_DATE_TIME),"%d/%m/%Y %H:%M")
marees_AP$RETURN_DATE_TIME=strptime(as.character(marees_AP$RETURN_DATE_TIME),"%d/%m/%Y %H:%M")
marees_AP$SEA_TIME=0

sequences_AP=read.csv("Données/SIH-GEOLOC-seq.txt",sep="\t",dec = ".",col.names = c("PROGRAM_FK","VESSEL_FK","SECT_LOCATION_LEVEL_FK","SECT_LABEL","REG_LOCATION_COUNTRY_LABEL","REG_LOCATION_LEVEL_FK","REG_LOCATION_LABEL","START_DATE_TIME","END_DATE_TIME","FISHING_TIME_VESSEL","ROUTE_TIME_VESSEL","UNKNOWN_TIME_VESSEL"))
sequences_AP=sequences_AP[,c("VESSEL_FK","START_DATE_TIME","END_DATE_TIME","FISHING_TIME_VESSEL")]
sequences_AP$VESSEL_FK=as.character(sequences_AP$VESSEL_FK)
sequences_AP$START_DATE_TIME=strptime(as.character(sequences_AP$START_DATE_TIME),"%d/%m/%Y %H:%M")
sequences_AP$END_DATE_TIME=strptime(as.character(sequences_AP$END_DATE_TIME),"%d/%m/%Y %H:%M")
#str(sequences_AP)

positions_AP=read.csv("Données/SIH-GEOLOC-positions.txt",sep="\t",col.names = c("ID","VESSEL_FK","INT_REGISTRATION_CODE","POSITION_DATE","LONGITUDE","LATITUDE","DIRECTION","DEPTH","INST_SPEED","AVG_SPEED","LOCATION_LABEL","LOCATION_NAME","LOCATION_DIST","IS_FISHING"))
positions_AP=positions_AP[,c("VESSEL_FK","POSITION_DATE","LOCATION_LABEL","LOCATION_NAME","LOCATION_DIST")]
positions_AP$VESSEL_FK=as.character(positions_AP$VESSEL_FK)
positions_AP$POSITION_DATE=strptime(as.character(positions_AP$POSITION_DATE),"%d/%m/%Y %H:%M")

marees_AP_finales=c()
navires_AP=unique(marees_AP$VESSEL_FK)
for (n in 1:length(navires_AP)) {
  navire=navires_AP[n]
  #print(paste0("traitement du navire : ",navire," (",n,"/",length(navires_AP),")"))
  marees_AP_navire=marees_AP[marees_AP$VESSEL_FK==navire,]  
  sequences_AP_navire=sequences_AP[sequences_AP$VESSEL_FK==navire,]
  positions_AP_navire=positions_AP[positions_AP$VESSEL_FK==navire,]  
  for (i in 1:nrow(marees_AP_navire)) {
    maree_AP=marees_AP_navire[i,]
    #print(paste0("traitement de la marée ",i,"/",nrow(marees_AP_navire)))
    maree_AP$DEPARTURE_LOCATION_DIST=positions_AP_navire[positions_AP_navire$POSITION_DATE==maree_AP$DEPARTURE_DATE_TIME,]$LOCATION_DIST[1]
    maree_AP$RETURN_LOCATION_DIST=positions_AP_navire[positions_AP_navire$POSITION_DATE==maree_AP$RETURN_DATE_TIME,]$LOCATION_DIST[1]
    maree_AP$FISHING_TIME=sum(sequences_AP_navire[sequences_AP_navire$START_DATE_TIME>=maree_AP$DEPARTURE_DATE_TIME&sequences_AP_navire$END_DATE_TIME<=maree_AP$RETURN_DATE_TIME,]$FISHING_TIME_VESSEL)
    marees_AP_finales=rbind(marees_AP_finales,maree_AP)
  }
}

marees_AP_finales=marees_AP_finales[,c("VESSEL_FK","DEPARTURE_LOCATION_LABEL","DEPARTURE_LOCATION_NAME","DEPARTURE_LOCATION_COUNTRY","DEPARTURE_LOCATION_DIST","RETURN_LOCATION_LABEL","RETURN_LOCATION_NAME","RETURN_LOCATION_COUNTRY","RETURN_LOCATION_DIST","DEPARTURE_DATE_TIME","DEPARTURE_QUALITY","RETURN_DATE_TIME","RETURN_QUALITY","DT_AVG_POS","NB_INTERRUPTIONS","NB_STATIC_POSITIONS","NB_ERROR_POSITIONS","SEA_TIME","FISHING_TIME")]
write.table(marees_AP_finales,file="Données/MAREES_VMS_SORTIE_ALGOPESCA.csv",dec=".",sep=";")

