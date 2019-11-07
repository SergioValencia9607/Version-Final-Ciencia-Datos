#Limpieza de base de datos de accidentalidad en el municipio de Medellín.

# Carga de las librerias sf y dplyr.
library(sf)
library(dplyr)

# Convertir las coordenadas de la BD al sistema de latitud y longitud conforme la información .sf que se descarga.
for (year in 2014:2019){
  Archivo <- gsub(" ", "", paste("Accidentalidad/",year,"/Accidentalidad_georreferenciada_",year))
  Datos_sin_Coordenadas <- st_read(gsub(" ","",paste(Archivo,".shp")), stringsAsFactors=FALSE)
  Datos_Coordenadas <- st_transform(Datos_sin_Coordenadas, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  if (year == 2014) {
    Accidentalidad_Total <- Datos_Coordenadas
  } else {
    Accidentalidad_Total <- rbind(Accidentalidad_Total,Datos_Coordenadas)
  }
}

# Eliminar los datos que no tienen barrios ni comunas asociados.
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$TIPO_GEOCO != "ZONA RURAL",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "0",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "AU",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "In",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "NA",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$COMUNA != "SN",]
Accidentalidad_Total <- Accidentalidad_Total[!is.na(Accidentalidad_Total$COMUNA),]

Accidentalidad_Total <- Accidentalidad_Total[!is.na(Accidentalidad_Total$BARRIO),]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "0",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "Inst",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "Sin nombre",]
Accidentalidad_Total <- Accidentalidad_Total[Accidentalidad_Total$BARRIO != "AUC1",]

# Eliminar las columnas que no se necesitan para los analisis.
Accidentalidad_Total <- select(Accidentalidad_Total, -OBJECTID, -X, -Y, -RADICADO, -DIRECCION_, -CBML, -TIPO_GEOCO, -DISENO, -MES_NOMBRE)

# Separación de la información de latitud y longitud, y agregar a la BD.
coordenadas <- Accidentalidad_Total$geometry
coordenadas <-as.character(coordenadas)
coordenadas <-gsub("c(","",coordenadas,fixed=TRUE)
coordenadas <-gsub(")","",coordenadas,fixed=TRUE)
coordenadas <-strsplit(coordenadas,split=",",fixed = TRUE)
coordenadas <-unlist(coordenadas)
coordenadas <-as.numeric(coordenadas)
longitud <-coordenadas[seq(1,length(coordenadas),by=2)]
latitud <-coordenadas[seq(2,length(coordenadas),by=2)]
Accidentalidad_Total$Longitud <- longitud
Accidentalidad_Total$Latitud <- latitud
Accidentalidad_Total$geometry <- NULL

# Separación de fecha del accidente
Accidentalidad_Total$f_accidente <- substr(Accidentalidad_Total$FECHA,1,10)
Accidentalidad_Total$FECHA <- NULL

# Indicar cuales días son festivos
festivos<-read.csv("festivos.csv",header = TRUE,sep = ";")
names(festivos) <- c("FechaFestivo")
Accidentalidad_Total$Festivo <- ifelse(Accidentalidad_Total$f_accidente %in% festivos$FechaFestivo,"Festivo","No Festivo")

# Establecer los valores unicos de barrio y clase de accidente
Accidentalidad_Total$BARRIO <- gsub(". 1",".1",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub(". 2",".2",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("B. Cerro  El Volador","B. Cerro El Volador",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Barrios de Jesús","Barrio de Jesús",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Berlín","Berlin",Accidentalidad_Total$BARRIO,fixed=TRUE)
Accidentalidad_Total$BARRIO <- gsub("Villa Lilliam","Villa Liliam",Accidentalidad_Total$BARRIO,fixed=TRUE)

Accidentalidad_Total$COMUNA <- gsub("Laureles Estadio","Laureles",Accidentalidad_Total$COMUNA,fixed=TRUE)
Accidentalidad_Total$COMUNA <- gsub("Laureles","Laureles Estadio",Accidentalidad_Total$COMUNA,fixed=TRUE)

Accidentalidad_Total$CLASE <- gsub("Caída de Ocupante","Caída Ocupante",Accidentalidad_Total$CLASE,fixed=TRUE)
Accidentalidad_Total$CLASE <- gsub("Caida Ocupante","Caída Ocupante",Accidentalidad_Total$CLASE,fixed=TRUE)


# Corregir los valores erroneos de barrios y comunas, intercambiando el nombre del barrio por columna y viceversa
Comunas_Med <- c("Popular", "Santa Cruz", "Manrique", "Aranjuez", "Castilla", "Doce de Octubre", "Robledo", "Villa Hermosa", "Buenos Aires", "La Candelaria", "Laureles Estadio", "La América", "San Javier", "El Poblado", "Guayabal", "Belén","Corregimiento de San Antonio de Prado","Corregimiento de Santa Elena","Corregimiento de Altavista","Corregimiento de San Cristóbal","Corregimiento de San Sebastián de Palmitas")
Accidentalidad_Total$Comuna2 <- ifelse(Accidentalidad_Total$COMUNA %in% Comunas_Med, "Se encuentra","No se encuentra")
for (fila in 1:length(Accidentalidad_Total$Comuna2)){
  if (Accidentalidad_Total[fila,"Comuna2"] == "No se encuentra"){
    Barrio_Cambio <- Accidentalidad_Total[fila,"COMUNA"]
    Comuna_Cambio <- Accidentalidad_Total[fila,"BARRIO"]
    
    Accidentalidad_Total[fila,"BARRIO"] <- Barrio_Cambio
    Accidentalidad_Total[fila,"COMUNA"] <- Comuna_Cambio
  }
}
Accidentalidad_Total$Comuna2 <- NULL

Accidentalidad <- Accidentalidad_Total

# Cambiar el formato de la hora AM/PM a formato militar para mejorar el manejo de la informacion
Accidentalidad$HORA <- gsub(" ","",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub(".","",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub("am","AM",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub("pm","PM",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$Indicacion <- substr(Accidentalidad$HORA,nchar(as.character(Accidentalidad$HORA)) - 1,nchar(as.character(Accidentalidad$HORA)))
Accidentalidad$HORA <- substr(Accidentalidad$HORA, 1,nchar(as.character(Accidentalidad$HORA))-2)
Accidentalidad$HORA <- ifelse(nchar(as.character(Accidentalidad$HORA))==7,substr(Accidentalidad$HORA, 1,4),Accidentalidad$HORA)
Accidentalidad$HORA <- ifelse(nchar(as.character(Accidentalidad$HORA))==8,substr(Accidentalidad$HORA, 1,5),Accidentalidad$HORA)
Accidentalidad$HORA2 <- substr(Accidentalidad$HORA, 1, nchar(as.character(Accidentalidad$HORA))-3)
Accidentalidad$HORA2 <- ifelse((Accidentalidad$Indicacion == "PM" & as.numeric(Accidentalidad$HORA2) != 12),as.numeric(Accidentalidad$HORA2) + 12,Accidentalidad$HORA2)
Accidentalidad$HORA2 <- ifelse((Accidentalidad$Indicacion == "AM" & as.numeric(Accidentalidad$HORA2) == 12),as.numeric(Accidentalidad$HORA2) - 12,Accidentalidad$HORA2)

# Eliminar de las variables string la informacion que no es encesaria para el analisis y para mejorar el manerjo
Accidentalidad$COMUNA <- gsub("Corregimiento de ","",Accidentalidad$COMUNA,fixed=TRUE)
Accidentalidad$COMUNA <- gsub("de ","",Accidentalidad$COMUNA,fixed=TRUE)

# Guardar la informacion en una base de datos limpia para la utilizacion de la aplicacion
write.csv(Accidentalidad,"Accidentalidad_Total_3.csv")
