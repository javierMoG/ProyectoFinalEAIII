library(readxl)
library(stats)
#Leemos la base de datos del Banco Mundial
datosWB <- read_excel("The Last Dance/Aplicada III/Proyecto/Data_Extract_From_World_Development_Indicators .xlsx")

head(datosWB)

#Nombramos las columnas
names(datosWB) <- c("AÑO", "PAIS", "CO2", "ER", "PIB", "GH", "EV", "AE", "GINI", "SS", 
                     "SA", "P", "DE", "EF", "MI", "DEP", "ESC", "HOM")
#Tomamos las columnas de interés
datosAux <- datosWB[,3:18]

#Matriz de correlación
cor(datosAux, use ="na.or.complete")

#Estadísticas de los datos
summary(datosAux)

#Imputación de datos con la media
datos1<-datosWB
#CO2
#Notamos que para el 2018, ningún país tiene registros de C02
for (i in 1:185){
  if(is.na(datos1[i,"CO2"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"CO2"]<-mean(datos1$CO2[datos1$PAIS==p], na.rm=T)
  }
}

#ER
#Solo hay un valor nulo
p<-as.character(datos1$PAIS[is.na(datos1$ER)])
datos1$ER[is.na(datos1$ER)]<-mean(datos1$ER[datos1$PAIS==p], na.rm=T)

#AE
for (i in 1:185){
  if(is.na(datos1[i,"AE"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"AE"]<-mean(datos1$AE[datos1$PAIS==p], na.rm=T)
  }
}

#GINI
for (i in 1:185){
  if(is.na(datos1[i,"GINI"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"GINI"]<-mean(datos1$GINI[datos1$PAIS==p], na.rm=T)
  }
}
#Notar que NZL no tiene calculos del índice de GINI para ningún año

#SS
for (i in 1:185){
  if(is.na(datos1[i,"SS"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"SS"]<-mean(datos1$SS[datos1$PAIS==p], na.rm=T)
  }
}

#SA
for (i in 1:185){
  if(is.na(datos1[i,"SA"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"SA"]<-mean(datos1$SA[datos1$PAIS==p], na.rm=T)
  }
}

#P
for (i in 1:185){
  if(is.na(datos1[i,"P"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"P"]<-mean(datos1$P[datos1$PAIS==p], na.rm=T)
  }
}
# Nueve países no tiene datos de la tasa de recuento de la pobreza en 
# las líneas nacionales de pobreza

#DE
#Solo tiene un valor nulo
p<-as.character(datos1$PAIS[is.na(datos1$DE)])
datos1$DE[is.na(datos1$DE)]<-mean(datos1$DE[datos1$PAIS==p], na.rm=T)

#ESC
for (i in 1:185){
  if(is.na(datos1[i,"ESC"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"ESC"]<-mean(datos1$ESC[datos1$PAIS==p], na.rm=T)
  }
}
#Islandia no tiene datos de la media de años de escolaridad

#HOM
for (i in 1:185){
  if(is.na(datos1[i,"HOM"])){
    p<-as.character(datos1[i,"PAIS"])
    datos1[i,"HOM"]<-mean(datos1$HOM[datos1$PAIS==p], na.rm=T)
  }
}

summary(datos1)

