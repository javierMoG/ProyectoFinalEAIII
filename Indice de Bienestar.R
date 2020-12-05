library(readxl)
library(stats)
#Leemos la base de datos del Banco Mundial
datosWB <- read_excel("The Last Dance/Aplicada III/Proyecto/Data_Extract_From_World_Development_Indicators .xlsx")

head(datosWB)

#Nombramos las columnas
names(datosWB) <- c("AÑO", "PAIS", "CO2", "ER", "PIB", "GH", "EV", "AE", "GINI", "SS", 
                     "SA", "P", "DE", "EF", "MI", "DEP", "ESC", "HOM")

#Análisis exploratorio
summary(datosWB)

#Tomamos las columnas de interés
datosAux <- datosWB[,3:18]

paises<-datosWB$PAIS[1:37]
#PIB a lo largo de los años

#Gráfica 2002
a1<-datosWB$PIB[datosWB$AÑO==2002]*(1/1000)
p1<-data.frame(paises,a1)
p1<-p1[order(-p1[,2]),]
max1<-max(a1)
plot(p1$a1, main="PIB per cápita año 2002", xlab="Posición con respecto al PIB per cápita",ylab="Miles de dólares", ylim=c(0,max1+1), pch=18, col="red")
text(p1$a1, labels=p1$paises,cex=0.5, pos=3, col="blue")

summary(a1)
var(a1)

#Gráfica 2006
a2<-datosWB$PIB[datosWB$AÑO==2006]*(1/1000)
p2<-data.frame(paises,a2)
p2<-p2[order(-p2[,2]),]
max2<-max(a2)
plot(p2$a2, main="PIB per cápita año 2006", xlab="Posición con respecto al PIB per cápita",ylab="Miles de dólares", ylim=c(0,max2+1), pch=18, col="red")
text(p2$a2, labels=p2$paises,cex=0.5, pos=3, col="blue")
summary(a2)
var(a2)

#Gráfica 2010
a3<-datosWB$PIB[datosWB$AÑO==2010]*(1/1000)
p3<-data.frame(paises,a3)
p3<-p3[order(-p3[,2]),]
max3<-max(a3)
plot(p3$a3, main="PIB per cápita año 2010", xlab="Posición con respecto al PIB per cápita",ylab="Miles de dólares", ylim=c(0,max3+1), pch=18, col="red")
text(p3$a3, labels=p3$paises,cex=0.5, pos=3, col="blue")
summary(a3)
var(a3)

#Gráfica 2014
a4<-datosWB$PIB[datosWB$AÑO==2014]*(1/1000)
p4<-data.frame(paises,a4)
p4<-p4[order(-p4[,2]),]
max4<-max(a4)
plot(p4$a4, main="PIB per cápita año 2014", xlab="Posición con respecto al PIB per cápita",ylab="Miles de dólares", ylim=c(0,max4+1), pch=18, col="red")
text(p4$a4, labels=p4$paises,cex=0.5, pos=3, col="blue")
summary(a4)
var(a4)

#Gráfica 2018
a5<-datosWB$PIB[datosWB$AÑO==2018]*(1/1000)
p5<-data.frame(paises,a5)
p5<-p5[order(-p5[,2]),]
max5<-max(a5)
plot(p5$a5, main="PIB per cápita año 2018", xlab="Posición con respecto al PIB per cápita",ylab="Miles de dólares", ylim=c(0,max5+1), pch=18, col="red")
text(p5$a5, labels=p5$paises,cex=0.5, pos=3, col="blue")
summary(a5)
var(a5)

#Matriz de correlación
cor(datosAux, method = "pearson", use="na.or.complete")

# Gráfica de correlación entre variables
library(corrplot)

corrplot(cor(datosAux, method = "pearson", use="na.or.complete"), type = "upper", method = "ellipse")

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

