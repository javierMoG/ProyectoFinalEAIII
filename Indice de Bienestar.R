library(readxl)
library(stats)
#Leemos la base de datos del Banco Mundial
datosWB <- read_excel("The Last Dance/Aplicada III/Proyecto/Data_Extract_From_World_Development_Indicators .xlsx")

head(datosWB)
#Tomamos las columnas de inter?s
datosAux <- datosWB[,5:20]
#Nombramos las columnas
names(datosAux) <- c("CO2", "ER", "PIB", "GH", "EV", "AE", "GINI", "SS", 
                     "SA", "P", "DE", "EF", "MI", "DEP", "ESC", "DES", "HOM")
#Matriz de correlaci?n
cor(datosAux, use ="na.or.complete")