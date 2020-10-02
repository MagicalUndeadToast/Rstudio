
#XXX# LEER PORFAVOR LA SECCION DE COMENTARIOS SOBRE EL PAPER, ES SUMAMENTE IMPORTANTE AL MOMENTO DE ANALIZAR NUESTROS AVANZES #XXX#

# Clear Section -----------------------------------------------------------

rm(list = ls()) #limpia variable
graphics.off() #limpia graficos
cat("\014") #limpia ventana comandos

setwd("~/GitHub/Rstudio")

# Cargar Librerias --------------------------------------------------------

# install.packages("Libreria")

require(rmgarch)
library(readr)
library(BatchGetSymbols)
library(xts)
library(moments)
library(tseries)
library(xlsx)
library(openxlsx) 
library(sandwich)
library(lmtest)
library(grDevices)

# Funciones.

source("FuncionesparaR.R")

# Carga de Datos ----------------------------------------------------------

# Rates de Gran Bretaña, Alemania, Suiza, Estados Unidos.
# United Kingdom, Deutschland, Switzerland, United States.
# Abreviaturas Oficiales UK, DE, CH, US.

UKrates<-read.xlsx("Datos.xlsx",sheet = "UK Rates", rowNames = TRUE)
DErates<-read.xlsx("Datos.xlsx",sheet = "Germany Rates", rowNames = TRUE)
CHrates<-read.xlsx("Datos.xlsx",sheet = "Swiss Rates", rowNames = TRUE)
USrates<-read.xlsx("Datos.xlsx",sheet = "USD Rates", rowNames = TRUE)

# Tipos de Cambio.
# GBP Great Britain Pound, CHF Confoederatio Helvetica Franc, DEM Deutsche Mark, USD United States Dollar.

GBPToUSD<-read.xlsx("Datos.xlsx",sheet = "Pound-USD", rowNames = TRUE)
DEMToUSD<-read.xlsx("Datos.xlsx",sheet = "Mark-USD", rowNames =  TRUE)
CHFToUSD<-read.xlsx("Datos.xlsx",sheet = "Swiss-USD", rowNames = TRUE)


# Procesos y Estadistica Descriptiva --------------------------------------

options(stringsAsFactors=FALSE)

# Rates.

row.names(UKrates)<-as.Date(as.double(row.names(UKrates)),origin = "1899-12-30")
row.names(DErates)<-as.Date(as.double(row.names(DErates)),origin = "1899-12-30")
row.names(CHrates)<-as.Date(as.double(row.names(CHrates)),origin = "1899-12-30")
row.names(USrates)<-as.Date(as.double(row.names(USrates)),origin = "1899-12-30")
  
# Tipos de Cambio.

row.names(GBPToUSD)<-as.Date(as.double(row.names(GBPToUSD)),origin = "1899-12-30")
row.names(DEMToUSD)<-as.Date(as.double(row.names(DEMToUSD)),origin = "1899-12-30")
row.names(CHFToUSD)<-as.Date(as.double(row.names(CHFToUSD)),origin = "1899-12-30")

# Los datos parecieran estar, almenos para el tipo de cambio, como se va a ver mas adelante, como 
# los reciprocos de los que utiliza Samuel en su Paper, por lo cual, procederemos a transformarlos.

# Calculamos los reciprocos de los exchange rates.

GBPToUSD<-GBPToUSD^-1
DEMToUSD<-DEMToUSD^-1
CHFToUSD<-CHFToUSD^-1

# Nos dimos cuenta que los Rates vienen en porcentaje.

UKrates<-UKrates/100
DErates<-DErates/100
CHrates<-CHrates/100
USrates<-USrates/100

# Ahora procedemos a calcular la estadistica descriptiva.

# En excel seria.

# Deberia estar compuesta de:
# Media
# Error Estandar
# Mediana
# Moda
# Volatilidad
# Varianza
# Curtosis
# Coeficiente de Asimetria
# Rango (Max-Min)
# Min
# Max
# Suma
# Cuenta
# Conf Lvl 95%

# En el paper seria de la siguiente manera.

# Mean
# S.D.
# Median
# Minimun
# Maximun

# Partimos para los recientemente calculados tipos de cambio.

# GBP o BP.

DEGBPToUSD<-matrix(0,1,5)
DEGBPToUSD[1,1]<-sapply(GBPToUSD, mean, na.rm = TRUE)
DEGBPToUSD[1,2]<-sapply(GBPToUSD, sd, na.rm = TRUE)
DEGBPToUSD[1,3]<-sapply(GBPToUSD, median, na.rm = TRUE)
DEGBPToUSD[1,4]<-sapply(GBPToUSD, min, na.rm = TRUE)
DEGBPToUSD[1,5]<-sapply(GBPToUSD, max, na.rm = TRUE)

row.names(DEGBPToUSD)<-"GBPToUSD"
colnames(DEGBPToUSD)<-c("Mean","S.D.","Median","Minimun","Maximun")

# DM o DEM.

DEDEMToUSD<-matrix(0,1,5)
DEDEMToUSD[1,1]<-sapply(DEMToUSD, mean, na.rm = TRUE)
DEDEMToUSD[1,2]<-sapply(DEMToUSD, sd, na.rm = TRUE)
DEDEMToUSD[1,3]<-sapply(DEMToUSD, median, na.rm = TRUE)
DEDEMToUSD[1,4]<-sapply(DEMToUSD, min, na.rm = TRUE)
DEDEMToUSD[1,5]<-sapply(DEMToUSD, max, na.rm = TRUE)

row.names(DEDEMToUSD)<-"DEMToUSD"
colnames(DEDEMToUSD)<-c("Mean","S.D.","Median","Minimun","Maximun")

# SF o CHF.

DECHFToUSD<-matrix(0,1,5)
DECHFToUSD[1,1]<-sapply(CHFToUSD, mean, na.rm = TRUE)
DECHFToUSD[1,2]<-sapply(CHFToUSD, sd, na.rm = TRUE)
DECHFToUSD[1,3]<-sapply(CHFToUSD, median, na.rm = TRUE)
DECHFToUSD[1,4]<-sapply(CHFToUSD, min, na.rm = TRUE)
DECHFToUSD[1,5]<-sapply(CHFToUSD, max, na.rm = TRUE)

row.names(DECHFToUSD)<-"CHFToUSD"
colnames(DECHFToUSD)<-c("Mean","S.D.","Median","Minimun","Maximun")

# Pasamos a calcular ahora los ratios, con las filas como los tenores 
# y las columnas como las estadisticas descriptivas.

# US Rates.

DEUSrates<-matrix(0,7,5)
DEUSrates[,1]<-t(sapply(USrates, mean, na.rm = TRUE))
DEUSrates[,2]<-t(sapply(USrates, sd, na.rm = TRUE))
DEUSrates[,3]<-t(sapply(USrates, median, na.rm = TRUE))
DEUSrates[,4]<-t(sapply(USrates, min, na.rm = TRUE))
DEUSrates[,5]<-t(sapply(USrates, max, na.rm = TRUE))

row.names(DEUSrates)<-c("US1M","US3M","US6M","US1Y","US2Y","US3Y","US5Y")
colnames(DEUSrates)<-c("Mean","S.D.","Median","Minimun","Maximun")

# UK Rates.

DEUKrates<-matrix(0,7,5)
DEUKrates[,1]<-t(sapply(UKrates, mean, na.rm = TRUE))
DEUKrates[,2]<-t(sapply(UKrates, sd, na.rm = TRUE))
DEUKrates[,3]<-t(sapply(UKrates, median, na.rm = TRUE))
DEUKrates[,4]<-t(sapply(UKrates, min, na.rm = TRUE))
DEUKrates[,5]<-t(sapply(UKrates, max, na.rm = TRUE))

row.names(DEUKrates)<-c("UK1M","UK3M","UK6M","UK1Y","UK2Y","UK3Y","UK5Y")
colnames(DEUKrates)<-c("Mean","S.D.","Median","Minimun","Maximun")

# DE Rates.

DEDErates<-matrix(0,7,5)
DEDErates[,1]<-t(sapply(DErates, mean, na.rm = TRUE))
DEDErates[,2]<-t(sapply(DErates, sd, na.rm = TRUE))
DEDErates[,3]<-t(sapply(DErates, median, na.rm = TRUE))
DEDErates[,4]<-t(sapply(DErates, min, na.rm = TRUE))
DEDErates[,5]<-t(sapply(DErates, max, na.rm = TRUE))

row.names(DEDErates)<-c("DE1M","DE3M","DE6M","DE1Y","DE2Y","DE3Y","DE5Y")
colnames(DEDErates)<-c("Mean","S.D.","Median","Minimun","Maximun")

# CH Rates.

DECHrates<-matrix(0,7,5)
DECHrates[,1]<-t(sapply(CHrates, mean, na.rm = TRUE))
DECHrates[,2]<-t(sapply(CHrates, sd, na.rm = TRUE))
DECHrates[,3]<-t(sapply(CHrates, median, na.rm = TRUE))
DECHrates[,4]<-t(sapply(CHrates, min, na.rm = TRUE))
DECHrates[,5]<-t(sapply(CHrates, max, na.rm = TRUE))

row.names(DECHrates)<-c("CH1M","CH3M","CH6M","CH1Y","CH2Y","CH3Y","CH5Y")
colnames(DECHrates)<-c("Mean","S.D.","Median","Minimun","Maximun")

# Ahora procedemos a simular la tabla del paper.

DescriptiveStatistics <- rbind(DEGBPToUSD,DEDEMToUSD,DECHFToUSD,
                               DEUSrates,DEUKrates,DEDErates,DECHrates)

# Replicacion del Paper ---------------------------------------------------

# S es la tasa de cambio, F la tasa forward.

ValueofAyL(CHFToUSD,USrates[1,1],dim(CHFToUSD)[1])

# Asumimos Bonos cupones ceros para los pasivos, con valor cara de
# 100.000 en el caso domestico y 200.000 en el caso internacional.

# Las tasas que vienen en nuestra BB.DD no son iguales a las que estan
# presentes en este paper. 

Balance<-c("1984-02-06","1984-05-16","1985-10-01","1986-02-06","1986-12-03","1987-02-25","1987-03-11")

# Duraciones de Macaulay.
Days<-c(100, 503, 128, 300, 84, 14,0)
Years<-Days/365
Duration<-matrix(0,1,7)
aux=sum(Years)
for (i in 1:6) {
  Duration[i]<-aux
  aux=aux-Years[i]
}

# Extraccion datos USD.
DrUS<-matrix(0,1,7)
for (i in 1:7) {
  DrUS[1,i]<-SearchValue(USrates,Balance[i],"3Y")
}
USLia<-LiaValue(100000,Duration,DrUS)
USLiaConv<-LiaConv(Duration,DrUS) #Convexidad




# Replicacion tabla Suiza.
ExCHFUSD<-matrix(0,1,7)
FrCH<-matrix(0,1,7)
ActivosDomesticosFr<-matrix(71610.52,1,7)
for (i in 1:7) {
  ExCHFUSD[1,i]<-SearchValue(CHFToUSD,Balance[i],"Exchange")
  FrCH[1,i]<-SearchValue(CHrates,Balance[i],"3Y")
}

CHLia<-LiaValue(200000,Duration,FrCH)



TotalLiaCH<-matrix(0,1,7) 
for (i in 1:7) {
    TotalLiaCH[i]<-CHLia[i]*ExCHFUSD[1,i]+USLia[i]
}
ConveDom<-LiaConv(Duration,DrUS) #Convexidad
ConveFor<-LiaConv(Duration,FrCH) #Convexidad

#Tabla suiza datos iniciales
SUIZA1<-rbind(ExCHFUSD, DrUS, FrCH)
colnames(SUIZA1)<-Balance
rownames(SUIZA1)<-c("US$/SF","r_d", "r_f")
SUIZA1

#Tabla Suiza Valores Presentes
SUIZA2<-rbind(USLia,USLia,CHLia,CHLia,TotalLiaCH,TotalLiaCH)
colnames(SUIZA2)<-Balance
rownames(SUIZA2)<-c("US Assets (US$)","US Liabilities (US$)", "Swiss Assets (SF)","Swiss Liabilities (SF)", "Total Assets (US$)", "Total Liabilities (US$)")
SUIZA2

#Tabla Suiza Duracion
SUIZA3<-rbind(Duration,Duration,Duration,Duration,Duration,Duration)
colnames(SUIZA3)<-Balance
rownames(SUIZA3)<-c("US Assets","US Liabilities", "Swiss Assets", "Swiss Liabilities", "Total Assets", "Total Liabilities")
SUIZA3

#Tabla Suiza Convexidad
SUIZA4<-rbind(ConveDom,ConveDom,ConveFor,ConveFor)
colnames(SUIZA4)<-Balance
rownames(SUIZA4)<-c("US Assets","US Liabilities", "Swiss Assets", "Swiss Liabilities")
SUIZA4

#Tabla Suiza Pagos
SUIZA5<-c(0)

# Replicacion tabla Inglesa.
ExGBPUSD<-matrix(0,1,7)
FrUK<-matrix(0,1,7)
for (i in 1:7) {
  ExGBPUSD[1,i]<-SearchValue(GBPToUSD,Balance[i],"Exchange")
  FrUK[1,i]<-SearchValue(UKrates,Balance[i],"3Y")
}
UKLia<-LiaValue(200000,Duration,FrUK)
DomesticAssetUK<-UKLia 

TotalLiaUK<-matrix(0,1,7) 
for (i in 1:7) {
  TotalLiaUK[i]<-UKLia[i]*ExGBPUSD[1,i]+USLia[i]
}
ConveFor1<-LiaConv(Duration,FrUK) #Convexidad
ConveDom1<-LiaConv(Duration,DrUS)

#Tabla inglesa datos iniciales
UK1<-rbind(ExGBPUSD, DrUS, FrUK)
colnames(UK1)<-Balance
rownames(UK1)<-c("US$/BP","r_d", "r_f")
UK1

#Tabla inglesa Valores Presentes
UK2<-rbind(USLia,USLia,UKLia,UKLia,TotalLiaUK,TotalLiaUK)
colnames(UK2)<-Balance
rownames(UK2)<-c("US Assets (US$)","US Liabilities (US$)", "British Assets (BP)","British Liabilities (BP)", "Total Assets (US$)", "Total Liabilities (US$)")
UK2

#Tabla inglesa Duracion
UK3<-rbind(Duration,Duration,Duration,Duration,Duration,Duration)
colnames(UK3)<-Balance
rownames(UK3)<-c("US Assets","US Liabilities", "British Assets", "British Liabilities", "Total Assets", "Total Liabilities")
UK3

#Tabla inglesa Convexidad
UK4<-rbind(ConveDom1,ConveDom1,ConveFor1,ConveFor1)
colnames(UK4)<-Balance
rownames(UK4)<-c("US Assets","US Liabilities", "British Assets", "British Liabilities")
UK4

#Tabla inglesa Pagos
UK5<-c(0)

# Replicacion tabla alemania.

ExDEMUSD<-matrix(0,1,7)
FrDE<-matrix(0,1,7)
for (i in 1:7) {
  ExDEMUSD[1,i]<-SearchValue(DEMToUSD,Balance[i],"Exchange")
  FrDE[1,i]<-SearchValue(DErates,Balance[i],"3Y")
}
DELia<-LiaValue(200000,Duration,FrDE)
DomesticAssetDE<-DELia

TotalLiaDE<-matrix(0,1,7) 
for (i in 1:7) {
  TotalLiaDE[i]<-DELia[i]*ExDEMUSD[1,i]+USLia[i]
}
ConveDom2<-LiaConv(Duration,DrUS) #Convexidad
ConveFor2<-LiaConv(Duration,FrDE)

#Tabla alemana datos iniciales
ALEMANIA1<-rbind(ExDEMUSD, DrUS, FrDE)
colnames(ALEMANIA1)<-Balance
rownames(ALEMANIA1)<-c("US$/DM","r_d", "r_f")
ALEMANIA1

#Tabla alemana Valores Presentes
ALEMANIA2<-rbind(USLia,USLia,DELia,DELia,TotalLiaDE,TotalLiaDE)
colnames(ALEMANIA2)<-Balance
rownames(ALEMANIA2)<-c("US Assets (US$)","US Liabilities (US$)", "German Assets (Dm)","German Liabilities (DM)", "Total Assets (US$)", "Total Liabilities (US$)")
ALEMANIA2

#Tabla alemana Duracion
ALEMANIA3<-rbind(Duration,Duration,Duration,Duration,Duration,Duration)
colnames(ALEMANIA3)<-Balance
rownames(ALEMANIA3)<-c("US Assets","US Liabilities", "German Assets", "German Liabilities", "Total Assets", "Total Liabilities")
ALEMANIA3

#Tabla alemana Convexidad
ALEMANIA4<-rbind(ConveDom2,ConveDom2,ConveFor2,ConveFor2)
colnames(ALEMANIA4)<-Balance
rownames(ALEMANIA4)<-c("US Assets","US Liabilities", "German Assets", "German Liabilities")
ALEMANIA4

#Tabla alemana Pagos
ALEMANIA5<-c(0)

# COMENTAR SOBRE LA DIFERENCIA DE LA CONVEXIDAD.







# Se verifica que NO se cumple la condicion de segundo orden

x <- seq(0,6.8734/1.221,0.1)
plot(x, 6.8734-1.221*x,main="Funcion Duraciones",
     ylab="D_af",
     type="l",
     col="blue",
     xlim=c(0.21,6.8734/1.221),
     ylim=c(0.26,6.8734))

z<-6.8734-1.221*x

auxz<-matrix(FrCH[1],1,length(z))
ConveAF<-LiaConv(z,auxz)

ConveLF<-LiaConv(Duration[1],FrCH[1])

auxx<-matrix(DrUS[1],1,length(x))
ConveAD<-LiaConv(x,auxx)

ConveLD<-LiaConv(Duration[1],DrUS[1])


for (i in 1:length(x)){
  if(ConveAF[i]>ConveLF && ConveAD[i]>ConveLD){
    print("Se cumple la condicion de segundo orden")
  }else{
    print("No se cumple la condicion de segundo orden")
  }
}


# Asumiremos Duracion activos y pasivos domesticas iguales. --> Lo más cercano
# a que se cumpla la condicion de segundo orden, donde se cumpliria si fuera
# en vez de ser estrictamente mayor, solo fuera mayor o igual 

ConveAF<-LiaConv(Duration[1],FrCH[1])
ConveLF<-LiaConv(Duration[1],FrCH[1])
ConveAD<-LiaConv(Duration[1],DrUS[1])
ConveLD<-LiaConv(Duration[1],DrUS[1])

if(ConveAF>ConveLF && ConveAD>ConveLD){
  print("Se cumple la condicion de segundo orden")
}else{
  print("No se cumple la condicion de segundo orden")
}


# Comentarios Sobre el Codigo ---------------------------------------------

# En esta seccion se plantean los problemas vistos en la replicacion del paper, especialmente en aquellos en donde vemos
# que no se cumplen las condiciones propuestas por este mismo. Tambien se explicara el porque fue necesario aplicar
# ingenieria inversa, asi como los problemas al momento de calzar los datos. Finalmente se plantea nuestra postura en el porque
# es imposible replicar este paper con la informacion entregada y datos mencionados al menos de manera determinista, por lo que
# nosotros pudimos captar a contraparte de que haya algun dato obviado por nosotros o por el paper.

#XXX# Sobre la disponibilidad y fuente de los datos #XXX#

# Uno de los primeros problemas con el que nos encontramos fue al momento de acceder a los datos y la fuente que hace
# referencia el paper, siendo este el Banco Central de Israel. A pesar de la dificultad para acceder a los datos, al momento
# de descargar los datos disponibles del banco central de israel, nos dimos cuenta que solo estaban disponibles los de los tipos
# de cambio entre la moneda israeli y el resto de divisas, lo cual no supuso un mayor problema dado que aun asi logramos obtener
# de forma implicita el valor de los tipos de cambio entre las divisas del paper. Sobre las tasas, no se encontro rastro de estas
# en los datos disponibles por el banco, por lo que fue necesario usar los datos proporcionados por el profesor. Por otra parte,
# los datos proporcionados por el banco tenian muchas fechas faltantes, incluso las usadas por el paper, por lo cual se decidio
# utilizar en su totalidad los datos proporcionados por el profesor.

#XXX# Sobre la estadistica descriptiva y los datos #XXX#

# Otro de los grandes problemas encontrados en el paper, es que la estadistica descriptiva como se muestra en el paper no coincide
# con la estadistica descriptiva DescriptiveStatistics obtenida por nosotros, nisiquiera al momento de usar los datos del Banco
# Central de Israel. Este punto, sin embargo, se deja a la duda del paper, dado que puede ser un problema simplemente de capacidad
# computacional de la epoca, en donde puedan haberse ahorrado calculos decimales para reducir el uso de memoria. Por otra parte,
# sin embargo, hay datos que divergen mucho respecto a los presentados por el paper, como lo son especialmente para el caso de
# Suiza. Estos cambios se pueden ver en las diferencias dadas por el paper y nuestras tablas de datos.

#XXX# Sobre las carteras de activos y pasivos utilizados #XXX#
