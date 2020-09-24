
# Clear Section -----------------------------------------------------------

rm(list = ls()) #limpia variable
graphics.off() #limpia graficos
cat("\014") #limpia ventana comandos


# Cargar Librerias --------------------------------------------------------

#install.packages("Libreria")

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


# Carga de Datos ----------------------------------------------------------

# Rates de Gran Bretaña, Alemania, Suiza, Estados Unidos.
# United Kingdom, Deutschland, Switzerland, United States.
# Abreviaturas Oficiales UK, DE, CH, US.

UKrates<-read.xlsx("Datos.xlsx",sheet = "UK Rates", rowNames = TRUE)
DErates<-read.xlsx("Datos.xlsx",sheet = "Germany Rates", rowNames = TRUE)
CHrates<-read.xlsx("Datos.xlsx",sheet = "Swiss Rates", rowNames = TRUE)
USrates<-read.xlsx("Datos.xlsx",sheet = "Swiss Rates", rowNames = TRUE)

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

# Ahora procedemos a calcular la estadistica descriptiva.




