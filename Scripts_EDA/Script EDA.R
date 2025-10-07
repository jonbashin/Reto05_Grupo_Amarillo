#===============
#LIBRERIAS
#===============
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyr)
library(naniar)


#==============
#CARGAR DATOS
#==============
exogenos <- read_excel("DATOS/originales/exogenas_paises_punto2.xlsx")
pib_ipc <- read.csv("DATOS/originales/pib_ipc_paises_punto2.csv")

#==========
#DATOS GENERALES
#===========
# =========================
# 2. DATOS GENERALES DE LOS DATAFRAMES
# =========================
#Datos generales
summary(pib_ipc)
summary(exogenos)


#Dimension de los dataframes originales
dim(pib_ipc)
dim(exogenos)

#Año de registro
max(pib_ipc$Year)
min(pib_ipc$Year)

max(exogenos$Year)
min(exogenos$Year)

#Total paises
unique(pib_ipc$Country)
unique(exogenos$Country)

# =========================
# 3. DATOS DE AUSTRALIA LIMPIOS
# =========================
df<- read.csv("DATOS/limpios/Datos_Limpios_Australia.csv")
dim(df)

#============

