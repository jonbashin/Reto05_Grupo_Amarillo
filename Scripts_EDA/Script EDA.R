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
st_cpi<- readRDS("Series_Temporales/Trimestrales/cpi_ts_trimestral.rds")
st_gdp= readRDS("Series_Temporales/Trimestrales/gdp_ts_trimestral.rds")
st_money_supply=readRDS("Series_Temporales/Trimestrales/money_supply_ts_trimestral.rds")
st_stock_market=readRDS("Series_Temporales/Trimestrales/stock_market_ts_trimestral.rds")
st_unemployment=readRDS("Series_Temporales/Trimestrales/unemployment_ts_trimestral.rds")
#Pasar a datos trimestrales
df_datos_buscado<- data.frame(Año= 1996:2022,
                              Tipo_de_cambio_AUD=,
                              Tipo_de_cambio_SD=,
                              Tipo_de_Interes=,
                              Consumo=,
                              Invesion_Bruta_privada=,
                              Exportaciones=,
                              Importaciones=,
                              PIB_per_capital=,
                              Deflactor=,
                              Desempleo_por_sector=,
                              Pobalcion_activa_AUS=,
                              Poblacion_total=)



#============
#Paletas de colores
#============
paleta_colores <- c(
  verde = "#6DBC00",
  magenta = "#E20074", 
  berenjena = "#7A1E5A",
  beige = "#F4EDE4",
  gris_texto = "#333333",
  gris_fondo = "#E0E0E0"
)


#
library(ggplot2)
library(zoo)

# Convertir la serie a data.frame
st_gdp_df <- data.frame(
  Date = as.yearqtr(time(st_gdp)),
  CPI = as.numeric(st_gdp)
)

# Graficar con ggplot2
ggplot(st_gdp_df, aes(x = Date, y = CPI)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  labs(
    title = "Evolución del cpi trimestral",
    x = "Año",
    y = "cpi"
  ) +
  theme_minimal(base_size = 14)

