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
library(zoo)


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
#==================
#Funcion
#================
# Tema personalizado
tema_economico <- function() {
  theme_minimal(base_size = 14) +
    theme(
      text = element_text(color = paleta_colores["gris_texto"], family = "sans"),
      plot.title = element_text(
        hjust = 0.5, 
        face = "bold",
        color = paleta_colores["gris_texto"],
        size = 16,
        margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        color = paleta_colores["gris_texto"],
        size = 12,
        margin = margin(b = 20)
      ),
      axis.title = element_text(color = paleta_colores["gris_texto"], face = "bold"),
      axis.text = element_text(color = paleta_colores["gris_texto"]),
      plot.background = element_rect(fill = paleta_colores["beige"], color = NA),
      panel.background = element_rect(fill = paleta_colores["beige"], color = NA),
      panel.grid.major = element_line(color = alpha(paleta_colores["gris_fondo"], 0.6)),
      panel.grid.minor = element_line(color = alpha(paleta_colores["gris_fondo"], 0.3)),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Convertir series a data.frames
st_gdp <- data.frame(
  Date = as.yearqtr(time(st_gdp)),
  GDP = as.numeric(st_gdp)
)

st_cpi <- data.frame(
  Date = as.yearqtr(time(st_cpi)),
  CPI = as.numeric(st_cpi)
)

st_unemployment_df <- data.frame(
  Date = as.yearqtr(time(st_unemployment)),
  Unemployment = as.numeric(st_unemployment)
)

st_money_supply_df <- data.frame(
  Date = as.yearqtr(time(st_money_supply)),
  Money_Supply = as.numeric(st_money_supply)
)

st_stock_market_df <- data.frame(
  Date = as.yearqtr(time(st_stock_market)),
  Stock_Market = as.numeric(st_stock_market)
)



#==================
#GRAFICAR IPC
#==================
# ASEGURAR QUE ESTE COMO AÑO
st_cpi$Date <- as.yearqtr(st_cpi$Date)

# Gráfico mejorado sin puntos y con etiquetas cada 2 años
ggplot(st_cpi, aes(x = Date, y = CPI)) +
  geom_line(color = paleta_colores["magenta"], size = 1.3) +  # solo línea
  scale_x_yearqtr(
    breaks = seq(from = min(st_cpi$Date), 
                 to = max(st_cpi$Date), 
                 by = 2),         # cada 2 años
    format = "%Y"                # mostrar solo el año
  ) +
  labs(
    title = "Evolución del IPC por trimestre en Australia",
    subtitle = "Índice de Precios al Consumo (CPI)",
    x = "Año",
    y = "Índice (CPI)"
  ) +
  tema_economico()





