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

gg_cpi <- ggplot(st_cpi, aes(x = Date, y = CPI)) +
  geom_line(color = paleta_colores["magenta"], size = 1.3) +  # línea principal
  # añadir el punto del pico que cae
  geom_point(data = subset(st_cpi, Date == "2020 Q4"), 
             aes(x = Date, y = CPI), 
             color = "#333333", size = 3) +
  geom_text(
    data = subset(st_cpi, Date == "2020 Q4"),
    aes(label = "2020 Q4"),
    vjust = 2, color = "#333333", size = 3.5, fontface = "bold"
  ) +
  scale_x_yearqtr(
    breaks = seq(from = min(st_cpi$Date), 
                 to = max(st_cpi$Date), 
                 by = 2),   # cada 2 años
    format = "%Y"
  ) +
  labs(
    title = "Evolución del IPC por trimestre en Australia",
    subtitle = "Índice de Precios al Consumo (CPI)",
    x = "Año",
    y = "Índice (CPI)"
  ) +
  tema_economico()

gg_cpi

# =========================
#PIB (GDP) - Evolución trimestral
# =========================
st_gdp$Date <- as.yearqtr(st_gdp$Date)

gg_gdp <- ggplot(st_gdp, aes(x = Date, y = GDP)) +
  geom_line(color = paleta_colores["verde"], size = 1.3) +
  scale_x_yearqtr(
    breaks = seq(from = min(st_gdp$Date), to = max(st_gdp$Date), by = 2),
    format = "%Y"
  ) +
  labs(
    title = "Evolución del PIB trimestral en Australia",
    subtitle = "Producto Interno Bruto (GDP)",
    x = "Año", y = "PIB (índice o millones AUD)"
  ) +
  tema_economico()

ggplotly(gg_gdp)


# =========================
# Masa monetaria (Money Supply)
# =========================
st_money_supply_df$Date <- as.yearqtr(st_money_supply_df$Date)

gg_money <- ggplot(st_money_supply_df, aes(x = Date, y = Money_Supply)) +
  geom_line(color = paleta_colores["verde"], size = 1.3) +
  scale_x_yearqtr(
    breaks = seq(from = min(st_money_supply_df$Date), to = max(st_money_supply_df$Date), by = 2),
    format = "%Y"
  ) +
  labs(
    title = "Serie temporal de la masa monetaria (Money Supply) en Australia",
    x = "Año", y = "Masa monetaria (índice)"
  ) +
  tema_economico()

ggplotly(gg_money)

# =========================
#Índice bursátil (Stock Market)
# =========================
st_stock_market_df$Date <- as.yearqtr(st_stock_market_df$Date)

gg_stock <- ggplot(st_stock_market_df, aes(x = Date, y = Stock_Market)) +
  geom_line(color = paleta_colores["berenjena"], size = 1.3) +
  scale_x_yearqtr(
    breaks = seq(from = min(st_stock_market_df$Date), to = max(st_stock_market_df$Date), by = 2),
    format = "%Y"
  ) +
  labs(
    title = "Serie temporal del índice bursátil en Australia",
    x = "Año", y = "Índice bursátil"
  ) +
  tema_economico()

ggplotly(gg_stock)


# =========================
#Serie temporal del paro (Unemployment)
# =========================
# Puedes reutilizar el mismo de arriba o hacerlo distinto
gg_unemp_series <- ggplot(st_unemployment_df, aes(x = Date, y = Unemployment)) +
  geom_line(color = paleta_colores["magenta"], size = 1.3) +
  scale_x_yearqtr(
    breaks = seq(from = min(st_unemployment_df$Date), to = max(st_unemployment_df$Date), by = 2),
    format = "%Y"
  ) +
  labs(
    title = "Serie temporal del paro en Australia",
    x = "Año", y = "Porcentaje de paro"
  ) +
  tema_economico()

ggplotly(gg_unemp_series)

#=============
#PIB VS DESEMPLEO
#===========
# Unir PIB y Desempleo por fecha
df_okun <- merge(st_gdp, st_unemployment_df, by = "Date")

# Escalar (para comparar en la misma escala)
df_okun$GDP_scaled <- scale(df_okun$GDP)
df_okun$Unemployment_scaled <- scale(df_okun$Unemployment)

# Gráfico comparativo
gg_okun <- ggplot(df_okun, aes(x = Date)) +
  geom_line(aes(y = GDP_scaled, color = "PIB (GDP)"), size = 1.3) +
  geom_line(aes(y = Unemployment_scaled, color = "Desempleo"), size = 1.3) +
  scale_color_manual(
    values = c("PIB (GDP)" = paleta_colores[["verde"]],
               "Desempleo" = paleta_colores[["berenjena"]])
  ) +
  scale_x_yearqtr(
    breaks = seq(from = min(df_okun$Date),
                 to = max(df_okun$Date),
                 by = 2),
    format = "%Y"
  ) +
  labs(
    title = "Relación entre PIB y Desempleo en Australia",
    subtitle = "Evolución trimestral (valores estandarizados)",
    x = "Año",
    y = "Escala estandarizada",
    color = "Variable"
  ) +
  tema_economico()

gg_okun

#==============
#UNIR LOS DATAFRAMES
#=============
df_macro <- st_gdp %>%
  left_join(st_unemployment_df, by = "Date") %>%
  left_join(st_cpi, by = "Date") %>%
  left_join(st_money_supply_df, by = "Date") %>%
  left_join(st_stock_market_df, by = "Date")
df_macro

df_macro_long <- df_macro %>%
  pivot_longer(cols = -Date, names_to = "Variable", values_to = "Valor")

gg_macro_facet <- ggplot(df_macro_long, aes(x = Date, y = Valor)) +
  geom_line(color = "#2C3E50", size = 1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Evolución de indicadores macroeconómicos en Australia",
    x = "Año", y = "Valor"
  ) +
  tema_economico()

gg_macro_facet
