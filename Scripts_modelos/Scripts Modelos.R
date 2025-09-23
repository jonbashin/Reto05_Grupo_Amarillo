#1 OBJETIVO

#Cargar librerias
library(dplyr)
library(readxl)

#Cargar datos
exogenos <- read_excel("DATOS/originales/exogenas_paises_punto2.xlsx")
pib_ipc<- read.csv("DATOS/originales/pib_ipc_paises_punto2.csv")

#Analizar datos
summary(exogenos)
str(exogenos)
length(exogenos)
summary(pib_ipc)
str(pib_ipc)
length(pib_ipc)


#Filtrar datos por pais
unique(exogenos$Country)
unique(pib_ipc$Country)

exogenos_AUS<- exogenos%>% filter(Country=="Australia")
pib_ipc_AUS<- pib_ipc %>% filter(Country == "Australia")

#Juntar df
df_final <- exogenos_AUS %>%
  left_join(pib_ipc_AUS, by = c("Country", "Code", "ContinentCode", "Year", "Month"))


#Pasar el df con datos trimestrales
monthly_to_quarterly <- function(df) {
  
  df_quarterly <- df %>%
    # Convertir las columnas numéricas que vienen como character a numeric
    mutate(
      `Money supply billion currency units` = as.numeric(`Money supply billion currency units`),
      `Unemployment rate percent` = as.numeric(`Unemployment rate percent`),
      `Stock market index` = as.numeric(`Stock market index`),
      Quarter = ceiling(Month / 3)  # Crear columna trimestre
    ) %>%
    group_by(Country, Code, ContinentCode, Year, Quarter) %>%
    summarise(
      `Money supply billion currency units` = sum(`Money supply billion currency units`, na.rm = TRUE),  # Sumar totales
      `Unemployment rate percent` = mean(`Unemployment rate percent`, na.rm = TRUE),                     # Promedio tasas
      `Stock market index` = mean(`Stock market index`, na.rm = TRUE),                                   # Promedio índice
      .groups = "drop"
    )
  
  return(df_quarterly)
}

df_quarterly <- monthly_to_quarterly(df_final)
