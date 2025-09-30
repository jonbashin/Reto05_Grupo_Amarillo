#1. OBJETIVO

#Cargar librerias
library(dplyr)
library(readxl)
library(naniar)

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




#########################           IMPUTAR VALORES DE NA                #################################

#Mirar cantidad de NA-s
miss_var_summary(df_final)
#HAY MISSINGS EN VARIABLES --> SE DEBE IMPUTAR (Unemployment rate percent,Money supply billion currency units,Stock market index )

#Identificar que instancias tienen NA, para buscarlas en internet.
instancias_NA <- df_final %>%
  filter(
    is.na(`Unemployment rate percent`) |
      is.na(`Money supply billion currency units`) |
      is.na(`Stock market index`))
instancias_NA

#Voy a imputar los valores de 2022 apartir del mes 8, el 8 inlcuido ya que no tengo mas datos de ahi para adelante.

#2022-7--> Unemployment Rate
df_final$`Unemployment rate percent`[df_final$Country == "Australia" & 
                                       df_final$Code == "AUS" & 
                                       df_final$Year == 2022 &
                                       df_final$Month == 7] <- 3.40

#2022-8--> Money supply billion currency units 
df_final$`Money supply billion currency units`[df_final$Country == "Australia" & 
                                       df_final$Code == "AUS" & 
                                       df_final$Year == 2022 &
                                       df_final$Month == 8] <- 2780.6
#2022-8--> Unemployment rate percent
df_final$`Unemployment rate percent`[df_final$Country == "Australia" & 
                                                 df_final$Code == "AUS" & 
                                                 df_final$Year == 2022 &
                                                 df_final$Month == 8] <- 3.5

#Links usados--> https://tradingeconomics.com/australia/unemployment-rate
#                https://tradingeconomics.com/australia/money-supply-m3 



###################################       DATOS TRIMESTRALES       ################################

monthly_to_quarterly <- function(df) {
  
  # Asegurarse de que los nombres son correctos
  df <- df %>%
    mutate(
      `Money supply billion currency units` = as.numeric(`Money supply billion currency units`),
      `Unemployment rate percent` = as.numeric(`Unemployment rate percent`),
      `Stock market index` = as.numeric(`Stock market index`),
      `GDP.billion.currency.units` = as.numeric(`GDP.billion.currency.units`),
      `Consumer.Price.Index..CPI.` = as.numeric(`Consumer.Price.Index..CPI.`),
      Quarter = ceiling(Month / 3)
    )
  
  # Calcular los agregados trimestrales para columnas mensuales
  df_monthly_aggr <- df %>%
    group_by(Country, Code, ContinentCode, Year, Quarter) %>%
    summarise(
      `Money supply billion currency units` = sum(`Money supply billion currency units`, na.rm = TRUE),
      `Unemployment rate percent` = mean(`Unemployment rate percent`, na.rm = TRUE),
      `Stock market index` = mean(`Stock market index`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Extraer solo una fila por trimestre con los datos trimestrales (GDP y CPI)
  df_quarterly_static <- df %>%
    filter(Month %% 3 == 0) %>%  # Seleccionar el último mes de cada trimestre (marzo, junio, etc.)
    mutate(Quarter = ceiling(Month / 3)) %>%
    select(Country, Code, ContinentCode, Year, Quarter,
           `GDP.billion.currency.units`, `Consumer.Price.Index..CPI.`)
  
  # Unir ambas partes (mensual agregada + trimestral ya existente)
  df_quarterly <- left_join(df_monthly_aggr, df_quarterly_static,
                            by = c("Country", "Code", "ContinentCode", "Year", "Quarter"))
  
  return(df_quarterly)
}

# Ejecutar la función
df_trimestral <- monthly_to_quarterly(df_final)

