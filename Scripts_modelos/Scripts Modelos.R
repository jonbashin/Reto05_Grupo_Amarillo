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
dim(df_final)


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

df_trimestral <- monthly_to_quarterly(df_final)




#############################################################################################################################################3
class(serie)
frequency(df_trimestral)
start(df_trimestral)
end(df_trimestral)
autoplot(df_trimestral)
tsdisplay(df_trimestral)
tsdisplay(diff(df_trimestral))

serie<- ts(df_trimestral)




#######################   TESTSES PARA CONVERTIR CADA VARIABLE A ESTACIONARIA  #######################3

#mirar la varianza y en casode que tenga quitarla

####################          MONEY SUPLLY BILLION CURRENCY UNITS               ####################
####################################################################################################
####--------           PRIMERA RONDA (Serie Original) SIN HACER DIFF           -----------------------------
# Test ADF
adf_ms <- adf.test(df$Money.supply.billion.currency.units)
if (adf_ms$p.value < 0.05) {
  print("ADF: estacionaria")
  } else {
  print("ADF: NO estacionaria")}

#Test KPSS
kpss_ms <- kpss.test(df$Money.supply.billion.currency.units, null="Level")
if (kpss_ms$p.value < 0.05) {
  print("KPSS: NO estacionaria")
  } else {
  print("KPSS: estacionaria")}

####---------------------          PRIMER DIFF                        ---------------------------------------------------
ms_diff1 <- diff(df$Money.supply.billion.currency.units, differences = 1)
adf_ms1 <- adf.test(ms_diff1)
if (adf_ms1$p.value < 0.05) {
  print("ADF (diff1): estacionaria")
  } else {
  print("ADF (diff1): NO estacionaria")}

kpss_ms1 <- kpss.test(ms_diff1, null="Level")
if (kpss_ms1$p.value < 0.05) {
  print("KPSS (diff1): NO estacionaria")
  } else {
  print("KPSS (diff1): estacionaria")}


####################          UNEMPLOYMENT RATE (%)                ######################################
####################################################################################################
####--------           PRIMERA RONDA (Serie Original) SIN HACER DIFF           -----------------------------
adf_ur <- adf.test(df$Unemployment.rate.percent)
if (adf_ur$p.value < 0.05) {
  print("ADF: estacionaria")
  } else {
  print("ADF: NO estacionaria")}

kpss_ur <- kpss.test(df$Unemployment.rate.percent, null="Level")
if (kpss_ur$p.value < 0.05) {
  print("KPSS: NO estacionaria")
  } else {
  print("KPSS: estacionaria")}

####---------------------          PRIMER DIFF                        ---------------------------------------------------

ur_diff1 <- diff(df$Unemployment.rate.percent, differences = 1)
adf_ur1 <- adf.test(ur_diff1)
if (adf_ur1$p.value < 0.05) {
  print("ADF (diff1): estacionaria")
  } else {
  print("ADF (diff1): NO estacionaria")}

kpss_ur1 <- kpss.test(ur_diff1, null="Level")
if (kpss_ur1$p.value < 0.05) {
  print("KPSS (diff1): NO estacionaria")
  } else {
  print("KPSS (diff1): estacionaria")}


####################          STOCK MARKET INDEX                ######################################
####################################################################################################


####--------           PRIMERA RONDA (Serie Original) SIN HACER DIFF           -----------------------------
adf_sm <- adf.test(df$Stock.market.index)
if (adf_sm$p.value < 0.05) {
  print("ADF: estacionaria")
} else {
    print("ADF: NO estacionaria")}

kpss_sm <- kpss.test(df$Stock.market.index, null="Level")
if (kpss_sm$p.value < 0.05) {
  print("KPSS: NO estacionaria")
} else {
    print("KPSS: estacionaria")}

####---------------------          PRIMER DIFF                        ---------------------------------------------------

sm_diff1 <- diff(df$Stock.market.index, differences = 1)
adf_sm1 <- adf.test(sm_diff1)
if (adf_sm1$p.value < 0.05) {
  print("ADF (diff1): estacionaria")
} else {
    print("ADF (diff1): NO estacionaria")}

kpss_sm1 <- kpss.test(sm_diff1, null="Level")
if (kpss_sm1$p.value < 0.05) {
  print("KPSS (diff1): NO estacionaria")
} else {
    print("KPSS (diff1): estacionaria")}


####################          GDP (Billion currency units)                ######################################
####################################################################################################


####--------           PRIMERA RONDA (Serie Original) SIN HACER DIFF           -----------------------------
adf_gdp <- adf.test(df$GDP.billion.currency.units)
if (adf_gdp$p.value < 0.05) {
  print("ADF: estacionaria")
} else {
    print("ADF: NO estacionaria")}

kpss_gdp <- kpss.test(df$GDP.billion.currency.units, null="Level")
if (kpss_gdp$p.value < 0.05) {
  print("KPSS: NO estacionaria")
} else {
    print("KPSS: estacionaria")}

####---------------------          PRIMER DIFF                        ---------------------------------------------------

gdp_diff1 <- diff(df$GDP.billion.currency.units, differences = 1)
adf_gdp1 <- adf.test(gdp_diff1)
if (adf_gdp1$p.value < 0.05) {
  print("ADF (diff1): estacionaria")
} else {
    print("ADF (diff1): NO estacionaria")}

kpss_gdp1 <- kpss.test(gdp_diff1, null="Level")
if (kpss_gdp1$p.value < 0.05) {
  print("KPSS (diff1): NO estacionaria")
} else {
    print("KPSS (diff1): estacionaria")}



#####################          CONSUMER PRICE                #####################################
##################################################################################################

####--------           PRIMERA RONDA (Serie Original) SIN HACER DIFF           -----------------------------
adf_cpi <- adf.test(df$Consumer.Price.Index..CPI.)
if (adf_cpi$p.value < 0.05) {
  print("ADF: estacionaria")
} else {
    print("ADF: NO estacionaria")}

kpss_cpi <- kpss.test(df$Consumer.Price.Index..CPI., null="Level")
if (kpss_cpi$p.value < 0.05) {
  print("KPSS: NO estacionaria")
} else {
    print("KPSS: estacionaria")}

# Primera diferencia
cpi_diff1 <- diff(df$Consumer.Price.Index..CPI., differences = 1)
adf_cpi1 <- adf.test(cpi_diff1)
if (adf_cpi1$p.value < 0.05) {
  print("ADF (diff1): estacionaria")
} else {
    print("ADF (diff1): NO estacionaria")}

kpss_cpi1 <- kpss.test(cpi_diff1, null="Level")
if (kpss_cpi1$p.value < 0.05) {
  print("KPSS (diff1): NO estacionaria")
} else {
    print("KPSS (diff1): estacionaria")}

