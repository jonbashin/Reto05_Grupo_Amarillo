
#####---- LIMPIEZA DE DATOS

# LIBRERÍAS NECESARIAS
# =========================
library(readxl)
library(dplyr)
library(tidyr)
library(naniar)
library(forecast)
library(zoo)
library(ggplot2)
library(VIM)
library(tseries)
library(gridExtra)
library(astsa)

paleta <- c("#c88fb2",  "#8db41c",  "#93044e","#D1006F",  "#F5F0E6",  "#4D4D4D")

# 1. CARGAR DATOS
# =========================
exogenos <- read_excel("DATOS/originales/exogenas_paises_punto2.xlsx")
pib_ipc <- read.csv("DATOS/originales/pib_ipc_paises_punto2.csv")

# 2. FILTRAR SOLO AUSTRALIA
# =========================
exogenos_AUS <- exogenos %>% filter(Country == "Australia")
pib_ipc_AUS <- pib_ipc %>% filter(Country == "Australia")

# 3. UNIR LOS DOS DATA FRAMES
# =========================
df_final <- exogenos_AUS %>%
  left_join(pib_ipc_AUS, by = c("Country", "Code", "ContinentCode", "Year", "Month"))

# 4. IMPUTACIÓN MANUAL DE NA PARA ÚLTIMOS MESES (ejemplo 2022)
# =========================
#HAY MISSINGS EN VARIABLES --> SE DEBE IMPUTAR (Unemployment rate percent,Money supply billion currency units,Stock market index )

#Identificar que instancias tienen NA, para buscarlas en internet.
instancias_NA <- df_final %>%
  filter(
    is.na(`Unemployment rate percent`) |
      is.na(`Money supply billion currency units`) |
      is.na(`Stock market index`))
instancias_NA

#Imputaremos los valores de 2022 apartir del mes 9, el 9 incluido ya que no tengo mas datos de ahi para adelante, para ninugna variable

#2022-7--> Unemployment Rate
df_final$`Unemployment rate percent`[df_final$Country == "Australia" &
                                       df_final$Code == "AUS" &
                                       df_final$Year == 2022 &
                                       df_final$Month == 7] <- 3.40
#2022-8--> Unemployment rate percent
df_final$`Unemployment rate percent`[df_final$Country == "Australia" &
                                       df_final$Code == "AUS" &
                                       df_final$Year == 2022 &
                                       df_final$Month == 8] <- 3.5

#2022-8--> Money supply billion currency units
df_final$`Money supply billion currency units`[df_final$Country == "Australia" &
                                                 df_final$Code == "AUS" &
                                                 df_final$Year == 2022 &
                                                 df_final$Month == 8] <- 2780.6

#2022-9--> Unemployment rate percent
df_final$`Unemployment rate percent`[df_final$Country == "Australia" &
                                       df_final$Code == "AUS" &
                                       df_final$Year == 2022 &
                                       df_final$Month == 9] <- 3.60

#2022-9--> Money supply billion currency units
df_final$`Money supply billion currency units`[df_final$Country == "Australia" &
                                                 df_final$Code == "AUS" &
                                                 df_final$Year == 2022 &
                                                 df_final$Month == 9] <- 2780.4

#2022-9--> Stock market index
df_final$`Stock market index`[df_final$Country == "Australia" &
                                df_final$Code == "AUS" &
                                df_final$Year == 2022 &
                                df_final$Month == 9] <- 122.62


#Links usados--> https://tradingeconomics.com/australia/unemployment-rate
#                https://tradingeconomics.com/australia/money-supply-m3
#                https://fred.stlouisfed.org/series/SPASTT01AUM661N

#Comprobar NA-s
miss_var_summary(df_final)
instancias_NA_final <- df_final %>%
  filter(
    is.na(`Unemployment rate percent`) |
      is.na(`Money supply billion currency units`) |
      is.na(`Stock market index`))
instancias_NA_final
miss_var_summary(df_final)
#Quedan 3 datos de cada variable (NA en datos de 2022 apartir del mes 10)
#Solo quedan NA-s en la variables GDP y CPI porque hay solo datos trimestrales para esas variables
#Los NA-s de la variables money supply, unemplyment rate y stock market son lo que hay que predecir.

#Guardamos la base de datos limpia
#write.csv(df_final, file = "Datos_Limpios_Australia.csv", row.names = FALSE)



##############################################################################################################################################################
##############################################################################################################################################################
##########################              SERIES TEMPORALES            #########################################################################################

#Cargar datos:
#=========================
datos_limpios_AUS<- read.csv("DATOS/limpios/Datos_Limpios_Australia.csv")

#Analizar los datos
str(datos_limpios_AUS)
summary(datos_limpios_AUS)
dim(datos_limpios_AUS)
head(datos_limpios_AUS)
colnames(datos_limpios_AUS)



#Las variables PCI y GDP las tenemos en trimestrales ya en los datos Originales, en cambio Unemployment, money supply y Stock Market estan en mensuales.
#Por lo que las pasaremos a series trimestrals las tres que estan en mensual, ya que trabajaremos con datos trimestrales para hacer la prediccion.

# 1. PASAR A SERIES TEMPORALES TRIMESTRALES
# =========================

# ---- VARIABLES MENSUALES A TS ---- 
#Convertir a ts mensual 
unemployment_ts_mensual<- ts(as.numeric(datos_limpios_AUS$Unemployment.rate.percent), start= c(1996,1), frequency=12)
money_supply_ts_mensual<- ts(as.numeric(datos_limpios_AUS$Money.supply.billion.currency.units), start= c(1996,1), frequency=12)
stock_market_ts_mensual<- ts(as.numeric(datos_limpios_AUS$Stock.market.index), start= c(1996,1), frequency=12)
#CPI y GDP son trimestrales por lo que no se pude sacar el mensual

#Guardar series temporales Mensuales
# saveRDS(unemployment_ts_mensual, "unemployment_ts_mensual.rds")
# saveRDS(money_supply_ts_mensual, "money_supply_ts_mensual.rds")
# saveRDS(stock_market_ts_mensual, "stock_market_ts_mensual.rds")


# ---- VARIABLES TRIMESTRALES (CPI y GDP)----
# CPI y GDP ya son trimestrales. Pasaremos a series temporales pero necesitaremos eliminar los NA-s
#-->CPI
# Tomar solo los valores numéricos (sin NAs iniciales)
cpi_vals <- na.omit(as.numeric(datos_limpios_AUS$Consumer.Price.Index..CPI.))
# Crear serie trimestral empezando en el primer año y trimestre válido
cpi_ts_trimestral <- ts(cpi_vals, start=c(1996,3), frequency=4)
#Guardar
#saveRDS(cpi_ts_trimestral, "cpi_ts_trimestral.rds")

#-->GDP
gdp_vals <- na.omit(as.numeric(datos_limpios_AUS$GDP.billion.currency.units))
gdp_ts_trimestral <- ts(gdp_vals, start=c(1996,3), frequency=4)
#Guardar
#saveRDS(gdp_ts_trimestral, "gdp_ts_trimestral.rds")


# ---- CONVERTIR VARIABLES MENSUALES A TRIMESTRALES ----
#Se ha decidido convertir en serie temporal trimestral--> Unemployment Rate= Media de lso tres meses
# Money Supply / Stock Market = Valor del ultimo mes (Enero,Febrero, Marzo quedarnos con MARZO)

#--Unemployment: media trimestral
unemployment_ts_trimestral <- aggregate(unemployment_ts_mensual, nfrequency=4, FUN=mean)
#Guardar
#saveRDS(unemployment_ts_trimestral, "unemployment_ts_trimestral.rds")

#--Money supply: último valor del trimestre
money_supply_ts_trimestral <- aggregate(money_supply_ts_mensual, nfrequency = 4, FUN = function(x) tail(x, 1))
#Guardar
#saveRDS(money_supply_ts_trimestral, "money_supply_ts_trimestral.rds")

#--Stock market: último valor del trimestre
stock_market_ts_trimestral <- aggregate(stock_market_ts_mensual, nfrequency = 4, FUN = function(x) tail(x, 1))
#Guardar
#saveRDS(stock_market_ts_trimestral, "stock_market_ts_trimestral.rds")


# 2. GRAFICAR SERIES MENSUALES Y TRIMESTRALES 
# =========================

# ---- SERIES MENSUALES ----
#--------------------------------------------------------------------------
#png("graficos_series_mensuales_todos.png", width=800, height=1200)

par(mfrow=c(3,1), mar=c(4,4,2,1))  # 3 filas, 1 columna
plot(unemployment_ts_mensual, type="l", col= paleta[1], lwd=2,
     main="Tasa de Desempleo Mensual - Australia",
     ylab="%", xlab="Año")

plot(money_supply_ts_mensual, type="l", col=paleta[4], lwd=2,
     main="Money Supply Mensual - Australia",
     ylab="Billion Currency Units", xlab="Año")

plot(stock_market_ts_mensual, type="l", col=paleta[5], lwd=2,
     main="Stock Market Index Mensual - Australia",
     ylab="Index", xlab="Año")

par(mfrow=c(1,1))
#dev.off()



# ---- SERIES TRIMESTRALES ----
#--------------------------------------------------------------------------
#png("graficos_series_trimestrales_todos.png", width=800, height=1200)

par(mfrow=c(5,1), mar=c(4,4,2,1))  # 5 filas, 1 columna

# Unemployment trimestral
plot(unemployment_ts_trimestral, type="l", col=paleta[1], lwd=2,
     main="Tasa de Desempleo Trimestral - Australia",
     ylab="%", xlab="Año")

# Money supply trimestral
plot(money_supply_ts_trimestral, type="l", col=paleta[4], lwd=2,
     main="Money Supply Trimestral - Australia",
     ylab="Billion Currency Units", xlab="Año")

# Stock market trimestral
plot(stock_market_ts_trimestral, type="l", col=paleta[5], lwd=2,
     main="Stock Market Index Trimestral - Australia",
     ylab="Index", xlab="Año")

# CPI trimestral
plot(cpi_ts_trimestral, type="l", col=paleta[2], lwd=2,
     main="CPI Trimestral - Australia",
     ylab="Index", xlab="Año")

# GDP trimestral
plot(gdp_ts_trimestral, type="l", col=paleta[3], lwd=2,
     main="GDP Trimestral - Australia",
     ylab="Billion Currency Units", xlab="Año")

par(mfrow=c(1,1))
#dev.off()

# --- CPI Trimestral ---
#png("pci_trimestral.png", width=800, height=400)
plot(cpi_ts_trimestral, type="l", col=paleta[2], lwd=2,
     main="CPI Trimestral - Australia",
     ylab="Index", xlab="Año")
#dev.off()

# --- GDP Trimestral ---
#png("pib_trimestral.png", width=800, height=400)
plot(gdp_ts_trimestral, type="l", col=paleta[3], lwd=2,
     main="GDP Trimestral - Australia",
     ylab="Billion Currency Units", xlab="Año")
#dev.off()





# 3. DETECCIÓN Y LIMPIEZA DE OUTLIERS
# =========================
#Solo hacerlo en trimestral

# ---- DETECCIÓN DE OUTLIERS ----
# Con tsoutliers podemos ver en qué fechas aparecen valores extraños
# Esto NO limpia, solo reporta
print(tsoutliers(unemployment_ts_trimestral))
print(tsoutliers(money_supply_ts_trimestral))
print(tsoutliers(stock_market_ts_trimestral))
print(tsoutliers(cpi_ts_trimestral))
print(tsoutliers(gdp_ts_trimestral))

# ---- LIMPIEZA DE OUTLIERS ----
# Usamos tsclean() que reemplaza valores atípicos por interpolaciones robustas
unemployment_ts_trimestral_sin_outliers  <- tsclean(unemployment_ts_trimestral)
money_supply_ts_trimestral_sin_outliers  <- tsclean(money_supply_ts_trimestral)
stock_market_ts_trimestral_sin_outliers  <- tsclean(stock_market_ts_trimestral)
cpi_ts_trimestral_sin_outliers    <- tsclean(cpi_ts_trimestral)
gdp_ts_trimestral_sin_outliers    <- tsclean(gdp_ts_trimestral)

# ---- 3. VISUALIZACIÓN COMPARATIVA ----
# ---- 2. Función auxiliar para graficar ----
plot_outliers <- function(original_ts, titulo){
  outs <- tsoutliers(original_ts)
  
  # Paleta de colores si no está definida
  if (!exists("paleta")) {
  }
  
  # ---- Ajuste de márgenes para dejar espacio a la derecha ----
  par(mar = c(4, 4, 2, 6))  # espacio a la derecha
  par(xpd = TRUE)          # permite dibujar fuera del área del plot
  
  # ---- Definir rango para el eje Y con margen superior ----
  ylim_range <- c(min(original_ts, na.rm=TRUE),
                  max(original_ts, na.rm=TRUE) * 1.1)
  
  # ---- Graficar la serie original ----
  plot(original_ts, type = "l", lwd = 2, col = paleta[2],
       main = paste(titulo, "- Outliers marcados"),
       ylab = "Valor", xlab = "Año",
       ylim = ylim_range)
  
  # ---- Marcar outliers si existen ----
  if (length(outs$index) > 0) {
    points(time(original_ts)[outs$index],
           original_ts[outs$index],
           col = paleta[1], pch = 19, cex = 1.2)
  }
  
  # ---- Leyenda fuera del área del gráfico pero visible ----
  legend("topright", inset = c(-0.1, 0), xpd = TRUE,
         legend = c("Serie original", "Outliers"),
         col = c(paleta[2], paleta[1]),
         lty = c(1, NA), pch = c(NA, 19),
         bty = "n", x.intersp = 0.8, text.width = strwidth("Serie original") * 1.2)
  
  # ---- Restaurar parámetros por defecto ----
  par(xpd = FALSE)
  par(mar = c(5, 4, 4, 2) + 0.1)
}



#png("outliers_unemployment.png", width=800, height=800)
plot_outliers(unemployment_ts_trimestral, 
              "Unemployment Rate (Trimestral)")
#dev.off()

#png("outliers_money_supply.png", width=800, height=800)
plot_outliers(money_supply_ts_trimestral, "Money Supply (Trimestral)")
#dev.off()

#png("outliers_stock_market.png", width=800, height=800)
plot_outliers(stock_market_ts_trimestral, "Stock Market Index (Trimestral)")
#dev.off()

#png("outliers_pci.png", width=800, height=800)
plot_outliers(cpi_ts_trimestral, "PCI (Trimestral)")
#dev.off()

#png("outliers_PIB.png", width=800, height=800)
plot_outliers(gdp_ts_trimestral, "PIB (Trimestral)")
#dev.off()


# 4. SEPARAR TRAIN / TEST
#======================
#Queremos predecir datos que ya tenemos para evaluar los modelos
#Separar datos en train y test

train_ipc<-window(cpi_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2020,4))
test_ipc<-window(cpi_ts_trimestral_sin_outliers,start=c(2021,1), end= c(2022,2))

train_pib<-window(gdp_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2020,4))
test_pib<-window(gdp_ts_trimestral_sin_outliers,start=c(2021,1), end= c(2022,2))


# 5. TIENEN VARIANZA?
#======================

# GDP
#png("varianza_pib_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(train_pib, col=paleta[3], lwd=2,
       main="PIB (Sin outliers)  ", ylab="Valor", xlab="Tiempo")
#dev.off()
# Varianza creciente (ligera), con estacionalidad

# CPI
#png("varianza_pci_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(train_ipc, col=paleta[2], lwd=2,
       main="PCI (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Constante

# Stock Market
#png("varianza_stock_market_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(stock_market_ts_trimestral_sin_outliers, col=paleta[1], lwd=2,
       main="Stock Market (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Creciente

# Money Supply
#png("money_supply_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(money_supply_ts_trimestral_sin_outliers, col=paleta[1], lwd=2,
       main="Money Supply (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza constante

# Unemployment
#png("unemployment_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(unemployment_ts_trimestral_sin_outliers, col=paleta[1], lwd=2,
       main="Unemployment (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Constante



# ESTABILIZAR VARIANZA
# -------------------------

# Transformación log para estabilizar la varianza
stock_market_ts_trimestral_sin_outliers_log <- log(stock_market_ts_trimestral_sin_outliers)
train_pib_log <- log(train_pib)

# Graficar después de log-transform
#png("varianza_estabilizada_stock_market_Serie_log.png", width=800, height=600)
tsplot(stock_market_ts_trimestral_sin_outliers_log, col=paleta[1], lwd=2,
       main="Stock Market - Serie log-transformada", ylab="Log-Valor", xlab="Tiempo")
#dev.off()

#png("varianza_estabilizada_pib_Serie_log.png", width=800, height=600)
tsplot(train_pib_log, col=paleta[3], lwd=2,
       main="GDP - Serie log-transformada", ylab="Log-Valor", xlab="Tiempo")
#dev.off()


# 6. DESCOPOSICION DE LA SERIE
#=======================
# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(train_ipc)
plot(descomposicion_ipc, col=paleta[1])
#png("ipc_descomposicion.png", width = 800, height = 600)  # Guardar gráfico
plot(descomposicion_ipc, col=paleta[1])
#dev.off()
descomposicion_ipc$seasonal
descomposicion_ipc$trend
random_ipc <- descomposicion_ipc$random

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(train_pib_log)
plot(descomposicion_pib, col= paleta[1])
#png("pib_descomposicion.png", width = 800, height = 600)
plot(descomposicion_pib, col=paleta[1])
#dev.off()
descomposicion_pib$seasonal
descomposicion_pib$trend
random_pib <- descomposicion_pib$random

# Descomposición de la serie de masa monetaria
descomposicion_money_supply <- decompose(money_supply_ts_trimestral_sin_outliers)
plot(descomposicion_money_supply, col= paleta[1])
#png("money_supply_descomposicion.png", width = 800, height = 600)
plot(descomposicion_money_supply, col=paleta[1])
#dev.off()
descomposicion_money_supply$seasonal
descomposicion_money_supply$trend
random_money_supply <- descomposicion_money_supply$random

# Descomposición de la serie de indice bursatil
descomposicion_stock_market <- decompose(stock_market_ts_trimestral_sin_outliers_log)
plot(descomposicion_stock_market, col= paleta[1])
#png("stock_market_descomposicion.png", width = 800, height = 600)
plot(descomposicion_stock_market, col=paleta[1])
#dev.off()
descomposicion_stock_market$seasonal
descomposicion_stock_market$trend
random_stock_market <- descomposicion_stock_market$random

# Descomposición de la serie del paro
descomposicion_unemployment <- decompose(unemployment_ts_trimestral_sin_outliers)
plot(descomposicion_unemployment, col=paleta[1])
#png("unemployment_descomposicion.png", width = 800, height = 600)
plot(descomposicion_unemployment, col=paleta[1])
#dev.off()
descomposicion_unemployment$seasonal
descomposicion_unemployment$trend
random_unemployment <- descomposicion_unemployment$random

# 7. ANALIZAR ESTACIONALIDAD
#======================
#Comprobar estacionalidad apra ver si meter lag o no
nsdiffs(train_pib_log) #SI
nsdiffs(train_ipc) #NO
nsdiffs(money_supply_ts_trimestral_sin_outliers) #NO
nsdiffs(stock_market_ts_trimestral_sin_outliers_log) #NO
nsdiffs(unemployment_ts_trimestral_sin_outliers) #SI

# 8. ANALIZAR TENDENCIA
#========================

#Si el ESTIMATE (El valor estimado de la pendiente) = ES POSITIVO : Tendencia creciente
#                                                     ES NEGATIVO : Tendencia decreciente
#Si el p-value del coeficiente es < 0.05 hay tendencia.

#--- GDP
# Crear variable tiempo
t <- 1:length(train_pib_log)
train_gdp_log <- as.numeric(train_pib_log)
head(t)
length(t)

# Regresión lineal simple
modelo_tendencia1 <- lm(train_pib_log ~ t)
summary(modelo_tendencia1)
plot(t, train_pib_log, type="l", col=paleta[3], lwd=2,
     main="PIB con Tendencia",
     ylab="Log(PIB)", xlab="Tiempo")
abline(modelo_tendencia1, col="red", lwd=2)
legend("topleft", legend=c("Serie log", "Tendencia"), col=c(paleta[3],"red"), lwd=2)

#cuando t=1 --> 5.0057757 (Intercept, Estimate)
#intercept=5.0057757   Valor base de la serie en el primer periodo

#VALOR t=  0.0142585 (Cada period(mes) el log de la serie aumenta  0.0142585 )
#P-Value (del coeficiente) = 2e-16 
#Tendecia muy siginificativa (El p-value es muy pequeño)

#Cuánto sube realmente el desempleo por mes o por año???
exp(5.0057757)      # Valor base en desempleo original
exp(5.0057757 + 0.0142585*1)  # Primer mes
exp(5.0057757 + 0.0142585*12) # Después de 1 año
# La serie de desempleo sube lentamente cada mes, y acumulativamente aumenta más a lo largo del año.


# 9. ANALISIS DE AUTOCORRELACION
#=======================
# ACF y PACF
train_ipc <- na.omit(train_ipc)
train_pib_log <- na.omit(train_pib_log)
stock_market_ts_trimestral_sin_outliers_log <- na.omit(stock_market_ts_trimestral_sin_outliers_log)
money_supply_ts_trimestral_sin_outliers <- na.omit(money_supply_ts_trimestral_sin_outliers)
unemployment_ts_trimestral_sin_outliers <- na.omit(unemployment_ts_trimestral_sin_outliers)

# ---- IPC ----
#png("ACF_IPC_Sin_Diferencia.png", width=800, height=600)
ggAcf(train_ipc) + ggtitle("ACF de IPC (Sin diferenciar)") + theme_minimal()
#dev.off()

#png("PACF_IPC_Sin_Diferencia.png", width=800, height=600)
ggPacf(train_ipc) + ggtitle("PACF de IPC (Sin diferenciar)") + theme_minimal()
#dev.off()

# ---- Money Supply ----
#png("ACF_Money_Supply_Sin_Diferencia.png", width=800, height=600)
ggAcf(money_supply_ts_trimestral_sin_outliers) + ggtitle("ACF de Money Supply (Sin diferenciar)") + theme_minimal()
#dev.off()

#png("PACF_Money_Supply_Sin_Diferencia.png", width=800, height=600)
ggPacf(money_supply_ts_trimestral_sin_outliers) + ggtitle("PACF de Money Supply (Sin diferenciar)") + theme_minimal()
#dev.off()

# ---- GDP ----
#png("ACF_GDP_Sin_Diferencia.png", width=800, height=600)
ggAcf(train_pib_log) + ggtitle("ACF de PIB (Log, Sin diferenciar)") + theme_minimal()
#dev.off()

#png("PACF_GDP_Sin_Diferencia.png", width=800, height=600)
ggPacf(train_pib_log) + ggtitle("PACF de PIB (Log, Sin diferenciar)") + theme_minimal()
#dev.off()

# ---- Stock Market ----
#png("ACF_Stock_Market_Sin_Diferencia.png", width=800, height=600)
ggAcf(stock_market_ts_trimestral_sin_outliers_log) + ggtitle("ACF de Stock Market (Log, Sin diferenciar)") + theme_minimal()
#dev.off()

#png("PACF_Stock_Market_Sin_Diferencia.png", width=800, height=600)
ggPacf(stock_market_ts_trimestral_sin_outliers_log) + ggtitle("PACF de Stock Market (Log, Sin diferenciar)") + theme_minimal()
#dev.off()

# ---- Unemployment Rate ----
#png("ACF_Unemployment_Sin_Diferencia.png", width=800, height=600)
ggAcf(unemployment_ts_trimestral_sin_outliers) + ggtitle("ACF de Unemployment (Sin diferenciar)") + theme_minimal()
#dev.off()

#png("PACF_Unemployment_Sin_Diferencia.png", width=800, height=600)
ggPacf(unemployment_ts_trimestral_sin_outliers) + ggtitle("PACF de Unemployment (Sin diferenciar)") + theme_minimal()
#dev.off()

# 10. SON SERIES ESTACIONARIAS?
#=======================

#los lags de las diferencias van en funcion de si hay estacionalidad, se ven en los graficos de autocorrelacion. o con el decompose 
#!!!! HAY QUE USAR TS.DISPLAY, AUTOCORRELACION , Y AUTOCORRELACION PARCIAL
#HAY QUE IR MIRANDO ESTO DE POCO EN POCO Y A LA VEZ APLICAR LOS TESES PARA CONFIRMAR.


########### ---------- Money Supply (TRIMESTRAL)  #####################
#######################################################################

#Aplicaremos ts.display() para ver si graficamente si son estacionarias o no
#----                 SIN DIFERENCIA
tsdisplay(money_supply_ts_trimestral_sin_outliers)             

#----               PRIMERA DIFERENCIA
#Aplicar diferencia
money_supply_diff1 <- diff(money_supply_ts_trimestral_sin_outliers, differences = 1)
tsdisplay(money_supply_diff1)

#Aplicaremos los teses para asegurarnos de que no es estacionaria al 100%
#TEST ADF
adf_test_money_supply_1 <- adf.test(money_supply_diff1)
if(adf_test_money_supply_1$p.value < 0.05){
  print("Money Supply - ADF (diff1): estacionaria")
} else{
  print("Money Supply - ADF (diff1): NO estacionaria")
}
#NO ESTACIONARIA diff=1

#TEST KPSS
kpss_test_money_supply_1 <- kpss.test(money_supply_diff1, null="Level")
if(kpss_test_money_supply_1$p.value < 0.05){
  print("Money Supply - KPSS (diff1): NO estacionaria")
} else{
  print("Money Supply - KPSS (diff1): estacionaria")
}
#NO ESTACIONARIA diff=1

#TEST LJUNG-BOX
LBtest_money_supply_1 <- Box.test(money_supply_diff1, lag = 10, type="Ljung")
if (LBtest_money_supply_1$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff1): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff1): Ausencia de correlación")
}

#EXISTE CORRELACION

#---            PRIMERA DIFERRENCIA Y LAG                -----

acf(money_supply_ts_trimestral_sin_outliers)
money_supply_diff1_lag <- diff(money_supply_ts_trimestral_sin_outliers, lag=4)
tsdisplay(money_supply_diff1_lag) 


#Aplicaremos los teses para asegurarnos de que no es estacionaria al 100%
#TEST ADF
adf_test_money_supply_1_lag <- adf.test(money_supply_diff1_lag)
if(adf_test_money_supply_1_lag$p.value < 0.05){
  print("Money Supply - ADF (diff1, lag): estacionaria")
} else{
  print("Money Supply - ADF (diff1, lag): NO estacionaria")
}
# No ESTACIONARIA diff=1, lag=4 

#TEST KPSS
kpss_test_money_supply_1_lag <- kpss.test(money_supply_diff1_lag, null="Level")
if(kpss_test_money_supply_1_lag$p.value < 0.05){
  print("Money Supply - KPSS (diff1, lag): NO estacionaria")
} else{
  print("Money Supply - KPSS (diff1, lag): estacionaria")
}
# NO ESTACIONARIA diff=1, lag=4 

#TEST LJUNG-BOX
LBtest_money_supply_1_lag <- Box.test(money_supply_diff1_lag, lag = 10, type="Ljung")
if (LBtest_money_supply_1_lag$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff1, lag): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff1, lag): Ausencia de correlación")
}
#EXISTE CORRELACION
#SIigue sin dar por lo que aplicaremos la segunda diferencia

#----                 SEGUNDA DIFERENCIA

money_supply_diff2 <- diff(money_supply_ts_trimestral_sin_outliers, differences = 2)
tsdisplay(money_supply_diff2)

#Aplicar los teses
#TEST ADF
adf_test_money_supply_2 <- adf.test(money_supply_diff2)
if(adf_test_money_supply_2$p.value < 0.05){
  print("Money Supply - ADF (diff2): estacionaria")
} else{
  print("Money Supply - ADF (diff2): NO estacionaria")
}
# ESTACIONARIA diff=2

#TEST KPSS
kpss_test_money_supply_2 <- kpss.test(money_supply_diff2, null="Level")
if(kpss_test_money_supply_2$p.value < 0.05){
  print("Money Supply - KPSS (diff2): NO estacionaria")
} else{
  print("Money Supply - KPSS (diff2): estacionaria")
}
#ESTACIONARIA diff=2

#TEST LJUNG-BOX
LBtest_money_supply_2 <- Box.test(money_supply_diff2, lag = 10, type="Ljung")
if (LBtest_money_supply_2$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff2): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff2): Ausencia de correlación")
}
#EXISTE CORRELACION

#MONEY SUPPLY--> Estacionaria con la segunda diferencia!
#Cambiamos el nombre para que sea mas faci
money_supply_ts_trimestral_sin_outliers_estacionaria<- money_supply_diff2

#saveRDS(money_supply_ts_trimestral_sin_outliers_estacionaria, file = "Money_Supply_ts_ESTACIONARIA.rds")

#-----------------------------------------------------------------
#Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparacion)
#png("Money_supply_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))  # dos gráficos en una ventana
plot(money_supply_ts_trimestral_sin_outliers, type="l", main="Money Supply (log) - Serie original", ylab="Nivel (log)")
plot(money_supply_ts_trimestral_sin_outliers_estacionaria, type="l", main="Money Supply (log) - Segunda diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1)) 
#dev.off()

#-------    ACF Y PACF
#png("ACF_Money_Supply_Segunda_Diferencia.png", width=800, height=600)
ggAcf(money_supply_ts_trimestral_sin_outliers_estacionaria) + ggtitle("ACF de Money Supply (Segunda diferencia)") + theme_minimal()
#dev.off()

#png("PACF_Money_Supply_Segunda_Diferencia.png", width=800, height=600)
ggPacf(money_supply_ts_trimestral_sin_outliers_estacionaria) + ggtitle("PACF de Money Supply (Segunda diferencia)") + theme_minimal()
#dev.off()  

#-------   QQ-PLOT --> Sirve para evaluar si los residuos o valores transformados siguen una distribución normal.
#png("QQ_money_supply.png", width=800, height=600)
qqnorm(money_supply_ts_trimestral_sin_outliers_estacionaria, main="QQ-plot - Money Supply (log, diff2)")
qqline(money_supply_ts_trimestral_sin_outliers_estacionaria, col="red")
#dev.off()
#La linea roja representa como se verian los datos si sigugieran una distribucion normal




########### ---------- Unemployment Rate (TRIMESTRAL)  #####################
#######################################################################

#----                 SIN DIFERENCIA
tsdisplay(unemployment_ts_trimestral_sin_outliers)             

#----               PRIMERA DIFERENCIA
#Aplicar diferencia
unemployment_diff1 <- diff(unemployment_ts_trimestral_sin_outliers, differences = 1, lag=4)
tsdisplay(unemployment_diff1)

#Aplicaremos los tests para comprobra
#TEST ADF
adf_test_unemployment_1 <- adf.test(unemployment_diff1)
if(adf_test_unemployment_1$p.value < 0.05){
  print("Unemployment Rate - ADF (diff1): estacionaria")
} else{
  print("Unemployment Rate - ADF (diff1): NO estacionaria")
}
#NO ESTACIONARIA

#TEST KPSS
kpss_test_unemployment_1 <- kpss.test(unemployment_diff1, null="Level")
if(kpss_test_unemployment_1$p.value < 0.05){
  print("Unemployment Rate - KPSS (diff1): NO estacionaria")
} else{
  print("Unemployment Rate - KPSS (diff1): estacionaria")
}
#ESTACIONARIA

#TEST LJUNG-BOX
LBtest_unemployment_1 <- Box.test(unemployment_diff1, lag = 10, type="Ljung")
if (LBtest_unemployment_1$p.value < 0.05) {
  print("Unemployment Rate - Ljung-Box (diff1): Existe correlación")
} else {
  print("Unemployment Rate - Ljung-Box (diff1): Ausencia de correlación")
}
#EXISTE CORRELACION

#----               SEGUNDA DIFERENCIA
#Aplicar diferencia
unemployment_diff2 <- diff(diff(unemployment_ts_trimestral_sin_outliers, lag=4))
tsdisplay(unemployment_diff2)

#Aplicaremos los tests para comprobra
#TEST ADF
adf_test_unemployment_2 <- adf.test(unemployment_diff2)
if(adf_test_unemployment_2$p.value < 0.05){
  print("Unemployment Rate - ADF (diff2): estacionaria")
} else{
  print("Unemployment Rate - ADF (diff2): NO estacionaria")
}
#ESTACIONARIA

#TEST KPSS
kpss_test_unemployment_2 <- kpss.test(unemployment_diff2, null="Level")
if(kpss_test_unemployment_2$p.value < 0.05){
  print("Unemployment Rate - KPSS (diff2): NO estacionaria")
} else{
  print("Unemployment Rate - KPSS (diff2): estacionaria")
}
#ESTACIONARIA

#TEST LJUNG-BOX
LBtest_unemployment_1 <- Box.test(unemployment_diff1, lag = 10, type="Ljung")
if (LBtest_unemployment_1$p.value < 0.05) {
  print("Unemployment Rate - Ljung-Box (diff2): Existe correlación")
} else {
  print("Unemployment Rate - Ljung-Box (diff2): Ausencia de correlación")
}
#EXISTE CORRELACION

#UNEMPLOYMENT RATE--> Estacionaria con la segunda diferencia!
#Cambiamos el nombre para que sea mas faci
unemployment_ts_trimestral_sin_outliers_estacionaria<- unemployment_diff2

#saveRDS(unemployment_ts_trimestral_sin_outliers_estacionaria, file = "Unemployment_ts_ESTACIONARIA.rds")


#-----------------------------------------------------------------
#Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparacion)
#png("Unemployment_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))  # dos gráficos en una ventana
plot(unemployment_ts_trimestral_sin_outliers, type="l", main="Unemployment Rate (log) - Serie original", ylab="Nivel (log)")
plot(unemployment_ts_trimestral_sin_outliers_estacionaria, type="l", main="Unemployment Rate (log) - Segunda diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  
#dev.off()

#-------    ACF Y PACF
#png("ACF_Unemployment_Segunda_Diferencia.png", width=800, height=600)
ggAcf(unemployment_ts_trimestral_sin_outliers_estacionaria) + ggtitle("ACF de Unemployment (Segunda diferencia)") + theme_minimal()
#dev.off() 

#png("PACF_Unemployment_Primera_Diferencia.png", width=800, height=600)
ggPacf(unemployment_ts_trimestral_sin_outliers_estacionaria) + ggtitle("PACF de Unemployment (Segunda diferencia)") + theme_minimal()
#dev.off() 

#-------   QQ-PLOT 
#png("QQ_unemployment.png", width=800, height=600)
qqnorm(unemployment_ts_trimestral_sin_outliers_estacionaria, main="QQ-plot - Unemployment Rate (log, diff1)")
qqline(unemployment_ts_trimestral_sin_outliers_estacionaria, col="red")
#dev.off()


###########            GDP (Billion currency units) (TRIMESTRAL)  #####################
#######################################################################

#----                 SIN DIFERENCIA
#png("pib_0_diferencias.png", width = 1000, height = 800)  
tsdisplay(train_pib_log)
#dev.off()

#----               PRIMERA DIFERENCIA y lag
# Aplicar diferencia
#png("pib_1_diferencia.png", width = 1000, height = 800)  
gdp_diff1 <- diff(train_pib_log, differences = 1, lag=4)
tsdisplay(gdp_diff1)
#dev.off()

# Aplicamos los tests para comprobar
# TEST ADF
adf_test_gdp_1 <- adf.test(gdp_diff1)
if(adf_test_gdp_1$p.value < 0.05){
  print("GDP - ADF (diff1): estacionaria")
} else{
  print("GDP - ADF (diff1): NO estacionaria")
}
#NO ESTACIONARIA diff=1

# TEST KPSS
kpss_test_gdp_1 <- kpss.test(gdp_diff1, null="Level")
if(kpss_test_gdp_1$p.value < 0.05){
  print("GDP - KPSS (diff1): NO estacionaria")
} else{
  print("GDP - KPSS (diff1): estacionaria")
}
#NO ESTACIONARIA diff=1


# TEST LJUNG-BOX
LBtest_gdp_1 <- Box.test(gdp_diff1, lag = 10, type="Ljung")
if (LBtest_gdp_1$p.value < 0.05) {
  print("GDP - Ljung-Box (diff1): Existe correlación")
} else {
  print("GDP - Ljung-Box (diff1): Ausencia de correlación")
}
#EXISTE CORRELACION

# -----------------   SEGUNDA DIFERENCIA
# Aplicar diferencia
png("pib_2_diferencia.png", width = 1000, height = 800)  
gdp_diff2 <- diff(diff(train_pib_log, lag=4))
tsdisplay(gdp_diff2)
dev.off()

# Aplicamos los tests para comprobar
# TEST ADF
adf_test_gdp_2 <- adf.test(gdp_diff2)
if(adf_test_gdp_2$p.value < 0.05){
  print("GDP - ADF (diff1): estacionaria")
} else{
  print("GDP - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=2

# TEST KPSS
kpss_test_gdp_2 <- kpss.test(gdp_diff2, null="Level")
if(kpss_test_gdp_2$p.value < 0.05){
  print("GDP - KPSS (diff2): NO estacionaria")
} else{
  print("GDP - KPSS (diff2): estacionaria")
}
#ESTACIONARIA diff=2

# TEST LJUNG-BOX
LBtest_gdp_2 <- Box.test(gdp_diff2, lag = 10, type="Ljung")
if (LBtest_gdp_2$p.value < 0.05) {
  print("GDP - Ljung-Box (diff2): Existe correlación")
} else {
  print("GDP - Ljung-Box (diff2): Ausencia de correlación")
}
#EXISTE CORRELACION

# GDP --> Estacionaria con la segunda diferencia!
# Cambiamos el nombre para facilitar su uso.
train_gdp_estacionaria <- gdp_diff2

#saveRDS(train_gdp_estacionaria, file = "PIB_Train_ts_ESTACIONARIA.rds")


#-----------------------------------------------------------------
# Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparación)
#png("PIB_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))
plot(train_pib_log, type="l", main="GDP (log) - Serie original", ylab="Nivel (log)")
plot(train_gdp_estacionaria, type="l", main="GDP (log) - Segunda diferencia (Estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1)) 
#dev.off()

#-------    ACF Y PACF
#png("ACF_PIB_Segunda_Diferencia.png", width=800, height=600)
ggAcf(train_gdp_estacionaria) + ggtitle("ACF de PIB (Segunda diferencia)") + theme_minimal()
#dev.off() 

#png("PACF_PIB_Segunda_Diferencia.png", width=800, height=600)
ggPacf(train_gdp_estacionaria) + ggtitle("PACF de PIB (Segunda diferencia)") + theme_minimal()
#dev.off() 

#-------   QQ-PLOT
#png("PIB_QQ_gdp.png", width=800, height=600)
qqnorm(train_gdp_estacionaria, main="QQ-plot - GDP (log, diff1)")
qqline(train_gdp_estacionaria, col="red")
#dev.off()


###########            CPI (Consumer Price Index) (TRIMESTRAL)  #####################
#####################################################################################
#----                 SIN DIFERENCIA
tsdisplay(train_ipc)             

#----               PRIMERA DIFERENCIA
# Aplicar diferencia
cpi_diff1 <- diff(train_ipc, differences = 1)
tsdisplay(cpi_diff1)

#Nos aseguraremos uqe si lo es con los teses

#----               TESTS DE ESTACIONARIEDAD
#TEST ADF
# Aplicamos el test ADF usando k óptimo.
# Esto asegura que usamos un k que dé un resultado más robusto y reproducible.
adf_test_cpi_1 <- adf.test(cpi_diff1)
if(adf_test_cpi_1$p.value < 0.05){
  print("CPI - ADF (diff1): estacionaria")
} else{
  print("CPI - ADF (diff1): NO estacionaria")
}
# ESTACIONARIA diff=1 

# TEST KPSS
kpss_test_cpi_1 <- kpss.test(cpi_diff1, null="Level")
if(kpss_test_cpi_1$p.value < 0.05){
  print("CPI - KPSS (diff1): NO estacionaria")
} else{
  print("CPI - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1


# TEST LJUNG-BOX
LBtest_cpi_1 <- Box.test(cpi_diff1, lag = 10, type="Ljung")
if (LBtest_cpi_1$p.value < 0.05) {
  print("CPI - Ljung-Box (diff1): Existe correlación")
} else {
  print("CPI - Ljung-Box (diff1): Ausencia de correlación")
}
#AUSENCIA DE CORRELACION

train_ipc_estacionaria <- cpi_diff1
#saveRDS(train_ipc_estacionaria, file = "IPC_Train_ts_ESTACIONARIA.rds")


#-----------------------------------------------------------------
# Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparación)
#png("CPI_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))
plot(train_ipc, type="l", main="CPI (log) - Serie original", ylab="Nivel (log)")
plot(train_ipc_estacionaria, type="l", main="CPI (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  
#dev.off()

#-------    ACF Y PACF
#png("ACF_CPI_Primera_Diferencia.png", width=800, height=600)
ggAcf(train_ipc_estacionaria) + ggtitle("ACF de IPC (Primera diferencia)") + theme_minimal()
title(main = "ACF de CPI (Primera Diferencia)")
#dev.off() 

#png("PACF_CPI_Primera_Diferencia.png", width=800, height=600)
ggPacf(train_ipc_estacionaria) + ggtitle("PACF de IPC (Primera diferencia)") + theme_minimal()
#dev.off()   

#-------   QQ-PLOT 
#png("QQ_cpi.png", width=800, height=600)
qqnorm(train_ipc_estacionaria, main="QQ-plot - CPI (log, diff1)")
qqline(train_ipc_estacionaria, col="red")
#dev.off()



########### STOCK MARKET INDEX (log) (TRIMESTRAL) #####################
#######################################################################

#---- SIN DIFERENCIA
tsdisplay(stock_market_ts_trimestral_sin_outliers_log)             

#---- PRIMERA DIFERENCIA
# Aplicar diferencia
stock_market_diff1 <- diff(stock_market_ts_trimestral_sin_outliers_log, differences = 1)
tsdisplay(stock_market_diff1)

#---- TESTS DE ESTACIONARIEDAD
# TEST ADF
adf_test_stock_market_1 <- adf.test(stock_market_diff1)
if(adf_test_stock_market_1$p.value < 0.05){
  print("Stock Market - ADF (diff1): estacionaria")
} else{
  print("Stock Market - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=1

# TEST KPSS
kpss_test_stock_market_1 <- kpss.test(stock_market_diff1, null="Level")
if(kpss_test_stock_market_1$p.value < 0.05){
  print("Stock Market - KPSS (diff1): NO estacionaria")
} else{
  print("Stock Market - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

# TEST LJUNG-BOX
LBtest_stock_market_1 <- Box.test(stock_market_diff1, lag = 10, type="Ljung")
if (LBtest_stock_market_1$p.value < 0.05) {
  print("Stock Market - Ljung-Box (diff1): Existe correlación")
} else {
  print("Stock Market - Ljung-Box (diff1): Ausencia de correlación")
}
#AUSENCIA DE CORRELACION

# Los tests indican que la serie es estacionaria con la primera diferencia.
stock_market_ts_trimestral_sin_outliers_log_estacionaria <- stock_market_diff1
#saveRDS(stock_market_ts_trimestral_sin_outliers_log_estacionaria, file = "Stock_Market_ts_ESTACIONARIA.rds")


#-----------------------------------------------------------------
# Graficamos
#-----------------------------------------------------------------
#------- SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparación)
#png("Stock_Market_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))
plot(stock_market_ts_trimestral_sin_outliers_log, type="l", main="Stock Market Index (log) - Serie original", ylab="Nivel (log)")
plot(stock_market_ts_trimestral_sin_outliers_log_estacionaria, type="l", main="Stock Market Index (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1)) 
#dev.off()

#------- ACF Y PACF
#png("ACF_Stock_Market_Primera_Diferencia.png", width=800, height=600)
ggAcf(stock_market_ts_trimestral_sin_outliers_log_estacionaria) + ggtitle("ACF de Stock Market (Primera diferencia)") + theme_minimal()
#dev.off() 

#png("PACF_Stock_Market_Primera_Diferencia.png", width=800, height=600)
ggPacf(stock_market_ts_trimestral_sin_outliers_log_estacionaria) + ggtitle("PACF de Stock Market (Primera diferencia)") + theme_minimal()
#dev.off()   

#------- QQ-PLOT 
#png("QQ_stock_market.png", width=800, height=600)
qqnorm(stock_market_ts_trimestral_sin_outliers_log_estacionaria, main="QQ-plot - Stock Market (log, diff1)")
qqline(stock_market_ts_trimestral_sin_outliers_log_estacionaria, col="red")
#dev.off()





# Crear un data.frame resumen
resumen_series <- data.frame(
  Variable = c("Money Supply", "Unemployment Rate", "GDP", "CPI", "Stock Market"),
  Tipo_Serie = c("Trimestral", "Trimestral", "Trimestral", "Trimestral", "Trimestral"),
  Log_Transform = c("No", "No", "Sí", "No", "Sí"),
  Diferencia = c("Segunda", "Segunda", "Segunda", "Primera", "Primera"),
  Lag = c("No", "Sí (lag=4)", "Sí (lag=4)", "No", "No"),
  Estacionaria = c("Sí", "Sí", "Sí", "Sí", "Sí")
)

resumen_series



