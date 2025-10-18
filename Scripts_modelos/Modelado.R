# MODELADO

#==========================
#Cargar librerias necesaria:
#==========================
library(readxl)
library(dplyr)
library(tidyr)
library(naniar)
library(forecast)
library(zoo)
library(ggplot2)
library(VIM)
#library(tsoutliers)
library(tseries)
library(gridExtra)
library(astsa)
library(Metrics)




#=========================
#Cargar datos:
#=========================
datos_limpios_AUS<- read.csv("DATOS/limpios/Datos_Limpios_Australia.csv")

#Analizar los datos
str(datos_limpios_AUS)
summary(datos_limpios_AUS)
dim(datos_limpios_AUS)
head(datos_limpios_AUS)
colnames(datos_limpios_AUS)

paleta_colores <-  c(
  "#84BD00",  # Verde pistacho
  "#D1006F",  # Magenta
  "#6A2C8A",  # Berenjena
  "#D1D3D4"   # Gris neutro
  )


#Las variables PCI y GDP las tenemos en trimestrales ya en los datos Originales, en cmabio Unemployment, money supply y Stock Market estan en mensuales.
#Por lo que las pasaremos a series trimestrals las tres que estan en mensual, ya que trabajaremos con datos trimestrales para hacer la prediccion.

# =========================
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



# =========================
# 2. GRAFICAR SERIES MENSUALES Y TRIMESTRALES 
# =========================

# ---- SERIES MENSUALES ----
#--------------------------------------------------------------------------
#png("graficos_series_mensuales.png", width=800, height=1200)

par(mfrow=c(3,1), mar=c(4,4,2,1))  # 3 filas, 1 columna
plot(unemployment_ts_mensual, type="l", col="blue", lwd=2,
     main="Tasa de Desempleo Mensual - Australia",
     ylab="%", xlab="Año")

plot(money_supply_ts_mensual, type="l", col="darkgreen", lwd=2,
     main="Money Supply Mensual - Australia",
     ylab="Billion Currency Units", xlab="Año")

plot(stock_market_ts_mensual, type="l", col="red", lwd=2,
     main="Stock Market Index Mensual - Australia",
     ylab="Index", xlab="Año")

par(mfrow=c(1,1))
#dev.off()



# ---- SERIES TRIMESTRALES ----
#--------------------------------------------------------------------------
#png("graficos_series_trimestrales.png", width=800, height=1200)

par(mfrow=c(5,1), mar=c(4,4,2,1))  # 5 filas, 1 columna

# Unemployment trimestral
plot(unemployment_ts_trimestral, type="l", col="blue", lwd=2,
     main="Tasa de Desempleo Trimestral - Australia",
     ylab="%", xlab="Año")

# Money supply trimestral
plot(money_supply_ts_trimestral, type="l", col="darkgreen", lwd=2,
     main="Money Supply Trimestral - Australia",
     ylab="Billion Currency Units", xlab="Año")

# Stock market trimestral
plot(stock_market_ts_trimestral, type="l", col="red", lwd=2,
     main="Stock Market Index Trimestral - Australia",
     ylab="Index", xlab="Año")

# CPI trimestral
plot(cpi_ts_trimestral, type="l", col="purple", lwd=2,
     main="CPI Trimestral - Australia",
     ylab="Index", xlab="Año")

# GDP trimestral
plot(gdp_ts_trimestral, type="l", col="orange", lwd=2,
     main="GDP Trimestral - Australia",
     ylab="Billion Currency Units", xlab="Año")

par(mfrow=c(1,1))
#dev.off()



# =========================
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
plot_outliers <- function(original_ts, clean_ts, titulo){
    outs <- tsoutliers(original_ts)
  par(mfrow=c(2,1), mar=c(4,4,2,1))
  
  plot(original_ts, type="l", lwd=2, col="blue",
       main=paste(titulo, "- Outliers marcados"), 
       ylab="Valor", xlab="Año",
       ylim=c(min(original_ts, na.rm=TRUE), max(original_ts, na.rm=TRUE)*1.1))
  
  if(length(outs$index) > 0){
    points(time(original_ts)[outs$index],
           original_ts[outs$index],
           col="red", pch=19, cex=1.2)
  }
  
  legend(x = max(time(original_ts), na.rm=TRUE), 
         y = max(original_ts, na.rm=TRUE)*1.05,  # 5% por encima
         legend = c("Serie original","Outliers"),
         col = c("blue","red"), lwd=c(2,NA), pch=c(NA,19),
         bty="n", xjust=1, yjust=1)
  
  
  # Diferencia Original vs Limpio
  diff_ts <- original_ts - clean_ts
  plot(diff_ts, type="h", lwd=2, col="purple",
       main=paste(titulo, "- Diferencia Original vs Sin Outliers"),
       ylab="Corrección aplicada", xlab="Año")
  abline(h=0, col="gray")
  
  par(mfrow=c(1,1))
}


#png("outliers_unemployment.png", width=800, height=800)
plot_outliers(unemployment_ts_trimestral, unemployment_ts_trimestral_sin_outliers, "Unemployment Rate (Trimestral)")
#dev.off()

#png("outliers_money_supply.png", width=800, height=800)
plot_outliers(money_supply_ts_trimestral, money_supply_ts_trimestral_sin_outliers, "Money Supply (Trimestral)")
#dev.off()

#png("outliers_stock_market.png", width=800, height=800)
plot_outliers(stock_market_ts_trimestral, stock_market_ts_trimestral_sin_outliers, "Stock Market Index (Trimestral)")
#dev.off()

#png("outliers_cpi.png", width=800, height=800)
plot_outliers(cpi_ts_trimestral, cpi_ts_trimestral_sin_outliers, "CPI (Trimestral)")
#dev.off()

#png("outliers_gdp.png", width=800, height=800)
plot_outliers(gdp_ts_trimestral, gdp_ts_trimestral_sin_outliers, "GDP (Trimestral)")
#dev.off()



#======================
# 4. SEPARAR TRAIN / TEST
#======================
#Queremos predecir datos que ya tenemos para evaluar los modelos
#Separar datos en train y test

train_ipc<-window(cpi_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2020,4))
test_ipc<-window(cpi_ts_trimestral_sin_outliers,start=c(2021,1), end= c(2022,2))

train_pib<-window(gdp_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2020,4))
test_pib<-window(gdp_ts_trimestral_sin_outliers,start=c(2021,1), end= c(2022,2))



#======================
# 5. TIENEN VARIANZA?
#======================

# GDP
#png("gdp_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(train_pib, col="steelblue", lwd=2,
       main="GDP (Sin outliers)  ", ylab="Valor", xlab="Tiempo")
#dev.off()
# Varianza creciente (ligera), con estacionalidad

# CPI
#png("cpi_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(train_ipc, col="steelblue", lwd=2,
       main="CPI (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Constante

# Stock Market
#png("stock_market_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(stock_market_ts_trimestral_sin_outliers, col="steelblue", lwd=2,
       main="Stock Market (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Creciente

# Money Supply
#png("money_supply_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(money_supply_ts_trimestral_sin_outliers, col="steelblue", lwd=2,
       main="Money Supply (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza constante

# Unemployment
#png("unemployment_Serie_Sin_Outliers.png", width=800, height=600)
tsplot(unemployment_ts_trimestral_sin_outliers, col="steelblue", lwd=2,
       main="Unemployment (Sin Outliers)", ylab="Valor", xlab="Tiempo")
#dev.off()
#Varianza Constante



# ESTABILIZAR VARIANZA
# -------------------------

# Transformación log para estabilizar la varianza
stock_market_ts_trimestral_sin_outliers_log <- log(stock_market_ts_trimestral_sin_outliers)
train_pib_log <- log(train_pib)

# Graficar después de log-transform
#png("stock_market_Serie_Estabilizada_log.png", width=800, height=600)
tsplot(stock_market_ts_trimestral_sin_outliers_log, col="darkgreen", lwd=2,
       main="Stock Market - Serie log-transformada", ylab="Log-Valor", xlab="Tiempo")
#dev.off()

#png("gdp_Serie_Estabilizada_log.png", width=800, height=600)
tsplot(train_pib_log, col="darkgreen", lwd=2,
       main="GDP - Serie log-transformada", ylab="Log-Valor", xlab="Tiempo")
#dev.off()


#=======================
# 6. DESCOPOSICION DE LA SERIE
#=======================
#?? No puedo poner el titulo que yo quiero
# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(train_ipc)
plot(descomposicion_ipc, col="#93044e")     # genera el gráfico con el título por defecto
mtext("Descomposición del IPC", side = 3, line = 0.5, cex = 1.2, font = 2)
descomposicion_ipc$seasonal #estacionalidad
descomposicion_ipc$trend #tendencia
random_ipc<-descomposicion_ipc$random #residuo

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(train_pib_log)
plot(descomposicion_pib, col= "#93044e")
descomposicion_pib$seasonal #estacionalidad
descomposicion_pib$trend #tendencia
random_pib<-descomposicion_pib$random #residuo

# Descomposición de la serie de masa monetaria
descomposicion_money_supply <- decompose(money_supply_ts_trimestral_sin_outliers)
plot(descomposicion_money_supply, col= "#93044e")
descomposicion_money_supply$seasonal #estacionalidad
descomposicion_money_supply$trend #tendencia
random_money_supply<-descomposicion_money_supply$random #residuo

# Descomposición de la serie de indice bursatil
descomposicion_stock_market <- decompose(stock_market_ts_trimestral_sin_outliers_log)
plot(descomposicion_stock_market, col= "#93044e")
descomposicion_stock_market$seasonal #estacionalidad
descomposicion_stock_market$trend #tendencia
random_stock_market<-descomposicion_stock_market$random #residuo

# Descomposición de la serie del paro
descomposicion_unemployment <- decompose(unemployment_ts_trimestral_sin_outliers)
plot(descomposicion_unemployment, col= "#93044e")
descomposicion_unemployment$seasonal #estacionalidad
descomposicion_unemployment$trend #tendencia
random_unemployment<-descomposicion_unemployment$random #residuo



#======================
# 7. ANALIZAR ESTACIONALIDAD
#======================
#Comprobar estacionalidad apra ver si meter lag o no
nsdiffs(train_pib_log) #SI
nsdiffs(train_ipc) #NO
nsdiffs(money_supply_ts_trimestral_sin_outliers) #NO
nsdiffs(stock_market_ts_trimestral_sin_outliers_log) #NO
nsdiffs(unemployment_ts_trimestral_sin_outliers) #SI



#========================
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
plot(t, train_pib_log, type="l", col="steelblue", lwd=2,
     main="PIB con Tendencia",
     ylab="Log(PIB)", xlab="Tiempo")
abline(modelo_tendencia1, col="red", lwd=2)
legend("topleft", legend=c("Serie log", "Tendencia"), col=c("steelblue","red"), lwd=2)

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



#=======================
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






#=======================
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

#Se analiza si la serie tiene estacionalidad.
decomposed <- decompose(money_supply_ts_trimestral_sin_outliers, type="multiplicative")
plot(decomposed)
decomposed$seasonal
#Si tiene porque los mismos valores se repiten cada 4 trimestres. Es por ello que haremos la primera diferncia con lag= 4

#---            PRIMERA DIFERRENCIA Y LAG                -----

acf(money_supply_ts_trimestral_sin_outliers)
#COn lag dos quitamos la estacionalidad de la serie
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

#Siuge sin dar por lo que aplicaremos la segunda diferencia

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
tsdisplay(train_pib_log)  

#----               PRIMERA DIFERENCIA y lag

# Aplicar diferencia
gdp_diff1 <- diff(train_pib_log, differences = 1, lag=4)
tsdisplay(gdp_diff1)

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
gdp_diff2 <- diff(diff(train_pib_log, lag=4))
tsdisplay(gdp_diff2)


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


#-----------------------------------------------------------------
# Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparación)
#png("GDP_Original_vs_Diferenciada.png, width = 900, height = 700)
par(mfrow=c(2,1))
plot(train_pib_log, type="l", main="GDP (log) - Serie original", ylab="Nivel (log)")
plot(train_gdp_estacionaria, type="l", main="GDP (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1)) 
#dev.off()

#-------    ACF Y PACF
#png("ACF_GDP_Segunda_Diferencia.png", width=800, height=600)
ggAcf(train_gdp_estacionaria) + ggtitle("ACF de PIB (Segunda diferencia)") + theme_minimal()
#dev.off() 

#png("PACF_GDP_Segunda_Diferencia.png", width=800, height=600)
ggPacf(train_gdp_estacionaria) + ggtitle("PACF de PIB (Segunda diferencia)") + theme_minimal()
#dev.off() 

#-------   QQ-PLOT
#png("QQ_gdp.png", width=800, height=600)
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


#auto.arima, sarima, , cuando le metes la variables exogenas es arimax.



############################################################
# 11. AJUSTE DE MODELOS + PREDICCIONES
############################################################

#........................................ IPC ..................................
#--------------------------------------------------------------------------------


#-----------------      AUTOARIMA ---- IPC        ----------------------
###########################################################################
#-------  MODELO
#Como la serie ya es estacionaria d=0. SI no lo fuese usariamos d=1.

#--Modelo
modelo_autoarima_ipc <- auto.arima(train_ipc_estacionaria, seasonal=FALSE, d=0) #D=0 para que no vuelva a diferenciar
summary(modelo_autoarima_ipc)

# Justificación:
# - d=0 → la serie ya fue diferenciada manualmente.
# - d=1 → solo si se observa tendencia residual o mejora notable de AIC.
#Es normal que las predicciones sean iguales: es la consecuencia lógica de una serie sin tendencia.


#Validacion grafica de residaules:
hist(residuals(modelo_autoarima_ipc), main="Histograma de residuales", xlab="Residual", col="lightblue")
checkresiduals(modelo_autoarima_ipc)

#Test de Ljung–Box 
boxtest_autoarima_ipc <- Box.test(residuals(modelo_autoarima_ipc), lag = round(log(length(train_ipc))), type = "Ljung-Box")
if (boxtest_autoarima_ipc$p.value > 0.05) {
  cat("Residuos parecen ruido blanco\n")
} else {
  cat("Residuos muestran autocorrelación\n")
}
#--> Residuos parecen ruido blanco

#-------  PREDICCIONES
prediccion_autoarima_ipc <- forecast(modelo_autoarima_ipc, h=length(test_ipc), level=90)
autoplot(prediccion_autoarima_ipc) + ggtitle("Predicción IPC con AutoARIMA") + ylab("IPC") + xlab("Trimestre") + theme_minimal()


#-------  REVERTIR 
#La serie IPC fue diferenciada 1 vez --> revertimos con diffinv()

forecast_autoarima_ipc_revertida <- diffinv(prediccion_autoarima_ipc$mean,
                                            differences = 1,
                                            xi = tail(train_ipc, 1))
forecast_autoarima_ipc_revertida<- forecast_autoarima_ipc_revertida[-1]

#-------  ACCURACY
acc_autoarima<- forecast::accuracy(forecast_autoarima_ipc_revertida,test_ipc)
acc_autoarima

#-------  GRAFICAR (Train vs Test vs Prediccion)
# Crear la serie temporal de predicciones con la frecuencia adecuada
forecast_autoarima_ipc_revertida_ts <- ts(
  forecast_autoarima_ipc_revertida,
  start = c(2021, 1),   # <-- ajusta al inicio real de tu test
  frequency = 4)       # trimestral

ts.plot(train_ipc,test_ipc,forecast_autoarima_ipc_revertida_ts,
        col = c("black", "blue", "red"),
        lty = c(1, 1, 2),
        lwd = c(2, 2, 3),  # ← controla el grosor de cada línea
        main = "Predicción IPC AUTOARIMA vs Observado",
        ylab = "IPC")
legend("topleft",
       legend = c("Entrenamiento", "Observado (Test)", "Predicción"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 3))



#-----------------      ARIMA MANUAL ---- IPC        ----------------------
###########################################################################
# train_ipc_estacionaria es la serie ya diferenciada (d=1)

# Ver ACF y PACF para decidir p y q
#Elegiriremos p y q fijandolnos en la autoccorelacion:
#p=orden autoregresivo → se decide con la PACF.--> p ≈ 1 (PACF corta rápido después de lag 1)
#q= orden de media móvil → se decide con la ACF. -_> q ≈ 0 (ACF corta rápido después de lag 0)
acf(train_ipc_estacionaria, main="ACF IPC diferenciada")
pacf(train_ipc_estacionaria, main="PACF IPC diferenciada")


#----------   MODELOS
modelo_arima_ipc <- arima(train_ipc_estacionaria, order=c(1,0,0))  # d=0 porque ya diferenciamos anteriormente. y no necesitamos diferenciarlo otra vez
summary(modelo_arima_ipc)

# Validación de residuales
hist(residuals(modelo_arima_ipc), main="Histograma de residuales ARIMA IPC", xlab="Residual", col="lightblue")
checkresiduals(modelo_arima_ipc)

boxtest_arima_ipc <- Box.test(residuals(modelo_arima_ipc), lag = round(log(length(train_ipc))), type="Ljung-Box")
if (boxtest_arima_ipc$p.value > 0.05) {
  cat("Residuos ARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}

#-----------------      PREDICCIONES
prediccion_arima_ipc <- forecast(modelo_arima_ipc, h=length(test_ipc), level=90)
autoplot(prediccion_arima_ipc) + ggtitle("Predicción IPC con ARIMA") + ylab("IPC") + xlab("Trimestre") + theme_minimal()

#-----------------      REVERTIR
# La serie original fue diferenciada 1 vez, usamos diffinv
forecast_arima_ipc_revertida <- diffinv(prediccion_arima_ipc$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arima_ipc_revertida <- forecast_arima_ipc_revertida[-1]

#-----------------      ACCURACY
acc_arima <- forecast::accuracy(forecast_arima_ipc_revertida, test_ipc)
acc_arima

#-----------------      GRAFICO FINAL
forecast_arima_ipc_revertida_ts <- ts(forecast_arima_ipc_revertida, start=c(2021,1), frequency=4)

ts.plot(train_ipc, test_ipc, forecast_arima_ipc_revertida_ts,
        col=c("black","blue","red"),
        lty=c(1,1,2),
        lwd=c(2,2,3),
        main="Predicción IPC ARIMA vs Observado",
        ylab="IPC")
legend("topleft",
       legend=c("Entrenamiento","Observado (Test)","Predicción"),
       col=c("black","blue","red"),
       lty=c(1,1,2),
       lwd=c(2,2,3))

#-----------------      SARIMA MANUAL ---- IPC----------------------
###########################################################################

#p= 0: No hay picos importantes en PACF → sin componente AR
#q= 1: ACF tiene ligera autocorrelación inicial → componente MA(1) captura el efecto
#d= 0 (ya esta la serie estacionaria no hace falta diferenciarla mas)

#P= 0: No hay picos en múltiplos de 4 → sin patrón estacional autoregresivo
#Q= 0: No hay correlaciones estacionales significativas
#D= 0: Ya eliminaste diferencias estacionales

#Elegir los mejores parametros:
acf(train_ipc_estacionaria)
pacf(train_ipc_estacionaria)
tsdisplay(train_ipc_estacionaria)


#-----------------      MODELO SARIMA IPC
modelo_sarima_ipc <- arima(train_ipc_estacionaria,
                           order = c(0, 0, 1),
                           seasonal = list(order = c(0, 0, 0), period = 4),
                           method = "ML")
summary(modelo_sarima_ipc)
checkresiduals(modelo_sarima_ipc)

#-----------------      PREDICCIONES
prediccion_sarima_ipc <- forecast(modelo_sarima_ipc, h=length(test_ipc), level=90)
autoplot(prediccion_sarima_ipc) + 
  ggtitle("Predicción IPC con SARIMA") + 
  ylab("IPC") + xlab("Trimestre") + 
  theme_minimal()

#-----------------      REVERTIR
# La serie original fue diferenciada 1 vez, usamos diffinv
forecast_sarima_ipc_revertida <- diffinv(prediccion_sarima_ipc$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_sarima_ipc_revertida <- forecast_sarima_ipc_revertida[-1]

#-----------------      ACCURACY
acc_sarima_ipc <- forecast::accuracy(forecast_sarima_ipc_revertida, 
                                     test_ipc)
acc_sarima_ipc

#-----------------      GRAFICO FINAL
forecast_sarima_ipc_revertida_ts <- ts(forecast_sarima_ipc_revertida, 
                                       start=c(2021,1), frequency=4)

ts.plot(train_ipc, test_ipc, forecast_sarima_ipc_revertida_ts,
        col=c("black","blue","red"),
        lty=c(1,1,2),
        lwd=c(2,2,3),
        main="Predicción IPC SARIMA vs Observado",
        ylab="IPC")
legend("topleft",
       legend=c("Entrenamiento","Observado (Test)","Predicción"),
       col=c("black","blue","red"),
       lty=c(1,1,2),
       lwd=c(2,2,3))



#================
#COMPARACION IPC MODELOS (AUTOARIMA, SARIMA, ARIMA)
#================

#TABLA MODELOS IPC
accuracy_tabla_IPC <- data.frame(
  Modelo = c("AutoARIMA", "Arima", "SARIMA"),
  ME   = c(acc_autoarima["Test set","ME"],
           acc_arima["Test set", "ME"],
           acc_sarima_ipc["Test set", "ME"]),
  RMSE = c(acc_autoarima["Test set","RMSE"],
           acc_arima["Test set", "RMSE"],
           acc_sarima_ipc["Test set", "RMSE"]),
  MAE  = c(acc_autoarima["Test set","MAE"],
           acc_arima["Test set", "MAE"],
           acc_sarima_ipc["Test set", "MAE"]),
  MAPE = c(acc_autoarima["Test set","MAPE"],
           acc_arima["Test set", "MAPE"],
           acc_sarima_ipc["Test set", "MAPE"])
)

accuracy_tabla_IPC

accuracy_tabla_IPC_ordenada <- accuracy_tabla_IPC[order(accuracy_tabla_IPC$RMSE), ]
accuracy_tabla_IPC_ordenada 
# Imprimir el mejor modelo
cat("El mejor modelo según RMSE es:", accuracy_tabla_IPC_ordenada$Modelo[1], "\n")
cat("Con RMSE =", accuracy_tabla_IPC_ordenada$RMSE[1], "\n")
#AUTOARIMA

#tscv--> validacion cruzada (diapo 141) 



#GRAFICO MODELOS IPC
# Convertir la tabla a formato largo para ggplot
accuracy_long <- accuracy_tabla_IPC %>%pivot_longer(cols = c(ME, RMSE, MAE, MAPE), names_to = "Métrica", values_to = "Valor")

# Gráfico
ggplot(accuracy_long, aes(x = Modelo, y = Valor, fill = Modelo)) + geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Métrica, scales = "free_y") +  # cada métrica en un panel
  theme_minimal() +
  ylab("Valor") +
  ggtitle("Comparación de Accuracy: Modelos de IPC") +
  theme(legend.position = "none") +
  geom_text(aes(label=round(Valor,2)), vjust=-0.3, size=3)



#........................................ PIB ..................................
#--------------------------------------------------------------------------------


#-----------------      AUTOARIMA ---- PIB        ----------------------
###########################################################################
#-------  MODELO
modelo_autoarima_pib <- auto.arima(train_gdp_estacionaria, seasonal = FALSE, d = 0)
summary(modelo_autoarima_pib)

# Validación gráfica de residuales
hist(residuals(modelo_autoarima_pib), main = "Histograma de residuales AutoARIMA PIB", xlab = "Residual", col = "lightblue")
checkresiduals(modelo_autoarima_pib)

# Test de Ljung-Box
boxtest_autoarima_pib <- Box.test(residuals(modelo_autoarima_pib), lag = round(log(length(train_pib))), type = "Ljung-Box")
if (boxtest_autoarima_pib$p.value > 0.05) {
  cat("Residuos AutoARIMA PIB parecen ruido blanco\n")
} else {
  cat("Residuos AutoARIMA PIB muestran autocorrelación\n")
}

#-------  PREDICCIONES
prediccion_autoarima_pib <- forecast(modelo_autoarima_pib, h = length(test_pib), level = 90)
autoplot(prediccion_autoarima_pib) + ggtitle("Predicción PIB con AutoARIMA") + ylab("PIB") + xlab("Trimestre") + theme_minimal()


#-----------------      REVERTIR
# Revertir la segunda diferencia (la no estacional)
forecast_tmp_autoarima<- diffinv(prediccion_autoarima_pib$mean, 
                              differences = 1, 
                              xi = tail(diff(train_gdp_log, lag = 4), 1))

# Revertir la primera diferencia (la estacional con lag = 4)
forecast_autoarima_pib_revertida <- diffinv(forecast_tmp_autoarima, 
                                        lag = 4, 
                                        differences = 1, 
                                        xi = tail(train_gdp_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_autoarima_pib_revertida <- forecast_autoarima_pib_revertida[-c(1:5)]

# Deshacer el logaritmo
forecast_autoarima_pib_revertida <- exp(forecast_autoarima_pib_revertida)


#-------  ACCURACY
acc_autoarima_pib <- forecast::accuracy(forecast_autoarima_pib_revertida, test_pib)
acc_autoarima_pib

#-------  GRAFICAR (Train vs Test vs Prediccion)
# Crear la serie temporal de predicciones con la frecuencia adecuada
forecast_autoarima_pib_revertida_ts <- ts(
  forecast_autoarima_pib_revertida,
  start = c(2021, 1),   # <-- ajusta al inicio real de tu test
  frequency = 4)       # trimestral

ts.plot(train_pib,test_pib,forecast_autoarima_pib_revertida_ts,
        col = c("black", "blue", "red"),
        lty = c(1, 1, 2),
        lwd = c(2, 2, 3),  # ← controla el grosor de cada línea
        main = "Predicción PIB AUTOARIMA vs Observado",
        ylab = "PIB")
legend("topleft",
       legend = c("Entrenamiento", "Observado (Test)", "Predicción"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 3))



#-----------------      ARIMA MANUAL ---- PIB       ----------------------
###########################################################################
# train_ipc_estacionaria es la serie ya diferenciada (d=1)

# 1. Ver ACF y PACF para decidir p y q
#Elegiriremos p y q fijandolnos en la autoccorelacion:
#p=orden autoregresivo → se decide con la PACF.--> p ≈ 1 (PACF corta rápido después de lag 1)
#q= orden de media móvil → se decide con la ACF. -_> q ≈ 1 (ACF corta rápido después de lag 1)
acf(train_gdp_estacionaria, main="ACF PIB diferenciada")
pacf(train_gdp_estacionaria, main="PACF PIB diferenciada")
tsdisplay(train_gdp_estacionaria)
#ROSA: con el acf y pacf me dice que (1,0,1) pero es que el accuracy me da mejor con (2,0,2)

#----------   MODELOS
modelo_arima_pib <- arima(train_gdp_estacionaria, order=c(2,0,2))  # d=0 porque ya diferenciamos
summary(modelo_arima_pib)

# Validación de residuales
hist(residuals(modelo_arima_pib), main="Histograma de residuales ARIMA PIB", xlab="Residual", col="lightblue")
checkresiduals(modelo_arima_pib)

boxtest_arima_pib <- Box.test(residuals(modelo_arima_pib), lag = round(log(length(train_pib))), type="Ljung-Box")
if (boxtest_arima_pib$p.value > 0.05) {
  cat("Residuos ARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}

#-----------------      PREDICCIONES
prediccion_arima_pib <- forecast(modelo_arima_pib , h=length(test_pib), level=90)
autoplot(prediccion_arima_pib) + ggtitle("Predicción PIB con ARIMA") + ylab("PIB") + xlab("Trimestre") + theme_minimal()

#-----------------      REVERTIR
# Revertir la segunda diferencia (la no estacional)
forecast_tmp <- diffinv(prediccion_arima_pib$mean, 
                        differences = 1, 
                        xi = tail(diff(train_gdp_log, lag = 4), 1))

# Revertir la primera diferencia (la estacional con lag = 4)
forecast_arima_pib_revertida <- diffinv(forecast_tmp, 
                                        lag = 4, 
                                        differences = 1, 
                                        xi = tail(train_gdp_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_arima_pib_revertida <- forecast_arima_pib_revertida[-c(1:5)]

# Deshacer el logaritmo
forecast_arima_pib_revertida <- exp(forecast_arima_pib_revertida)


#-----------------      ACCURACY
acc_arima_pib <- forecast::accuracy(forecast_arima_pib_revertida, test_pib)
acc_arima_pib


#-----------------      GRAFICO FINAL
forecast_arima_pib_revertida_ts <- ts(forecast_arima_pib_revertida, start=c(2021,1), frequency=4)

ts.plot(train_pib, test_pib, forecast_arima_pib_revertida_ts,
        col=c("black","blue","red"),
        lty=c(1,1,2),
        lwd=c(2,2,3),
        main="Predicción PIB ARIMA vs Observado",
        ylab="IPC")
legend("topleft",
       legend=c("Entrenamiento","Observado (Test)","Predicción"),
       col=c("black","blue","red"),
       lty=c(1,1,2),
       lwd=c(2,2,3))



#-----------------      SARIMA MANUAL ---- PIB       ----------------------
###########################################################################

#q=1 --> En lag 1, hay un pico fuerte negativo
#Q=0 --> No hay un pico fuerte estacionla (en los lag 4,8...)

#p=1 --> El PACF corta bruscamente despues de lag 1
#P=0 No hay un pico grande en lag =4

tsdisplay(train_gdp_estacionaria)
#----------   MODELO
modelo_sarima_pib<- arima(train_gdp_estacionaria,
                       order = c(2, 0, 2),
                       seasonal = list(order = c(0, 0, 0), period = 4),
                       method = "ML")
summary(modelo_sarima_pib)
checkresiduals(modelo_sarima_pib)

#-----------------      VALIDACIÓN DE RESIDUALES
hist(residuals(modelo_sarima_pib), main="Histograma de residuales SARIMA PIB", 
     xlab="Residual", col="lightblue")
checkresiduals(modelo_sarima_pib)

boxtest_sarima_pib <- Box.test(residuals(modelo_sarima_pib), 
                               lag = round(log(length(train_pib))), 
                               type="Ljung-Box")
if (boxtest_sarima_pib$p.value > 0.05) {
  cat("Residuos SARIMA PIB parecen ruido blanco\n")
} else {
  cat("Residuos SARIMA PIB muestran autocorrelación\n")
}

#-----------------      PREDICCIONES
prediccion_sarima_pib <- forecast(modelo_sarima_pib, h=length(test_pib), level=90)
autoplot(prediccion_sarima_pib) + 
  ggtitle("Predicción PIB con SARIMA") + 
  ylab("PIB") + xlab("Trimestre") + 
  theme_minimal()

#-----------------      REVERTIR
# Revertir la segunda diferencia (no estacional)
forecast_tmp <- diffinv(prediccion_sarima_pib$mean, 
                        differences = 1, 
                        xi = tail(diff(train_gdp_log, lag = 4), 1))

# Revertir la primera diferencia (estacional con lag = 4)
forecast_sarima_pib_revertida <- diffinv(forecast_tmp, 
                                         lag = 4, 
                                         differences = 1, 
                                         xi = tail(train_gdp_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_sarima_pib_revertida <- forecast_sarima_pib_revertida[-c(1:5)]

# Deshacer el logaritmo
forecast_sarima_pib_revertida <- exp(forecast_sarima_pib_revertida)

#-----------------      ACCURACY
acc_sarima_pib <- forecast::accuracy(forecast_sarima_pib_revertida, 
                                     test_pib)
acc_sarima_pib

#-----------------      GRAFICO FINAL
forecast_sarima_pib_revertida_ts <- ts(forecast_sarima_pib_revertida, 
                                       start=c(2021,1), frequency=4)

ts.plot(train_pib, test_pib, forecast_sarima_pib_revertida_ts,
        col=c("black","blue","red"),
        lty=c(1,1,2),
        lwd=c(2,2,3),
        main="Predicción PIB SARIMA vs Observado",
        ylab="PIB")
legend("topleft",
       legend=c("Entrenamiento","Observado (Test)","Predicción"),
       col=c("black","blue","red"),
       lty=c(1,1,2),
       lwd=c(2,2,3))

# ME     RMSE      MAE      MPE     MAPE
# Test set 15.895 29.29282 23.80709 2.768286 4.286727


#================
# COMPARACIÓN MODELOS PIB
#================
accuracy_table_pib <- data.frame(
  Modelo = c("AutoARIMA", "Arima", "SARIMA"),
  ME   = c(acc_autoarima_pib["Test set","ME"],
           acc_arima_pib["Test set", "ME"],
           acc_sarima_pib["Test set", "ME"]),
  RMSE = c(acc_autoarima_pib["Test set","RMSE"],
           acc_arima_pib["Test set", "RMSE"],
           acc_sarima_pib["Test set", "RMSE"]),
  MAE  = c(acc_autoarima_pib["Test set","MAE"],
           acc_arima_pib["Test set", "MAE"],
           acc_sarima_pib["Test set", "MAE"]),
  MAPE = c(acc_autoarima_pib["Test set","MAPE"],
           acc_arima_pib["Test set", "MAPE"],
           acc_sarima_pib["Test set", "MAPE"])
)


accuracy_table_pib
#Tomar en ceunta el ACF1 tambien, frist order autoccorrleation coefficient error.

# Ordenar la tabla por RMSE
accuracy_table_pib_ordenada <- accuracy_table_pib[order(accuracy_table_pib$RMSE), ]
accuracy_table_pib_ordenada

# Imprimir el mejor modelo
cat("El mejor modelo para PIB según RMSE es:", accuracy_table_pib_ordenada$Modelo[1], "\n")
cat("Con RMSE =", accuracy_table_pib_ordenada$RMSE[1], "\n")
#SARIMA


# Gráfico comparativo
accuracy_long_pib <- accuracy_table_pib %>%
  pivot_longer(cols = c(ME, RMSE, MAE, MAPE), names_to = "Métrica", values_to = "Valor")

ggplot(accuracy_long_pib, aes(x = Modelo, y = Valor, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Métrica, scales = "free_y") +
  theme_minimal() +
  ylab("Valor") +
  ggtitle("Comparación de Accuracy: Modelos de PIB") +
  theme(legend.position = "none") +
  geom_text(aes(label=round(Valor,2)), vjust=-0.3, size=3)





#======================
#CROSS VALIDATION 
#======================

############################     IPC     #############################
#autoarima
length(train_ipc_estacionaria)

#Funcion de forecast para tsCV
funcion_autoarima_ipc <- function(y, h) {
  # Ajusta ARIMA sin diferenciar de nuevo (d=0)
  forecast(auto.arima(y, d=0), h = h)
}

#Cross validation temporal
tsCV_IPC_autoarima <- tsCV(
  y = train_ipc_estacionaria,
  forecastfunction = funcion_autoarima_ipc,
  h = 3)

#Calcular RMSE promedio del CV
rmse_autoarima_cv <- sqrt(mean(tsCV_IPC_autoarima^2, na.rm = TRUE))
cat("RMSE cross-validation (AutoARIMA - IPC):", rmse_autoarima_cv, "\n")


#arima
funcion_arima_ipc <- function(y, h) {
  # Ajusta ARIMA sin diferenciar de nuevo (d=0)
  forecast(auto.arima(y, d=0), h = h)
}

tsCV_IPC_autoarima <- tsCV(
  y = train_ipc_estacionaria,
  forecastfunction = funcion_autoarima_ipc,
  h = 3
)

#sacar la rpeddcion


#accrucay








##########################
library(forecast)

#-----------------------------
# 1️⃣ Función segura para tsCV
#-----------------------------
funcion_autoarima_ipc <- function(y, h) {
  # Intentamos ajustar auto.arima y hacer forecast
  fc <- tryCatch({
    forecast(auto.arima(y, d = 0, seasonal = FALSE), h = h)$mean
  }, error = function(e) {
    rep(NA, h)  # Si falla, devolvemos NA de largo h
  })
  return(as.numeric(fc))
}

#-----------------------------
# 2️⃣ Cross-validation
#-----------------------------
horizonte <- 3
tsCV_IPC_autoarima <- tsCV(
  y = train_ipc_estacionaria,
  forecastfunction = funcion_autoarima_ipc,
  h = horizonte
)

#-----------------------------
# 3️⃣ RMSE global
#-----------------------------
rmse_total <- sqrt(mean(tsCV_IPC_autoarima^2, na.rm = TRUE))
cat("RMSE promedio:", rmse_total, "\n")

#-----------------------------
# 4️⃣ MSE por horizonte
#-----------------------------
mse_horizonte <- colMeans(tsCV_IPC_autoarima^2, na.rm = TRUE)
mse_horizonte_df <- data.frame(Horizonte = 1:horizonte, MSE = mse_horizonte)
print(mse_horizonte_df)

#-----------------------------
# 5️⃣ Gráfico MSE por horizonte
#-----------------------------
plot(mse_horizonte_df$Horizonte, mse_horizonte_df$MSE, type="b", pch=16, col="red",
     xlab="Horizonte de predicción", ylab="MSE",
     main="MSE de Cross-Validation por horizonte (AutoARIMA - IPC)")
grid()



























############################################################
# 5. GRAFICOS DE PREDICCIONES vs OBSERVADO
############################################################

extraer_valores <- function(pred){
  if("forecast" %in% class(pred)){
    return(as.numeric(pred$mean))
  } else {
    return(as.numeric(pred))  # ya es vector numérico
  }
}

graficar_predicciones <- function(test, pred_list, nombres, titulo){
  df <- data.frame(Tiempo = time(test), Observado = as.numeric(test))
  
  for(i in 1:length(pred_list)){
    df[[nombres[i]]] <- extraer_valores(pred_list[[i]])
  }
  
  library(tidyr)
  df_long <- pivot_longer(df, cols = -Tiempo, names_to = "Modelo", values_to = "Valor")
  
  ggplot(df_long, aes(x = Tiempo, y = Valor, color = Modelo)) +
    geom_line(size=1) +
    scale_color_manual(values = c("Observado"="black",
                                  "AutoARIMA"="blue",
                                  "Naive"="green",
                                  "SNaive"="red",
                                  "Mean"="purple",
                                  "MA"="orange",
                                  "AR"="brown",
                                  "ARMA"="pink")) +
    ggtitle(titulo) +
    ylab("Valor") +
    xlab("Trimestre") +
    theme_minimal()
}
# Ejemplo IPC
graphics.off()  # cierra todos los dispositivos gráficos

graficar_predicciones(test_ipc, 
                      list(prediccion_autoarima_ipc, prediccion_naive_ipc, prediccion_snaive_ipc),
                      c("AutoARIMA", "Naive", "SNaive"),
                      "Predicciones vs Observado - IPC")

# Ejemplo PIB
graficar_predicciones(test_pib, 
                      list(prediccion_autoarima_pib, prediccion_naive_pib, prediccion_snaive_pib),
                      c("AutoARIMA", "Naive", "SNaive"),
                      "Predicciones vs Observado - PIB")








# Después de evaluar qué modelo funciona mejor:
#   
#   Reviertes las diferencias y transformaciones (log, diferencias estacionales) para que las predicciones estén en la escala original.
# 
# Esto no es para encontrar otro modelo, sino para interpretar los resultados y calcular accuracy en la escala real:
#   
#   Ejemplo: predicción de PIB en miles de millones, CPI en índice real.
# 
# Ahora puedes comparar con los valores reales de test y graficar predicciones vs observados.Después de evaluar qué modelo funciona mejor:
#   
#   Reviertes las diferencias y transformaciones (log, diferencias estacionales) para que las predicciones estén en la escala original.
# 
# Esto no es para encontrar otro modelo, sino para interpretar los resultados y calcular accuracy en la escala real:
#   
#   Ejemplo: predicción de PIB en miles de millones, CPI en índice real.
# 
# Ahora puedes comparar con los valores reales de test y graficar predicciones vs observados.




# Series estacionarias → elegir mejor modelo → predecir test estacionario.
# 
# Revertir predicciones a escala original → evaluar error y graficar.
# 
# Usar modelo elegido para predecir futuros valores en la escala original.

############################################################
# 6. REVERTIR TRANSFORMACIONES PARA INTERPRETAR EN ESCALA ORIGINAL
############################################################
############################################################
# 6. REVERTIR TRANSFORMACIONES PARA INTERPRETAR EN ESCALA ORIGINAL
############################################################

##############################
# CPI: diferencia simple + estacional
##############################

# Tomar el último valor antes de la primera diferencia
ultimo_valor_diff_cpi <- tail(cpi_ts_trimestral_sin_outliers, 1)  

# Tomar los últimos 4 valores antes de la diferencia estacional (lag=4, trimestral)
ultimos_4_valores_cpi <- tail(cpi_ts_trimestral_sin_outliers, 4) 

# Revertir la diferencia estacional de las predicciones del modelo
pred_cpi_diff_est <- diffinv(pred_autoarima_ipc$mean, lag = 4, xi = ultimos_4_valores_cpi)

# Revertir la primera diferencia simple (lag=1)
pred_cpi_original <- diffinv(pred_cpi_diff_est, lag = 1, xi = ultimo_valor_diff_cpi)

# Ajustar la longitud final para que coincida con la serie de test
pred_cpi_original <- tail(pred_cpi_original, length(test_ipc))

# Extraer la serie original de test del CPI en la misma ventana temporal
CPI_test_ori <- window(cpi_ts_trimestral_sin_outliers, start=start(test_ipc), end=end(test_ipc))

# Calcular métricas de error (RMSE, MAE, MAPE, etc.) entre la predicción y los valores reales
accuracy_CPI <- accuracy(pred_cpi_original, CPI_test_ori)

# Mostrar resultados de accuracy
print(accuracy_CPI)


##############################
# PIB: log + primera diferencia
##############################

# Tomar el último valor de la serie log-transformada antes de diferenciar
ultimo_valor_diff_pib <- tail(gdp_ts_trimestral_sin_outliers_log,1)

# Revertir la primera diferencia de las predicciones sobre la serie log-transformada
pred_pib_diff <- diffinv(pred_autoarima_pib$mean, lag=1, xi = ultimo_valor_diff_pib)

# Revertir la transformación logarítmica para volver a la escala original
pred_pib_original <- exp(pred_pib_diff)

# Ajustar longitud final para que coincida con la serie de test
pred_pib_original <- tail(pred_pib_original, length(test_pib))

# Extraer la serie original de test del PIB en la misma ventana temporal
PIB_test_ori <- window(gdp_ts_trimestral_sin_outliers, start=start(test_pib), end=end(test_pib))

# Calcular métricas de error entre la predicción revertida y los valores reales
accuracy_PIB <- accuracy(pred_pib_original, PIB_test_ori)

# Mostrar resultados de accuracy
print(accuracy_PIB)

############################################################
# Ahora 'pred_cpi_original' y 'pred_pib_original' están en la
# escala original, listos para graficar y evaluar
############################################################


#Train / Test
#Convertir a estarcionaria solo el train
#Hacer modelos y forecast para cada variable.
#Revertir el forecast (Tendria que coincidir con test) (Diapo 81-82). Tene ne cuenta el si es primer valor o tener en cuenta el ultimo valor--> #para reveritr hay que darle el valor orginal no el de la difernecia.
#Outoplot serie original y las prediciones.
#Accuracy (Decidir el mejor)






#CAMBIAR LOS GRAFICOS DE LA VRIANZA CON ST.PLOT

#DEPENDE DE COMO DE ESTAICONARIA SEA HAY QUE APLICAR UN MODELO O OTRO DE ARIMA
#MIRNAOD LA SERIE DESPUES DE LAS DIFERENCIAS VER COMO DE ESTACIOANRIA ES Y APLICAR UN MODELO U OTRO. CON AUTO.ARIMA
# CUANDO HACES EL MODELO TE VA QUITANDO AL ESTACIONALIDAD TMAB. CHECK-REISUDALS MIRAR DESPUES DE MODELARLO.

#HACER MODELOS DE IPC Y GDP EN BASE A ELLAS MISMAS. PARA CADA UNA DE ELLAS UN MODELO.
#USAR SIMEPR EEL TS.DISPLAY PARA HACER LOS GRAFICOS.
#HACER UN GRAFICO SIN APLICAR DIFERENICA Y MIRAR EL GRAFICO
#LUEGO HACER UNA DIFERENCIA =1 Y LAG=4 Y HACER EL GRAFICO Y MIRAR SI YA ES ESTACIONARIA EN CASO DE QUE NO SEA HACER SEFUNDA DIFERENCIA DIFF(DIFF) 
#USAR LA DIFERENCIA QUE COICIDAN LSO DOS TESES.
#HACER LA TRAMPA DE MIRAR EN LOS TESES CUANTAS DIFERENCIAS TE DICE Y EN BASE A ESO HACER EL TS.DISPKAY LAS VECES QUE DIGAN CUNATAS DIFERENCIAS APLICAR. PERO EL TS.DISPLAY HAY QUE HACERLO ANTES DE APLCIAR LOS TESES PORQUE SE SUPONE QUE HAY QUE DECIDIR SI ES ESTACIONARIA MIRNADO LOS GRAFICOS DE TS.DISPLAY. 
#ENTONCES HAREMOS LOS TESES PRIMERO PARA VER CUANTAS DIFERENCIAS Y LEUGO  HACER LOS TS.DISPLAYS PERO LEUGO CMABAIREMOS EL ORDEN PARA DAR A ENTENDER QUE PRIMERO HEMOS ECHO EL TS.DISPLAY Y LUEGO HEMOS COMPROBADO CON LOS TESES.

#MODELOS:





#Luego ANalizaremos correlacion y relacion
#modelado de series tmeporales
#reocnstruccion de escala original

datos_limpios_AUS <- 