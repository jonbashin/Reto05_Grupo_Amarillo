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

paleta_colores <- paleta_colores <- c(
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
# 6. GRAFICAR SERIES MENSUALES Y TRIMESTRALES 
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
# 9. DETECCIÓN Y LIMPIEZA DE OUTLIERS
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
# SEPARAR TRAIN / TEST
#======================
#Queremos predecir datos que ya tenemos para evaluar los modelos
#Separar datos en train y test

train_ipc<-window(cpi_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2021,2))
test_ipc<-window(cpi_ts_trimestral_sin_outliers,start=c(2021,3), end= c(2022,2))

train_pib<-window(gdp_ts_trimestral_sin_outliers,start= c(1998,1),end=c(2021,2))
test_pib<-window(gdp_ts_trimestral_sin_outliers,start=c(2021,2), end= c(2022,2))



#======================
#11. TIENEN VARIANZA?
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
#DESCOPOSICION DE LA SERIE
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
#ANALIZAR ESTACIONALIDAD
#======================
#Comprobar estacionalidad apra ver si meter lag o no
nsdiffs(train_pib_log) #SI
nsdiffs(train_ipc) #NO
nsdiffs(money_supply_ts_trimestral_sin_outliers) #NO
nsdiffs(stock_market_ts_trimestral_sin_outliers_log) #NO
nsdiffs(unemployment_ts_trimestral_sin_outliers) #SI


#=======================
#ANALISIS DE AUTOCORRELACION
#=======================
# ACF y PACF
train_ipc <- na.omit(train_ipc)
train_pib_log <- na.omit(train_pib_log)
stock_market_ts_trimestral_sin_outliers_log <- na.omit(stock_market_ts_trimestral_sin_outliers_log)
money_supply_ts_trimestral_sin_outliers <- na.omit(money_supply_ts_trimestral_sin_outliers)
unemployment_ts_trimestral_sin_outliers <- na.omit(unemployment_ts_trimestral_sin_outliers)

# ---- IPC ----
#png("ACF_IPC_Sin_Diferencia.png", width=800, height=600)
acf(train_ipc)
title(main = "ACF del IPC (Sin Diferencia)")
#dev.off()

#png("PACF_IPC_Sin_Diferencia.png", width=800, height=600)
pacf(train_ipc)
title(main = "PACF del IPC (Sin Diferencia)")
#dev.off()

# ---- Money Supply ----
#png("ACF_Money_Supply_Sin_Diferencia.png", width=800, height=600)
acf(money_supply_ts_trimestral_sin_outliers)
title(main = "ACF de Money Supply (Sin Diferencia)")
#dev.off()

#png("PACF_Money_Supply_Sin_Diferencia.png", width=800, height=600)
pacf(money_supply_ts_trimestral_sin_outliers)
title(main = "PACF de Money Supply (Sin Diferencia)")
#dev.off()

# ---- GDP ----
#png("ACF_GDP_Sin_Diferencia.png", width=800, height=600)
acf(train_pib_log)
title(main = "ACF del GDP (Log, Sin Diferencia)")
#dev.off()

#png("PACF_GDP_Sin_Diferencia.png", width=800, height=600)
pacf(train_pib_log)
title(main = "PACF del GDP (Log, Sin Diferencia)")
#dev.off()

# ---- Stock Market ----
#png("ACF_Stock_Market_Sin_Diferencia.png", width=800, height=600)
acf(stock_market_ts_trimestral_sin_outliers_log)
title(main = "ACF del Stock Market (Log, Sin Diferencia)")
#dev.off()

#png("PACF_Stock_Market_Sin_Diferencia.png", width=800, height=600)
pacf(stock_market_ts_trimestral_sin_outliers_log)
title(main = "PACF del Stock Market (Log, Sin Diferencia)")
#dev.off()

# ---- Unemployment Rate ----
#png("ACF_Unemployment_Sin_Diferencia.png", width=800, height=600)
acf(unemployment_ts_trimestral_sin_outliers)
title(main = "ACF del Unemployment Rate (Sin Diferencia)")
#dev.off()

#png("PACF_Unemployment_Sin_Diferencia.png", width=800, height=600)
pacf(unemployment_ts_trimestral_sin_outliers)
title(main = "PACF del Unemployment Rate (Sin Diferencia)")
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
LBtest_money_supply_1_lag <- Box.test(money_supply_diff1_lag1, lag = 10, type="Ljung")
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
acf(money_supply_ts_trimestral_sin_outliers_estacionaria)
title(main = "ACF de Money Supply (Segunda Diferencia)")
#dev.off()

#png("PACF_Money_Supply_Segunda_Diferencia.png", width=800, height=600)
pacf(money_supply_ts_trimestral_sin_outliers_estacionaria)
title(main = "PACF de Money Supply (Segunda Diferencia)")
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
plot(unemployment_ts_trimestral_sin_outliers_estacionaria, type="l", main="Unemployment Rate (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  
#dev.off()

#-------    ACF Y PACF
#png("ACF_Unemployment_Primera_Diferencia.png", width=800, height=600)
acf(unemployment_ts_trimestral_sin_outliers_estacionaria)
title(main = "ACF de Unemployment (Primera Diferencia)")
#dev.off() 

#png("PACF_Unemployment_Primera_Diferencia.png", width=800, height=600)
pacf(unemployment_ts_trimestral_sin_outliers_estacionaria)
title(main = "PACF de Unemployment (Primera Diferencia)")
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
#png("ACF_GDP_Primera_Diferencia.png", width=800, height=600)
acf(train_gdp_estacionaria)
title(main = "ACF de GDP (Primera Diferencia)")
#dev.off() 

#png("PACF_GDP_Primera_Diferencia.png", width=800, height=600)
pacf(train_gdp_estacionaria)
title(main = "PACF de GDP (Primera Diferencia)")
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


train_cpi_estacionaria <- cpi_diff1


#-----------------------------------------------------------------
# Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparación)
#png("CPI_Original_vs_Diferenciada.png", width = 900, height = 700)
par(mfrow=c(2,1))
plot(train_ipc, type="l", main="CPI (log) - Serie original", ylab="Nivel (log)")
plot(train_cpi_estacionaria, type="l", main="CPI (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  
#dev.off()

#-------    ACF Y PACF
#png("ACF_CPI_Primera_Diferencia.png", width=800, height=600)
acf(train_cpi_estacionaria)
title(main = "ACF de CPI (Primera Diferencia)")
#dev.off() 

#png("PACF_CPI_Primera_Diferencia.png", width=800, height=600)
pacf(train_cpi_estacionaria)
title(main = "PACF de CPI (Primera Diferencia)")
#dev.off()   

#-------   QQ-PLOT 
#png("QQ_cpi.png", width=800, height=600)
qqnorm(train_cpi_estacionaria, main="QQ-plot - CPI (log, diff1)")
qqline(train_cpi_estacionaria, col="red")
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
acf(stock_market_ts_trimestral_sin_outliers_log_estacionaria)
title(main = "ACF de Stock Market (Primera Diferencia)")
#dev.off() 

#png("PACF_Stock_Market_Primera_Diferencia.png", width=800, height=600)
pacf(stock_market_ts_trimestral_sin_outliers_log_estacionaria)
title(main = "PACF de Stock Market (Primera Diferencia)")
#dev.off()   

#------- QQ-PLOT 
#png("QQ_stock_market.png", width=800, height=600)
qqnorm(stock_market_ts_trimestral_sin_outliers_log_estacionaria, main="QQ-plot - Stock Market (log, diff1)")
qqline(stock_market_ts_trimestral_sin_outliers_log_estacionaria, col="red")
#dev.off()



#==================
#TRAIN - TEST
#==================
library(forecast)
library(TSA)       # Para AR, ARMA
library(Metrics)
library(ggplot2)
library(dplyr)

#auto.arima, sarima, , cuando le metes la variables exogenas es arimax.



############################################################
# 2. AJUSTE DE MODELOS
############################################################

#---  Auto ARIMA
### IPC
#?????????? pongo el train de ipc estacionaria o sin estacionaria?
modelo_autoarima_ipc <- auto.arima(train_cpi_estacionaria, seasonal=FALSE, d=0, D=0) #D=0 para que no vuelva a diferenciar
#d=1 --> Aunque el test ndiffs() indicó que no era necesaria una diferencia (d = 0), la presencia de una tendencia visual clara, el patrón de autocorrelación, y la mejora del modelo ARIMA al usar d = 1 (según AIC) justifican plenamente la elección de d = 1 como valor más adecuado para el modelado.
#d=0 --> Quédate con el modelo con d = 0 si tu serie (train_cpi_estacionaria) ya está estacionaria.
         #Es normal que las predicciones sean iguales: es la consecuencia lógica de una serie sin tendencia.
summary(modelo_autoarima_ipc)
checkresiduals(modelo_autoarima_ipc)
#Validacion grafica de residaules:
hist(residuals(modelo_autoarima_ipc), main="Histograma de residuales", xlab="Residual", col="lightblue")
# QUE LAG PONER??????????????????????????????????????????
boxtest_autoarima_ipc <- Box.test(residuals(modelo_autoarima_ipc), lag = round(log(length(train_ipc))), type = "Ljung-Box")
if (boxtest_autoarima_ipc$p.value > 0.05) {
  cat("Residuos parecen ruido blanco\n")
} else {
  cat("Residuos muestran autocorrelación\n")
}
#--> Residuos parecen ruido blanco


### PIB
modelo_autoarima_pib <- auto.arima(train_gdp_estacionaria, seasonal = FALSE, d=0, D=0)
summary(modelo_autoarima_pib)
checkresiduals(modelo_autoarima_pib)
# Validacion garfica de residuales
hist(residuals(modelo_autoarima_pib), 
     main = "Histograma de residuales del PIB", 
     xlab = "Residual", 
     col = "lightblue")

boxtest_autoarima_pib <- Box.test(residuals(modelo_autoarima_pib), lag = round(log(length(train_pib))), type = "Ljung-Box")
if (boxtest_autoarima_pib$p.value > 0.05) {
  cat("Residuos parecen ruido blanco\n")
} else {
  cat("Residuos muestran autocorrelación\n")
}
#--> Residuos muestran autocorrelación



#--- Modelo Naive 
### IPC
modelo_naive_ipc <- naive(train_cpi_estacionaria, h=length(test_ipc))
summary(modelo_naive_ipc)
checkresiduals(modelo_naive_ipc)
# Validación gráfica de residuales
hist(residuals(modelo_naive_ipc), main="Histograma de residuales Naive IPC", xlab="Residual", col="lightgreen")
# Test Ljung-Box
boxtest_naive_ipc <- Box.test(residuals(modelo_naive_ipc), lag = round(log(length(train_ipc))), type = "Ljung-Box")
if (boxtest_naive_ipc$p.value > 0.05) {
  cat("Residuos Naive IPC parecen ruido blanco\n")
} else {
  cat("Residuos Naive IPC muestran autocorrelación\n")
}
#--> Residuos Naive IPC parecen ruido blanco


### PIB
modelo_naive_pib <- naive(train_gdp_estacionaria, h=length(test_pib))
summary(modelo_naive_pib)
checkresiduals(modelo_naive_pib)
# Validación gráfica de residuales
hist(residuals(modelo_naive_pib), main="Histograma de residuales Naive PIB", xlab="Residual", col="lightgreen")
# Test Ljung-Box
boxtest_naive_pib <- Box.test(residuals(modelo_naive_pib), lag = round(log(length(train_pib))), type = "Ljung-Box")
if (boxtest_naive_pib$p.value > 0.05) {
  cat("Residuos Naive PIB parecen ruido blanco\n")
} else {
  cat("Residuos Naive PIB muestran autocorrelación\n")
}
#--> Residuos Naive PIB muestran autocorrelación




#--- Modelos SNaive
### IPC
modelo_snaive_ipc <- snaive(train_cpi_estacionaria, h=length(test_ipc))
summary(modelo_snaive_ipc)
checkresiduals(modelo_snaive_ipc)
# Validación gráfica de residuales
hist(residuals(modelo_snaive_ipc), main="Histograma de residuales SNaive IPC", xlab="Residual", col="lightcoral")
# Test Ljung-Box
boxtest_snaive_ipc <- Box.test(residuals(modelo_snaive_ipc), lag = round(log(length(train_ipc))), type = "Ljung-Box")
if (boxtest_snaive_ipc$p.value > 0.05) {
  cat("Residuos SNaive IPC parecen ruido blanco\n")
} else {
  cat("Residuos SNaive IPC muestran autocorrelación\n")
}
#--> Residuos SNaive IPC muestran autocorrelación


### PIB
modelo_snaive_pib <- snaive(train_gdp_estacionaria, h=length(test_pib))
summary(modelo_snaive_pib)
checkresiduals(modelo_snaive_pib)
# Validación gráfica de residuales
hist(residuals(modelo_snaive_pib), main="Histograma de residuales SNaive PIB", xlab="Residual", col="lightcoral")
# Test Ljung-Box
boxtest_snaive_pib <- Box.test(residuals(modelo_snaive_pib), lag = round(log(length(train_pib))), type = "Ljung-Box")
if (boxtest_snaive_pib$p.value > 0.05) {
  cat("Residuos SNaive PIB parecen ruido blanco\n")
} else {
  cat("Residuos SNaive PIB muestran autocorrelación\n")
}
#--> Residuos SNaive PIB muestran autocorrelación




#--- Modelo Sarima (Como el autoarima pero seasonal=T)

############################################################
#---  Modelo SARIMA
############################################################

### IPC
# En este caso, utilizamos el modelo SARIMA permitiendo estacionalidad.
# Si tu serie ya está estacionaria (por diferencias aplicadas previamente),
# puedes fijar d=0 y D=0 para que no vuelva a diferenciar.

modelo_sarima_ipc <- auto.arima(train_cpi_estacionaria, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, d = 0, D = 0, trace = TRUE)
summary(modelo_sarima_ipc)
checkresiduals(modelo_sarima_ipc)
hist(residuals(modelo_sarima_ipc), 
     main = "Histograma de residuales SARIMA IPC", 
     xlab = "Residual", 
     col = "lightblue")

# Test de Ljung-Box para autocorrelación
boxtest_sarima_ipc <- Box.test(residuals(modelo_sarima_ipc), 
                               lag = round(log(length(train_ipc))), 
                               type = "Ljung-Box")

if (boxtest_sarima_ipc$p.value > 0.05) {
  cat("Residuos SARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos SARIMA IPC muestran autocorrelación\n")
}
#Residuos SARIMA IPC parecen ruido blanco

### PIB
modelo_sarima_pib <- auto.arima(train_gdp_estacionaria, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, d = 0, D = 0, trace = TRUE)
summary(modelo_sarima_pib)
checkresiduals(modelo_sarima_pib)
hist(residuals(modelo_sarima_pib), 
     main = "Histograma de residuales SARIMA PIB", 
     xlab = "Residual", 
     col = "lightblue")

# Test de Ljung-Box
boxtest_sarima_pib <- Box.test(residuals(modelo_sarima_pib), 
                               lag = round(log(length(train_pib))), 
                               type = "Ljung-Box")

if (boxtest_sarima_pib$p.value > 0.05) {
  cat("Residuos SARIMA PIB parecen ruido blanco\n")
} else {
  cat("Residuos SARIMA PIB muestran autocorrelación\n")
}
#Residuos SARIMA PIB parecen ruido blanco







############################################################
# 3. PREDICCIONES PARA TEST
############################################################

#--- IPC

prediccion_autoarima_ipc <- forecast(modelo_autoarima_ipc, h=length(test_ipc), level=90)
autoplot(prediccion_autoarima_ipc) + ggtitle("Predicción IPC con AutoARIMA") + ylab("IPC") + xlab("Trimestre") + theme_minimal()
# prediccion_autoarima_ipc
# Point Forecast      Lo 90    Hi 90
# 2021 Q3       0.544086 -0.1565169 1.244689
# 2021 Q4       0.544086 -0.1565169 1.244689
# 2022 Q1       0.544086 -0.1565169 1.244689
# 2022 Q2       0.544086 -0.1565169 1.244689

prediccion_naive_ipc    <- forecast(modelo_naive_ipc, h=length(test_ipc))
autoplot(prediccion_naive_ipc) + ggtitle("Predicción IPC con Naive") + ylab("IPC") + xlab("Trimestre") + theme_minimal()
# prediccion_naive_ipc
# Point Forecast       Lo 80    Hi 80      Lo 95    Hi 95
# 2021 Q3              1  0.26173200 1.738268 -0.1290835 2.129083
# 2021 Q4              1 -0.04406862 2.044069 -0.5967652 2.596765
# 2022 Q1              1 -0.27871769 2.278718 -0.9556299 2.955630
# 2022 Q2              1 -0.47653600 2.476536 -1.2581670 3.258167

prediccion_snaive_ipc   <- forecast(modelo_snaive_ipc, h=length(test_ipc))
autoplot(prediccion_snaive_ipc) + ggtitle("Predicción IPC con Snaive") + ylab("IPC") + xlab("Trimestre") + theme_minimal()
# prediccion_snaive_ipc
# Point Forecast      Lo 80     Hi 80      Lo 95     Hi 95
# 2021 Q3      0.4000000 -0.3375369 1.1375369 -0.7279653 1.5279653
# 2021 Q4     -0.2278992 -0.9654361 0.5096376 -1.3558645 0.9000661
# 2022 Q1     -0.1721008 -0.9096376 0.5654361 -1.3000661 0.9558645
# 2022 Q2      1.0000000  0.2624631 1.7375369 -0.1279653 2.1279653


prediccion_sarima_ipc<- forecast(modelo_sarima_ipc, h=length(test_ipc))
autoplot(prediccion_sarima_ipc) + ggtitle("Predicción IPC con Sarima") + ylab("IPC") + xlab("Trimestre") + theme_minimal()
# Point Forecast        Lo 80    Hi 80      Lo 95    Hi 95
# 2021 Q3       0.544086 -0.001773388 1.089945 -0.2907339 1.378906
# 2021 Q4       0.544086 -0.001773388 1.089945 -0.2907339 1.378906
# 2022 Q1       0.544086 -0.001773388 1.089945 -0.2907339 1.378906
# 2022 Q2       0.544086 -0.001773388 1.089945 -0.2907339 1.378906

#--- PIB
prediccion_autoarima_pib <- forecast(modelo_autoarima_pib, h=length(test_pib), level=90)
#Explicacion de todo 0-s--> “El modelo ARIMA(0,0,0) con media cero aplicado sobre la serie estacionaria del PIB muestra que, una vez eliminada la tendencia y la estacionalidad, la serie no presenta estructura temporal significativa. Esto indica que el proceso se comporta como ruido blanco y, por tanto, las variaciones futuras son esencialmente aleatorias alrededor de cero.”
autoplot(prediccion_autoarima_pib) +  ggtitle("Predicción PIB con AutoARIMA") + ylab("PIB") +  xlab("Trimestre") +  theme_minimal()
# Point Forecast       Lo 90      Hi 90
# 2021 Q3              0 -0.02159978 0.02159978
# 2021 Q4              0 -0.02159978 0.02159978
# 2022 Q1              0 -0.02159978 0.02159978
# 2022 Q2              0 -0.02159978 0.02159978
# 2022 Q3              0 -0.02159978 0.02159978

prediccion_naive_pib    <- forecast(modelo_naive_pib, h=length(test_pib))
autoplot(prediccion_naive_pib) +  ggtitle("Predicción PIB con Naive") + ylab("PIB") +  xlab("Trimestre") +  theme_minimal()
# Point Forecast        Lo 80      Hi 80       Lo 95      Hi 95
# 2021 Q3     0.01320375 -0.008048719 0.03445621 -0.01929909 0.04570659
# 2021 Q4     0.01320375 -0.016851778 0.04325927 -0.03276221 0.05916971
# 2022 Q1     0.01320375 -0.023606603 0.05001410 -0.04309283 0.06950032
# 2022 Q2     0.01320375 -0.029301184 0.05570868 -0.05180194 0.07820943
# 2022 Q3     0.01320375 -0.034318211 0.06072571 -0.05947482 0.08588231

prediccion_snaive_pib   <- forecast(modelo_snaive_pib, h=length(test_pib))
autoplot(prediccion_snaive_pib) +  ggtitle("Predicción PIB con Snaive") + ylab("PIB") +  xlab("Trimestre") +  theme_minimal()
# Point Forecast       Lo 80      Hi 80       Lo 95      Hi 95
# 2021 Q3    0.004319198 -0.02492196 0.03356035 -0.04040129 0.04903968
# 2021 Q4   -0.006270263 -0.03551142 0.02297089 -0.05099075 0.03845022
# 2022 Q1    0.015338444 -0.01390271 0.04457960 -0.02938204 0.06005893
# 2022 Q2    0.013203747 -0.01603741 0.04244490 -0.03151674 0.05792423
# 2022 Q3    0.004319198 -0.03703404 0.04567243 -0.05892512 0.06756352

prediccion_sarima_pib<- forecast(modelo_sarima_pib, h=length(test_pib))
autoplot(prediccion_sarima_pib) + ggtitle("Predicción PIB con Sarima") + ylab("PIB") + xlab("Trimestre") + theme_minimal()

# Point Forecast       Lo 80       Hi 80       Lo 95      Hi 95
# 2021 Q3  -0.0038760277 -0.01805072 0.010298668 -0.02555435 0.01780230
# 2021 Q4   0.0012436709 -0.01293103 0.015418367 -0.02043466 0.02292200
# 2022 Q1  -0.0068303535 -0.02100505 0.007344343 -0.02850868 0.01484797
# 2022 Q2  -0.0003635284 -0.01453822 0.013811168 -0.02204186 0.02131480
# 2022 Q3   0.0000000000 -0.01683396 0.016833960 -0.02574532 0.02574532

############################################################
# 4. CALCULO DE METRICAS
############################################################


calcular_metricas <- function(test, pred, modelo){
  # si es objeto forecast, tomar $mean, si no, usar directamente
  if("forecast" %in% class(pred)) {
    pred_vals <- as.numeric(pred$mean)
  } else {
    pred_vals <- as.numeric(pred)
  }
  
  data.frame(
    Modelo = modelo,
    RMSE = rmse(test, pred_vals),
    MAE  = mae(test, pred_vals),
    MAPE = mape(test, pred_vals)
  )
}

#============================
# IPC
tabla_ipc <- rbind(
  calcular_metricas(test_ipc, prediccion_autoarima_ipc, "AutoARIMA"),
  calcular_metricas(test_ipc, prediccion_naive_ipc, "Naive"),
  calcular_metricas(test_ipc, prediccion_snaive_ipc, "SNaive"))

#============================
# PIB
tabla_pib <- rbind(
  calcular_metricas(test_pib, prediccion_autoarima_pib, "AutoARIMA"),
  calcular_metricas(test_pib, prediccion_naive_pib, "Naive"),
  calcular_metricas(test_pib, prediccion_snaive_pib, "SNaive"))

# Mostrar tablas
tabla_ipc
tabla_pib



#Elegir el mejor modelo
library(dplyr)
mejor_modelo_ipc <- tabla_ipc %>% arrange(RMSE) %>% slice(1)
mejor_modelo_pib <- tabla_pib %>% arrange(RMSE) %>% slice(1)
mejor_modelo_ipc #NAIVE
mejor_modelo_pib #NAIVE


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


#=========================
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


#===============================
#REVERTIR SERIES
#==============================

#REVERSION --> GDP (APlicado: Segunda diferencia.)
#..........................................................................
#REVERSION AUTOARIMA

#Extraer valores previos:
ultimo_valor_original_pib <- tail(train_pib_log, 1)
penultimo_valor_original_pib <- tail(train_pib_log, 2)[1]

#Revertir la segunda diferencia:
#---- Autoarima
pred_revertida_log_autoarima_pib <- diffinv(
  prediccion_autoarima_pib$mean,          # Predicciones diferenciadas
  differences = 2,                         # Segunda diferencia
  xi = c(penultimo_valor_original_pib, ultimo_valor_original_pib)  # Valores previos log
)

#---- Naive
pred_revertida_log_naive_pib <- diffinv(
  prediccion_naive_pib$mean,
  differences = 2,
  xi = c(penultimo_valor_original_pib, ultimo_valor_original_pib)
)

#----- SNaive
pred_revertida_log_snaive_pib <- diffinv(
  prediccion_snaive_pib$mean,
  differences = 2,
  xi = c(penultimo_valor_original_pib, ultimo_valor_original_pib)
)

#Ajustar longitud:
pred_revertida_log_autoarima_pib <- pred_revertida_log_autoarima_pib[(length(pred_revertida_log_autoarima_pib) - length(test_pib) + 1):length(pred_revertida_log_autoarima_pib)]
pred_revertida_log_naive_pib <- pred_revertida_log_naive_pib[(length(pred_revertida_log_naive_pib) - length(test_pib) + 1) : length(pred_revertida_log_naive_pib)]
pred_revertida_log_snaive_pib <- pred_revertida_log_snaive_pib[(length(pred_revertida_log_snaive_pib) - length(test_pib) + 1) : length(pred_revertida_log_snaive_pib)]


#Reveritr logartimo (log) (Varianza):
prediccion_revertida_autoarima_pib <- exp(pred_revertida_log_autoarima_pib)
prediccion_revertida_naive_pib       <- exp(pred_revertida_log_naive_pib)
prediccion_revertida_snaive_pib      <- exp(pred_revertida_log_snaive_pib)

#Graficar predicciones revertidas: 
plot(test_pib, col="black", type="l", main="Predicciones revertidas (AutoARIMA, Naive, SNaive)", ylab="PIB")
lines(prediccion_revertida_autoarima_pib, col="red")
lines(prediccion_revertida_naive_pib, col="blue")
lines(prediccion_revertida_snaive_pib, col="green")
legend("topleft", legend=c("AutoARIMA", "Naive", "SNaive"), col=c("red","blue","green"), lty=1)


#Calcular accuracy
# --- 8. Calcular accuracy ---
# PIB_test_unidades_ori debe estar en la escala original
PIB_test_unidades_ori <- window(
  gdp_ts_trimestral_sin_outliers,
  start = c(2021, 3),
  end   = c(2022, 2)
)

# Calcular accuracy
accuracy_PIB_autoarima <- accuracy(prediccion_revertida_autoarima_pib, PIB_test_unidades_ori)
accuracy_PIB_naive      <- accuracy(prediccion_revertida_naive_pib,    PIB_test_unidades_ori)
accuracy_PIB_snaive     <- accuracy(prediccion_revertida_snaive_pib,   PIB_test_unidades_ori)

# Mostrar resultados
print("Accuracy AutoARIMA:")
print(accuracy_PIB_autoarima)

print("Accuracy Naive:")
print(accuracy_PIB_naive)

print("Accuracy SNaive:")
print(accuracy_PIB_snaive)





#REVERSION --> IPC (Aplicado: Primera Diferencia)




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