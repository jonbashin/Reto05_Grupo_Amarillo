#Limpieza iraia

#Limpieza de datos

# =========================
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
#library(tsoutliers)
library(tseries)
library(gridExtra)

# =========================
# 1. CARGAR DATOS
# =========================
exogenos <- read_excel("DATOS/originales/exogenas_paises_punto2.xlsx")
pib_ipc <- read.csv("DATOS/originales/pib_ipc_paises_punto2.csv")

# =========================
# 2. FILTRAR SOLO AUSTRALIA
# =========================
exogenos_AUS <- exogenos %>% filter(Country == "Australia")
pib_ipc_AUS <- pib_ipc %>% filter(Country == "Australia")

# =========================
# 3. UNIR LOS DOS DATA FRAMES
# =========================
df_final <- exogenos_AUS %>%
  left_join(pib_ipc_AUS, by = c("Country", "Code", "ContinentCode", "Year", "Month"))

# =========================
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


# =========================
# 5. PASAR A SERIES TEMPORALES TRIMESTRALES
# =========================

# ---- VARIABLES MENSUALES A TS ---- 
#Tenemos datos trimestrales para PCI y GDP
#Convertir a ts mensual 
unemployment_ts_mensual<- ts(as.numeric(df_final$`Unemployment rate percent`), start= c(1996,1), frequency=12)
money_supply_ts_mensual<- ts(as.numeric(df_final$`Money supply billion currency units`), start= c(1996,1), frequency=12)
stock_market_ts_mensual<- ts(as.numeric(df_final$`Stock market index`), start= c(1996,1), frequency=12)
#CPI y GDP son trimestrales por lo que no se pude sacar el mensual

# ---- VARIABLES TRIMESTRALES (CPI y GDP)----
# CPI y GDP ya son trimestrales. Pasaremos a series temporales pero necesitaremos eliminar los NA-s
#-->CPI
# Tomar solo los valores numéricos (sin NAs iniciales)
cpi_vals <- na.omit(as.numeric(df_final$Consumer.Price.Index..CPI.))
# Crear serie trimestral empezando en el primer año y trimestre válido
cpi_ts_trimestral <- ts(cpi_vals, start=c(1996,3), frequency=4)

#-->GDP
# Tomar solo los valores numéricos (sin NAs iniciales)
gdp_vals <- na.omit(as.numeric(df_final$GDP.billion.currency.units))
# Crear serie trimestral empezando en el primer trimestre válido (ajusta si no es Q3 1996)
gdp_ts_trimestral <- ts(gdp_vals, start=c(1996,3), frequency=4)

# ---- CONVERTIR VARIABLES MENSUALES A TRIMESTRALES ----
# Promediando los valores de cada trimestre
#!!! HE HECHO EN TODOS LA MEDIA PERO NI IDEA

#money supply y stock market quedarse con el ulitmo valor es decir (enero,febrero, marzo quedarnos con MARZO)
# Unemployment: media trimestral
unemployment_ts_trimestral <- aggregate(unemployment_ts_mensual, nfrequency=4, FUN=mean)
# Money supply: último valor del trimestre
money_supply_ts_trimestral <- aggregate(money_supply_ts_mensual, nfrequency = 4, FUN = function(x) tail(x, 1))
# Stock market: último valor del trimestre
stock_market_ts_trimestral <- aggregate(stock_market_ts_mensual, nfrequency = 4, FUN = function(x) tail(x, 1))


# =========================
# 6. VISUALIZACIÓN DE SERIES MENSUALES Y TRIMESTRALES - AUSTRALIA
# =========================

# ---- SERIES MENSUALES ----
#--------------------------------------------------------------------------
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

# ---- SERIES TRIMESTRALES ----
#--------------------------------------------------------------------------
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

# Restaurar parámetros gráficos por defecto
par(mfrow=c(1,1))



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
  
  # Detectar outliers
  outs <- tsoutliers(original_ts)
  
  par(mfrow=c(2,1), mar=c(4,4,2,1))
  
  # Serie original con outliers
  plot(original_ts, type="l", lwd=2, col="blue",
       main=paste(titulo, "- Outliers marcados"), 
       ylab="Valor", xlab="Año",
       ylim=c(min(original_ts, na.rm=TRUE), max(original_ts, na.rm=TRUE)*1.1))
  
  if(length(outs$index) > 0){
    points(time(original_ts)[outs$index],
           original_ts[outs$index],
           col="red", pch=19, cex=1.2)
  }
  
  # Leyenda confiable en la esquina superior derecha
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


# ---- 3. Aplicar a cada variable ----
plot_outliers(unemployment_ts_trimestral, unemployment_ts_trimestral_sin_outliers, "Unemployment Rate (Trimestral, Australia)")
plot_outliers(money_supply_ts_trimestral, money_supply_ts_trimestral_sin_outliers, "Money Supply (Trimestral, Australia)")
plot_outliers(stock_market_ts_trimestral, stock_market_ts_trimestral_sin_outliers, "Stock Market Index (Trimestral, Australia)")
plot_outliers(cpi_ts_trimestral, cpi_ts_trimestral_sin_outliers, "CPI (Trimestral, Australia)")
plot_outliers(gdp_ts_trimestral, gdp_ts_trimestral_sin_outliers, "GDP (Trimestral, Australia)")



#======================
#11. TIENEN VARIANZA?
#======================

#!!!!!!!!!!!!! hacer con tsplot los grafcio de la varianza

#Mirar si tienen picos conforme avanza el tiempo, para ver si tienen varianza creciente.
#--> GDP
plot(gdp_ts_trimestral_sin_outliers, type="l", col="steelblue", lwd=2,
     main="GDP - Serie limpia",
     ylab="Valor", xlab="Tiempo")
abline(h=mean(gdp_ts_trimestral_sin_outliers, na.rm=TRUE), col="red", lty=2)
#Varianza Creciente--> rosa tienes estacionalidad, y poca varianza creciente

#--> CPI
plot(cpi_ts_trimestral_sin_outliers, type="l", col="steelblue", lwd=2,
     main="CPI - Serie limpia",
     ylab="Valor", xlab="Tiempo")
abline(h=mean(cpi_ts_trimestral_sin_outliers, na.rm=TRUE), col="red", lty=2)
#No Varianza creciente --> Varianza constante

#-->Stock Market
plot(stock_market_ts_trimestral_sin_outliers, type="l", col="steelblue", lwd=2,
     main="Stock Market - Serie limpia",
     ylab="Valor", xlab="Tiempo")
abline(h=mean(stock_market_ts_trimestral_sin_outliers, na.rm=TRUE), col="red", lty=2)
#Varianza creciente.

#--> Money Supply
plot(money_supply_ts_trimestral_sin_outliers, type="l", col="steelblue", lwd=2,
     main="Money Supply - Serie limpia",
     ylab="Valor", xlab="Tiempo")
abline(h=mean(money_supply_ts_trimestral_sin_outliers, na.rm=TRUE), col="red", lty=2)
#No varianza creciente --> Varianza constante

#--> Unemployment Rate
plot(unemployment_ts_trimestral_sin_outliers, type="l", col="steelblue", lwd=2,
     main="Unemployment - Serie limpia",
     ylab="Valor", xlab="Tiempo")
abline(h=mean(unemployment_ts_trimestral_sin_outliers, na.rm=TRUE), col="red", lty=2)
#No varianza creciente--> Varianza constante


#ESTABILIZAREMOS LA VARIANZA (log())--> GDP y STOCK MARKET
# Transformación log para estabilizar la varianza
#money_supply_ts_trimestral_sin_outliers <- log(money_supply_ts_trimestral_sin_outliers)
#unemployment_ts_trimestral_sin_outliers <- log(unemployment_ts_trimestral_sin_outliers)
stock_market_ts_trimestral_sin_outliers <- log(stock_market_ts_trimestral_sin_outliers)
gdp_ts_trimestral_sin_outliers <- log(gdp_ts_trimestral_sin_outliers)
#cpi_ts_trimestral_sin_outliers <- log(cpi_ts_trimestral_sin_outliers)






#=======================
# 10. SON SERIES ESTACIONARIAS?
#=======================

#los lags de las diferencias van en funcion de si hay estacionalidad, se ven en los graficos de autocorrelacion. o con el decompose 
#!!!! HAY QUE USAR TS.DISPLAY, AUTOCORRELACION , Y AUTOCORRELACION PARCIAL
#HAY QUE IR MIRANDO ESTO DE POCO EN POCO Y A LA VEZ APLICAR LOS TESES PARA CONFIRMAR.


########### ---------- Money Supply (TRIMESTRAL)  #####################
#######################################################################

#hacer los teses pirmero para ver el numero de diferencias

#----------        Primera diferencia
money_supply_diff1 <- diff(money_supply_ts_trimestral_sin_outliers, differences = 1)
#Comprobar si ya es estacionaria

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
LBtest_money_supply_1 <- Box.test(money_supply_diff1, lag = 20, type="Ljung")
if (LBtest_money_supply_1$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff1): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff1): Ausencia de correlación")
}

#EXISTE CORRELACION

#----------        Segunda diferencia
money_supply_diff2 <- diff(money_supply_ts_trimestral_sin_outliers, differences = 2)
#Comprobar si ya es estacionaria

#TEST ADF
adf_test_money_supply_2 <- adf.test(money_supply_diff2)
if(adf_test_money_supply_2$p.value < 0.05){
  print("Money Supply - ADF (diff1): estacionaria")
} else{
  print("Money Supply - ADF (diff1): NO estacionaria")
}
# ESTACIONARIA diff=1

#TEST KPSS
kpss_test_money_supply_2 <- kpss.test(money_supply_diff2, null="Level")
if(kpss_test_money_supply_2$p.value < 0.05){
  print("Money Supply - KPSS (diff1): NO estacionaria")
} else{
  print("Money Supply - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

#TEST LJUNG-BOX
LBtest_money_supply_1 <- Box.test(money_supply_diff1, lag = 20, type="Ljung")
if (LBtest_money_supply_1$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff1): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff1): Ausencia de correlación")
}


#hacer los ts.display
#me da que es estacionaria con las dos difernecias, entonces tengo que hacer ts.display

tsdisplay(money_supply_ts_trimestral_sin_outliers,diff=1, lag=4)

#cambiar el orden.


































###########            Money Supply (TRIMESTRAL)  #####################
#######################################################################

#----------        Serie original sin diferencias
#TEST ADF
adf_test_money_supply <- adf.test(money_supply_ts_trimestral_sin_outliers)
if(adf_test_money_supply$p.value < 0.05){
  print("Money Supply - ADF: estacionaria")
} else{
  print("Money Supply - ADF: NO estacionaria")
}
#NO ESTACIONARIA diff=0

#TEST KPSS
kpss_test_money_supply <- kpss.test(money_supply_ts_trimestral_sin_outliers, null="Level")
if(kpss_test_money_supply$p.value < 0.05){
  print("Money Supply - KPSS: NO estacionaria")
} else{
  print("Money Supply - KPSS: estacionaria")
}
#NO ESTACIONARIA diff=0
#En ambas no es estacionaria por lo que habra que hacer la primera diferencia

#TEST LB JUNG--> lag maximo para el cual quieres que analize que es estacionaria.
LBtest_money_supply <- Box.test(money_supply_ts_trimestral_sin_outliers, 
                                lag = 20, type="Ljung")
if (LBtest_money_supply$p.value < 0.05) {
  print("Money Supply - Ljung-Box: Existe correlación")
} else {
  print("Money Supply - Ljung-Box: Ausencia de correlación")
}
#EXISTE CORRELACION --> Los valores pasados influyen en los presentes 



#----------        Primera diferencia
money_supply_diff1 <- diff(money_supply_ts_trimestral_sin_outliers, differences = 1)
#Comprobar si ya es estacionaria

#TEST ADF
adf_test_money_supply_1 <- adf.test(money_supply_diff1)
if(adf_test_money_supply_1$p.value < 0.05){
  print("Money Supply - ADF (diff1): estacionaria")
} else{
  print("Money Supply - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=1

#TEST KPSS
kpss_test_money_supply_1 <- kpss.test(money_supply_diff1, null="Level")
if(kpss_test_money_supply_1$p.value < 0.05){
  print("Money Supply - KPSS (diff1): NO estacionaria")
} else{
  print("Money Supply - KPSS (diff1): estacionaria")
}
#NO ESTACIONARIA diff=1

#TEST LJUNG-BOX
LBtest_money_supply_1 <- Box.test(money_supply_diff1, lag = 20, type="Ljung")
if (LBtest_money_supply_1$p.value < 0.05) {
  print("Money Supply - Ljung-Box (diff1): Existe correlación")
} else {
  print("Money Supply - Ljung-Box (diff1): Ausencia de correlación")
}

#EXISTE CORRELACION

#!!!! una me da estacionaria con un test y otra no estacioanria con el otro test. entonces que hago con la siguiente diferencia o como?
#Hay contradicción entre ADF y KPSS, pero es muy común. En la práctica, se suele creer más en KPSS porque ADF tiene poca potencia en series pequeñas



###########            Unemployment Rate (TRIMESTRAL)  #####################
#######################################################################

#----------        Serie original sin diferencias

#TEST ADF
adf_test_unemployment <- adf.test(unemployment_ts_trimestral_sin_outliers)
if(adf_test_unemployment$p.value < 0.05){
  print("Unemployment Rate - ADF: estacionaria")
} else{
  print("Unemployment Rate - ADF: NO estacionaria")
}
#NO ESTACIONARIA diff=0

#TEST KPSS
kpss_test_unemployment <- kpss.test(unemployment_ts_trimestral_sin_outliers, null="Level")
if(kpss_test_unemployment$p.value < 0.05){
  print("Unemployment Rate - KPSS: NO estacionaria")
} else{
  print("Unemployment Rate - KPSS: estacionaria")
}
#NO ESTACIONARIA diff=0
#Ambas coinciden en no estacionaria habrá que hacer la primera diferencia

#TEST LJUNG-BOX
LBtest_unemployment <- Box.test(unemployment_ts_trimestral_sin_outliers, 
                                lag = 20, type = "Ljung")
if (LBtest_unemployment$p.value < 0.05) {
  print("Unemployment Rate - Ljung-Box: Existe correlación")
} else {
  print("Unemployment Rate - Ljung-Box: Ausencia de correlación")
}
#EXISTE CORRELACION


#----------        Primera diferencia
unemployment_diff1 <- diff(unemployment_ts_trimestral_sin_outliers, differences = 1)

#Comprobar si ya es estacionaria
adf_test_unemployment_1 <- adf.test(unemployment_diff1)
if(adf_test_unemployment_1$p.value < 0.05){
  print("Unemployment Rate - ADF (diff1): estacionaria")
} else{
  print("Unemployment Rate - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=1

kpss_test_unemployment_1 <- kpss.test(unemployment_diff1, null="Level")
if(kpss_test_unemployment_1$p.value < 0.05){
  print("Unemployment Rate - KPSS (diff1): NO estacionaria")
} else{
  print("Unemployment Rate - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

#TEST LJUNG-BOX
LBtest_unemployment_1 <- Box.test(unemployment_diff1, lag = 20, type="Ljung")
if (LBtest_unemployment_1$p.value < 0.05) {
  print("Unemployment Rate - Ljung-Box (diff1): Existe correlación")
} else {
  print("Unemployment Rate - Ljung-Box (diff1): Ausencia de correlación")
}
#EXISTE CORRELACION

#!!!! una me da estacionaria con un test y otra no estacioanria con el otro test. entonces que hago con la siguiente diferencia o como?
#YA SON LAS DOS ESTACIONARIAS, PERO EXISTE CORRELACION TODAVIA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#HACER LOS GRAFICOS Y TODO



###########            GDP (Billion currency units) (TRIMESTRAL)  #####################
#######################################################################

#----------        Serie original sin diferencias
adf_test_gdp <- adf.test(gdp_ts_trimestral_sin_outliers)
if(adf_test_gdp$p.value < 0.05){
  print("GDP (log) - ADF: estacionaria")
} else{
  print("GDP (log) - ADF: NO estacionaria")
}
#NO ESTACIONARIA diff=0

kpss_test_gdp <- kpss.test(gdp_ts_trimestral_sin_outliers, null="Level")
if(kpss_test_gdp$p.value < 0.05){
  print("GDP (log) - KPSS: NO estacionaria")
} else{
  print("GDP (log) - KPSS: estacionaria")
}
#NO ESTACIONARIA-> diff=0

#TEST LJUNG-BOX
LBtest_gdp <- Box.test(gdp_ts_trimestral_sin_outliers, 
                                lag = 20, type = "Ljung")
if (LBtest_gdp$p.value < 0.05) {
  print("GDP (log) - Ljung-Box: Existe correlación")
} else {
  print("GDP (log) - Ljung-Box: Ausencia de correlación")
}
#EXISTE CORRELACION

#Ambos indican no estacionaria, se pasa a primera diferencia

#----------        Primera diferencia
gdp_diff1 <- diff(gdp_ts_trimestral_sin_outliers, differences = 1)

#Comprobar estacionariedad
adf_test_gdp_1 <- adf.test(gdp_diff1)
if(adf_test_gdp_1$p.value < 0.05){
  print("GDP (log) - ADF (diff1): estacionaria")
} else{
  print("GDP (log) - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=1

kpss_test_gdp_1 <- kpss.test(gdp_diff1, null="Level")
if(kpss_test_gdp_1$p.value < 0.05){
  print("GDP (log) - KPSS (diff1): NO estacionaria")
} else{
  print("GDP (log) - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

#Una vez que ya es estacionaria queda verificar si en esa serie diferenciada ya no queda correlacion significativa (residuos , ruido blanco)
#TEST LJUNG-BOX
LBtest_gdp_1 <- Box.test(gdp_diff1, lag = 20, type="Ljung")
if (LBtest_gdp_1$p.value < 0.05) {
  print("GDP (log) - Ljung-Box (diff1): Existe correlación")
} else {
  print("GDP (log) - Ljung-Box (diff1): Ausencia de correlación")
}
#AUSENCIA DE CORRELACION. 
#Genial ya esta la dependecia temporal quitada y puedo usar modelos simple como ARIMA.

#----> ESTACIONARIA CON LA DIFERENCIA 1 !!!!!

#Cambiaremos el nombre a la variable
gdp_ts_trimestral_sin_outliers_estacionaria<- gdp_diff1

#-----------------------------------------------------------------
#Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparacion)
par(mfrow=c(2,1))  # dos gráficos en una ventana
plot(gdp_ts_trimestral_sin_outliers, type="l", main="GDP (log) - Serie original", ylab="Nivel (log)")
plot(gdp_ts_trimestral_sin_outliers_estacionaria, type="l", main="GDP (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  

#-------    ACF Y PACF
#Muy utiles para ver la dependecia temporal y sugerir ordenes a AR/MA en modelos ARIMA:
p1 <- ggAcf(gdp_ts_trimestral_sin_outliers_estacionaria) + ggtitle("ACF - GDP (log, diff1)")
p2 <- ggPacf(gdp_ts_trimestral_sin_outliers_estacionaria) + ggtitle("PACF - GDP (log, diff1)")
grid.arrange(p1, p2, ncol=1)   

#-------   QQ-PLOT 
#Comprobar si los reisuod parecen normales:
#Es util porque cuando modelamos con ARIMA , los resiudos deberian comportarse como ruido blanco.
qqnorm(gdp_ts_trimestral_sin_outliers_estacionaria, main="QQ-plot - GDP (log, diff1)")
qqline(gdp_ts_trimestral_sin_outliers_estacionaria, col="red")
#El gráfico muestra que los datos no siguen perfectamente una normal, sobre todo en las colas. Esto es común en series económicas (como el PIB) porque suelen tener shocks o valores atípicos.
#Si el análisis es de series temporales (ARIMA, VAR, etc.), no es raro que los residuos no sean normales y suele aceptarse mientras no haya autocorrelación grave.








###########            CPI (Consumer Price Index) (TRIMESTRAL)  #####################
#######################################################################

#----------        Serie original sin diferencias
adf_test_cpi <- adf.test(cpi_ts_trimestral_sin_outliers)
if(adf_test_cpi$p.value < 0.05){
  print("CPI (log) - ADF: estacionaria")
} else{
  print("CPI (log) - ADF: NO estacionaria")
}
#NO ESTACIONARIA diff=0

kpss_test_cpi <- kpss.test(cpi_ts_trimestral_sin_outliers, null="Level")
if(kpss_test_cpi$p.value < 0.05){
  print("CPI (log) - KPSS: NO estacionaria")
} else{
  print("CPI (log) - KPSS: estacionaria")
}
#NO ESTACIONARIA diff=0


#TEST LJUNG-BOX
LBtest_cpi <- Box.test(cpi_ts_trimestral_sin_outliers, 
                       lag = 20, type = "Ljung")
if (LBtest_cpi$p.value < 0.05) {
  print("CPI (log) - Ljung-Box: Existe correlación")
} else {
  print("CPI (log) - Ljung-Box: Ausencia de correlación")
}
#EXISTE CORRELACION



#----------        Primera diferencia
cpi_diff1 <- diff(cpi_ts_trimestral_sin_outliers, differences = 1)

#Comprobar estacionariedad
adf_test_cpi_1 <- adf.test(cpi_diff1)
if(adf_test_cpi_1$p.value < 0.05){
  print("CPI (log) - ADF (diff1): estacionaria")
} else{
  print("CPI (log) - ADF (diff1): NO estacionaria")
}
#NO ESTACIONARIA diff=1

kpss_test_cpi_1 <- kpss.test(cpi_diff1, null="Level")
if(kpss_test_cpi_1$p.value < 0.05){
  print("CPI (log) - KPSS (diff1): NO estacionaria")
} else{
  print("CPI (log) - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

#!!!!!! Cada test me dice una cosa

LBtest_cpi_1 <- Box.test(cpi_diff1, lag = 20, type="Ljung")
if (LBtest_cpi_1$p.value < 0.05) {
  print("CPI (log) - Ljung-Box (diff1): Existe correlación")
} else {
  print("CPI (log) - Ljung-Box (diff1): Ausencia de correlación")
}

#AUSENCIA DE CORRELACION






###########            STOCK MARKET (TRIMESTRAL)  #####################
#######################################################################

#----------        Serie original sin diferencias
adf_test_stock <- adf.test(stock_market_ts_trimestral_sin_outliers)
if(adf_test_stock$p.value < 0.05){
  print("Stock Market (log) - ADF: estacionaria")
} else{
  print("Stock Market (log) - ADF: NO estacionaria")
}
#NO ESTACIONARIA diff=0

kpss_test_stock <- kpss.test(stock_market_ts_trimestral_sin_outliers, null="Level")
if(kpss_test_stock$p.value < 0.05){
  print("Stock Market (log) - KPSS: NO estacionaria")
} else{
  print("Stock Market (log) - KPSS: estacionaria")
}
#NO ESTACIONARIA diff=0

#TEST LJUNG-BOX
LBtest_stock <- Box.test(stock_market_ts_trimestral_sin_outliers, 
                       lag = 20, type = "Ljung")
if (LBtest_stock$p.value < 0.05) {
  print("Stock Market (log) - Ljung-Box: Existe correlación")
} else {
  print("Stock Market (log) - Ljung-Box: Ausencia de correlación")
}
#EXISTE CORRELACION

#----------        Primera diferencia
stock_diff1 <- diff(stock_market_ts_trimestral_sin_outliers, differences = 1)

#Comprobar estacionariedad
adf_test_stock_1 <- adf.test(stock_diff1)
if(adf_test_stock_1$p.value < 0.05){
  print("Stock Market (log) - ADF (diff1): estacionaria")
} else{
  print("Stock Market (log) - ADF (diff1): NO estacionaria")
}
#ESTACIONARIA diff=1

kpss_test_stock_1 <- kpss.test(stock_diff1, null="Level")
if(kpss_test_stock_1$p.value < 0.05){
  print("Stock Market (log) - KPSS (diff1): NO estacionaria")
} else{
  print("Stock Market (log) - KPSS (diff1): estacionaria")
}
#ESTACIONARIA diff=1

#TEST LJUNG-BOX
LBtest_stock_1 <- Box.test(stock_diff1, lag = 20, type="Ljung")
if (LBtest_stock_1$p.value < 0.05) {
  print("Stock Market (log) - Ljung-Box (diff1): Existe correlación")
} else {
  print("Stock Market (log) - Ljung-Box (diff1): Ausencia de correlación")
}

#AUSENCIA DE CORRELACION --> Genial

#La variable "Stock Market" es estacionaria al aplicar la primera diferencia
stock_market_ts_trimestral_sin_outliers_estacionaria<- stock_diff1

#-----------------------------------------------------------------
#Graficamos
#-----------------------------------------------------------------
#-------    SERIE ORIGINAL VS SERIE EN DIFERENCIA (Comparacion)
par(mfrow=c(2,1))  # dos gráficos en una ventana
plot(stock_market_ts_trimestral_sin_outliers, type="l", 
     main="Stock Market (log) - Serie original", ylab="Nivel (log)")
plot(stock_market_ts_trimestral_sin_outliers_estacionaria, type="l", 
     main="Stock Market (log) - Primera diferencia (estacionaria)", ylab="Cambio trimestral")
par(mfrow=c(1,1))  

#-------    ACF Y PACF
#Muy útiles para ver la dependencia temporal y sugerir órdenes AR/MA en modelos ARIMA:
p1 <- ggAcf(stock_market_ts_trimestral_sin_outliers_estacionaria) + 
  ggtitle("ACF - Stock Market (log, diff1)")
p2 <- ggPacf(stock_market_ts_trimestral_sin_outliers_estacionaria) + 
  ggtitle("PACF - Stock Market (log, diff1)")
grid.arrange(p1, p2, ncol=1)   

#-------   QQ-PLOT 
#Comprobar si los residuos parecen normales:
#Es útil porque cuando modelamos con ARIMA, los residuos deberían comportarse como ruido blanco.
qqnorm(stock_market_ts_trimestral_sin_outliers_estacionaria, 
       main="QQ-plot - Stock Market (log, diff1)")
qqline(stock_market_ts_trimestral_sin_outliers_estacionaria, col="red")

#Los puntos siguen bastante bien la línea roja en el centro → los datos son aproximadamente normales.
#En las colas hay ligeras desviaciones → presencia de colas algo más pesadas que la normal.
#En resumen: la serie diferenciada es casi normal, con pequeñas desviaciones en los extremos, algo típico en series financieras por la volatilidad.








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







#Luego ANalizaremos correlacion y relacion
#modelado de series tmeporales
#reocnstruccion de escala original
