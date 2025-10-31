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
library(tseries)
library(gridExtra)
library(astsa)
library(Metrics)
library(reshape2)
library(cowplot)
library(ggrepel)




paleta <- c("#c88fb2",  "#8db41c",  "#93044e","#D1006F",  "#F5F0E6",  "#4D4D4D")


#=========================
#Cargar datos:
#=========================
datos_limpios_AUS<- read.csv("DATOS/limpios/Datos_Limpios_Australia.csv")



#Cargar series temporales ESTACIONARIAS:
train_money_supply_estacionaria <- readRDS("DATOS/Series_Temporales_ESTACIONARIAS/Train_Money_Supply_ts_ESTACIONARIA.rds")
train_unemployment_estacionaria <- readRDS("DATOS/Series_Temporales_ESTACIONARIAS/Train_Unemployment_ts_ESTACIONARIA.rds")
train_pib_estacionaria <- readRDS("DATOS/Series_Temporales_ESTACIONARIAS/PIB_Train_ts_ESTACIONARIA.rds")
train_ipc_estacionaria <- readRDS("DATOS/Series_Temporales_ESTACIONARIAS/IPC_Train_ts_ESTACIONARIA.rds")
train_stock_market_estacionaria <- readRDS("DATOS/Series_Temporales_ESTACIONARIAS/Train_Stock_Market_ts_ESTACIONARIA.rds")
train_ipc<- readRDS("DATOS/Series_Temporales/Train_Test_Series/IPC_Train_ts.rds")
test_ipc<- readRDS("DATOS/Series_Temporales/Train_Test_Series/IPC_Test_ts.rds")
train_pib<- readRDS("DATOS/Series_Temporales/Train_Test_Series/PIB_Train_ts.rds")
test_pib<- readRDS("DATOS/Series_Temporales/Train_Test_Series/PIB_Test_ts.rds")
train_pib_log<- readRDS("DATOS/Series_Temporales/Train_Test_Series/Train_PIB_log")


#Cargar series originales (Sin diferencias):

train_stock_market <- readRDS("DATOS/Series_Temporales/Train_Test_Series/Train_stock_market.rds")
train_money_supply <- readRDS("DATOS/Series_Temporales/Train_Test_Series/Train_money_supply.rds")
train_unemployment<- readRDS("DATOS/Series_Temporales/Train_Test_Series/Train_unemployment.rds")

test_stock_market <- readRDS("DATOS/Series_Temporales/Train_Test_Series/Test_stock_market.rds")
test_money_supply <- readRDS("DATOS/Series_Temporales/Train_Test_Series/Test_money_supply.rds")
test_unemployment<- readRDS("DATOS/Series_Temporales/Train_Test_Series/Test_unemployment.rds")


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

# Extraer AIC, AICc y BIC
modelo_autoarima_ipc$aic    # AIC
modelo_autoarima_ipc$aicc   # AICc
modelo_autoarima_ipc$bic    # BIC

# Justificación:
# - d=0 → la serie ya fue diferenciada manualmente.
# - d=1 → solo si se observa tendencia residual o mejora notable de AIC.
#Es normal que las predicciones sean iguales: es la consecuencia lógica de una serie sin tendencia.


#Validacion grafica de residaules:
hist(residuals(modelo_autoarima_ipc), main="Histograma de residuales", xlab="Residual", col=paleta[2])
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
# Convertir train y test a ts
train_ts <- ts(as.numeric(train_ipc), start=c(1998,1), frequency=4)
test_ts  <- ts(as.numeric(test_ipc),  start=c(2021,1), frequency=4)
forecast_ts <- ts(forecast_autoarima_ipc_revertida,start = end(train_ts) + c(0,1), frequency = 4)

# Combinar para ggplot
df_plot <- data.frame(Fecha = c(time(train_ts), time(forecast_ts), time(test_ts)),
                      Valor = c(as.numeric(train_ts), as.numeric(forecast_ts), as.numeric(test_ts)), Tipo  = c(rep("Train", length(train_ts)),rep("Predicción", length(forecast_ts)),rep("Test", length(test_ts))))

# Graficar
ggplot(df_plot, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción IPC AutoARIMA", x="Año", y="IPC") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")


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

# Extraer AIC, AICc y BIC
aic_arima_ipc <-modelo_arima_ipc$aic    # AIC
k <- length(modelo_arima_ipc$coef)  # ar + intercept
n <- length(train_ipc_estacionaria) # tamaño de la muestra
aicc_arima_ipc <- aic_arima_ipc + (2 * k * (k + 1)) / (n - k - 1)
aicc_arima_ipc
bic_arima_ipc <- aic_arima_ipc + k * log(n)
bic_arima_ipc

# Validación de residuales
hist(residuals(modelo_arima_ipc), main="Histograma de residuales ARIMA IPC", xlab="Residual", col=paleta[2])
checkresiduals(modelo_arima_ipc)

boxtest_arima_ipc <- Box.test(residuals(modelo_arima_ipc), lag = round(log(length(train_ipc))), type="Ljung-Box")
if (boxtest_arima_ipc$p.value > 0.05) {
  cat("Residuos ARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}
#Residuos ARIMA IPC parecen ruido blanco

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
train_ts1 <- ts(as.numeric(train_ipc), start=c(1998,1), frequency=4)
test_ts1  <- ts(as.numeric(test_ipc),  start=c(2021,1), frequency=4)

# Predicción pegada al train
forecast_ts1 <- ts(forecast_arima_ipc_revertida,
                  start = end(train_ts1) + c(0,1),frequency = 4)

# Combinar para ggplot
df_plot1 <- data.frame(
  Fecha = c(time(train_ts1), time(forecast_ts1), time(test_ts1)),
  Valor = c(as.numeric(train_ts1), as.numeric(forecast_ts1), as.numeric(test_ts1)),
  Tipo  = c(rep("Train", length(train_ts1)),
            rep("Predicción", length(forecast_ts1)),
            rep("Test", length(test_ts1)))
)

# Graficar
ggplot(df_plot1, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción IPC ARIMA Manual", x="Año", y="IPC") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")




#-----------------      SARIMA MANUAL ---- IPC----------------------
###########################################################################

#p= 0: No hay picos importantes en PACF → sin componente AR
#q= 1: ACF tiene ligera autocorrelación inicial → componente MA(1) captura el efecto
#d= 0 (ya esta la serie estacionaria no hace falta diferenciarla mas)

#P= 0: No hay picos en múltiplos de 4 → sin patrón estacional autoregresivo
#Q= 0: No hay correlaciones estacionales significativas
#D= 0: Ya eliminaste diferencias estacionales

#Elegir los mejores parametros:
#png("ACF_IPC_Train_Estacionaria.png", width = 1800, height = 1400, res= 150)
acf(train_ipc_estacionaria)
#dev.off()

#png("PACF_IPC_Train_Estacionaria.png", width = 1800, height = 1400, res= 150)
pacf(train_ipc_estacionaria)
#dev.off()

tsdisplay(train_ipc_estacionaria)


#-----------------      MODELO SARIMA IPC
modelo_sarima_ipc <- arima(train_ipc_estacionaria,
                           order = c(0, 0, 1),
                           seasonal = list(order = c(1, 0, 0), period = 4),
                           method = "ML")
summary(modelo_sarima_ipc)
#AIC, AICc, BIC
aic_sarima_ipc <- modelo_sarima_ipc$aic
k <- length(modelo_sarima_ipc$coef)
n <- length(train_ipc_estacionaria)
aicc_sarima_ipc <- aic_sarima_ipc + (2 * k * (k + 1)) / (n - k - 1)
bic_sarima_ipc <- aic_sarima_ipc + k * log(n)
aic_sarima_ipc
aicc_sarima_ipc
bic_sarima_ipc

checkresiduals(modelo_sarima_ipc)

boxtest_sarima_ipc <- Box.test(residuals(modelo_sarima_ipc), lag = round(log(length(train_ipc))), type="Ljung-Box")
if (boxtest_sarima_ipc$p.value > 0.05) {
  cat("Residuos SARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos SARIMA IPC muestran autocorrelación\n")
}
#Residuos ARIMA IPC parecen ruido blanco

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
acc_sarima_ipc <- forecast::accuracy(forecast_sarima_ipc_revertida, test_ipc)
acc_sarima_ipc

#-----------------      GRAFICO FINAL
# Convertir train y test a series temporales trimestrales
train_ts2 <- ts(as.numeric(train_ipc), start=c(1998,1), frequency=4)
test_ts2  <- ts(as.numeric(test_ipc),  start=c(2021,1), frequency=4)

# Predicción pegada al train
forecast_ts2 <- ts(forecast_sarima_ipc_revertida,
                   start = end(train_ts2) + c(0,1), frequency = 4)

# Combinar todo en un solo data frame para ggplot
df_plot2 <- data.frame(
  Fecha = c(time(train_ts2), time(forecast_ts2), time(test_ts2)),
  Valor = c(as.numeric(train_ts2), as.numeric(forecast_ts2), as.numeric(test_ts2)),
  Tipo  = c(rep("Train", length(train_ts2)),
            rep("Predicción", length(forecast_ts2)),
            rep("Test", length(test_ts2)))
)

# Graficar
ggplot(df_plot2, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción IPC SARIMA Manual", x="Año", y="IPC") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")




#........................................ PIB ..................................
#--------------------------------------------------------------------------------


#-----------------      AUTOARIMA ---- PIB        ----------------------
###########################################################################
#-------  MODELO
modelo_autoarima_pib <- auto.arima(train_pib_estacionaria, seasonal = FALSE, d = 0)
summary(modelo_autoarima_pib)

#AIC, AICc, BIC
modelo_autoarima_pib$aic
modelo_autoarima_pib$aicc
modelo_autoarima_pib$bic


# Validación gráfica de residuales
hist(residuals(modelo_autoarima_pib), main = "Histograma de residuales AutoARIMA PIB", xlab = "Residual", col = paleta[3])
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
                        xi = tail(diff(train_pib_log, lag = 4), 1))

# Revertir la primera diferencia (la estacional con lag = 4)
forecast_autoarima_pib_revertida <- diffinv(forecast_tmp_autoarima, 
                                        lag = 4, 
                                        differences = 1, 
                                        xi = tail(train_pib_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_autoarima_pib_revertida <- forecast_autoarima_pib_revertida[-c(1:4)]

# Deshacer el logaritmo
forecast_autoarima_pib_revertida <- exp(forecast_autoarima_pib_revertida)


#-------  ACCURACY
acc_autoarima_pib <- forecast::accuracy(forecast_autoarima_pib_revertida, test_pib)
acc_autoarima_pib


#------------------ Graficar Train vs Test vs Predicción ------------------
#-----------------      GRAFICO FINAL 3
train_ts3 <- ts(as.numeric(train_pib), start=c(1998,1), frequency=4)
test_ts3  <- ts(as.numeric(test_pib),  start=c(2021,1), frequency=4)
forecast_ts3 <- ts(forecast_autoarima_pib_revertida,
                   start = end(train_ts3) + c(0,1), frequency = 4)

df_plot3 <- data.frame(
  Fecha = c(time(train_ts3), time(forecast_ts3), time(test_ts3)),
  Valor = c(as.numeric(train_ts3), as.numeric(forecast_ts3), as.numeric(test_ts3)),
  Tipo  = c(rep("Train", length(train_ts3)),
            rep("Predicción", length(forecast_ts3)),
            rep("Test", length(test_ts3))))

# Graficar
ggplot(df_plot3, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción PIB AutoARIMA", x="Año", y="PIB") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")




#-----------------      ARIMA MANUAL ---- PIB       ----------------------
###########################################################################
# train_ipc_estacionaria es la serie ya diferenciada (d=1)

# 1. Ver ACF y PACF para decidir p y q
#Elegiriremos p y q fijandolnos en la autoccorelacion:
#p=orden autoregresivo → se decide con la PACF.--> p ≈ 1 (PACF corta rápido después de lag 1)
#q= orden de media móvil → se decide con la ACF. -_> q ≈ 1 (ACF corta rápido después de lag 1)
acf(train_pib_estacionaria, main="ACF PIB diferenciada")
pacf(train_pib_estacionaria, main="PACF PIB diferenciada")
tsdisplay(train_pib_estacionaria)
#ROSA: con el acf y pacf me dice que (1,0,1) pero es que el accuracy me da mejor con (2,0,2)

#----------   MODELOS
modelo_arima_pib <- arima(train_pib_estacionaria, order=c(2,0,2))  # d=0 porque ya diferenciamos
summary(modelo_arima_pib)


#AIC, AICc, BIC
aic_arima_pib <- modelo_arima_pib$aic
k <- length(modelo_arima_pib$coef)
n <- length(train_ipc_estacionaria)
aicc_arima_pib  <- aic_arima_pib + (2 * k * (k + 1)) / (n - k - 1)
bic_arima_pib  <- aic_arima_pib + k * log(n)
aic_arima_pib 
aicc_arima_pib 
bic_arima_pib 

# Validación de residuales
hist(residuals(modelo_arima_pib), main="Histograma de residuales ARIMA PIB", xlab="Residual", col=paleta[3])

#png("checkresiduals_PIB_arima.png", width = 1800, height = 1400, res= 150)
checkresiduals(modelo_arima_pib)
#dev.off()

boxtest_arima_pib <- Box.test(residuals(modelo_arima_pib), lag = round(log(length(train_pib))), type="Ljung-Box")
if (boxtest_arima_pib$p.value > 0.05) {
  cat("Residuos ARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}
#Residuos ARIMA IPC parecen ruido blanco

#-----------------      PREDICCIONES
prediccion_arima_pib <- forecast(modelo_arima_pib , h=length(test_pib), level=90)
autoplot(prediccion_arima_pib) + ggtitle("Predicción PIB con ARIMA") + ylab("PIB") + xlab("Trimestre") + theme_minimal()

#-----------------      REVERTIR
# Revertir la segunda diferencia (la no estacional)
forecast_tmp <- diffinv(prediccion_arima_pib$mean, 
                        differences = 1, 
                        xi = tail(diff(train_pib_log, lag = 4), 1))

# Revertir la primera diferencia (la estacional con lag = 4)
forecast_arima_pib_revertida <- diffinv(forecast_tmp, 
                                        lag = 4, 
                                        differences = 1, 
                                        xi = tail(train_pib_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_arima_pib_revertida <- forecast_arima_pib_revertida[-c(1:5)]

# Deshacer el logaritmo
forecast_arima_pib_revertida <- exp(forecast_arima_pib_revertida)


#-----------------      ACCURACY
acc_arima_pib <- forecast::accuracy(forecast_arima_pib_revertida, test_pib)
acc_arima_pib


#-----------------      GRAFICO FINAL
train_ts4 <- ts(as.numeric(train_pib), start=c(1998,1), frequency=4)
test_ts4  <- ts(as.numeric(test_pib),  start=c(2021,1), frequency=4)
forecast_ts4 <- ts(forecast_arima_pib_revertida,
                   start = end(train_ts4) + c(0,1), frequency = 4)

df_plot4 <- data.frame(
  Fecha = c(time(train_ts4), time(forecast_ts4), time(test_ts4)),
  Valor = c(as.numeric(train_ts4), as.numeric(forecast_ts4), as.numeric(test_ts4)),
  Tipo  = c(rep("Train", length(train_ts4)),
            rep("Predicción", length(forecast_ts4)),
            rep("Test", length(test_ts4))))

# Graficar
png("PIB_Train_prediccion_test_ARIMA.png", width = 2500, height = 1800, res = 200)
ggplot(df_plot4, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción PIB ARIMA Manual (2,0,2)", x="Año", y="PIB") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")
dev.off()


#-----------------      SARIMA MANUAL ---- PIB       ----------------------
###########################################################################

#q=1 --> En lag 1, hay un pico fuerte negativo
#Q=0 --> No hay un pico fuerte estacionla (en los lag 4,8...)

#p=1 --> El PACF corta bruscamente despues de lag 1
#P=0 No hay un pico grande en lag =4

tsdisplay(train_pib_estacionaria)
#----------   MODELO
modelo_sarima_pib<- arima(train_pib_estacionaria,
                       order = c(2, 0, 2),
                       seasonal = list(order = c(0, 0, 1), period = 4),
                       method = "ML")
summary(modelo_sarima_pib)
checkresiduals(modelo_sarima_pib)



#AIC, AICc, BIC
aic_sarima_pib <- modelo_sarima_pib$aic
k <- length(modelo_sarima_pib$coef)
n <- length(train_pib_estacionaria)
aicc_sarima_pib  <- aic_sarima_pib + (2 * k * (k + 1)) / (n - k - 1)
bic_sarima_pib  <- aic_sarima_pib + k * log(n)
aic_sarima_pib 
aicc_sarima_pib 
bic_sarima_pib 


#-----------------      VALIDACIÓN DE RESIDUALES
hist(residuals(modelo_sarima_pib), main="Histograma de residuales SARIMA PIB", 
     xlab="Residual", col=paleta[3])
checkresiduals(modelo_sarima_pib)

boxtest_sarima_pib <- Box.test(residuals(modelo_sarima_pib), 
                               lag = round(log(length(train_pib))), 
                               type="Ljung-Box")
if (boxtest_sarima_pib$p.value > 0.05) {
  cat("Residuos SARIMA PIB parecen ruido blanco\n")
} else {
  cat("Residuos SARIMA PIB muestran autocorrelación\n")
}
#Residuos SARIMA PIB parecen ruido blanco

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
                        xi = tail(diff(train_pib_log, lag = 4), 1))

# Revertir la primera diferencia (estacional con lag = 4)
forecast_sarima_pib_revertida <- diffinv(forecast_tmp, 
                                         lag = 4, 
                                         differences = 1, 
                                         xi = tail(train_pib_log, 4))

# Quitar los valores iniciales usados en la inversión
forecast_sarima_pib_revertida <- forecast_sarima_pib_revertida[-c(1:5)]

# Deshacer el logaritmo
forecast_sarima_pib_revertida <- exp(forecast_sarima_pib_revertida)

#-----------------      ACCURACY
acc_sarima_pib <- forecast::accuracy(forecast_sarima_pib_revertida,test_pib)
acc_sarima_pib

#-----------------      GRAFICO FINAL
train_ts5 <- ts(as.numeric(train_pib), start=c(1998,1), frequency=4)
test_ts5  <- ts(as.numeric(test_pib),  start=c(2021,1), frequency=4)
forecast_ts5 <- ts(forecast_sarima_pib_revertida,
                   start = end(train_ts5) + c(0,1), frequency = 4)

df_plot5 <- data.frame(
  Fecha = c(time(train_ts5), time(forecast_ts5), time(test_ts5)),
  Valor = c(as.numeric(train_ts5), as.numeric(forecast_ts5), as.numeric(test_ts5)),
  Tipo  = c(rep("Train", length(train_ts5)),
            rep("Predicción", length(forecast_ts5)),
            rep("Test", length(test_ts5))))

# Graficar
ggplot(df_plot5, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción PIB SARIMA Manual", x="Año", y="PIB") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")






##############################################################################################
############################                 ARIMAX                ###########################
##############################################################################################

################        ---------    IPC   ----------------          #########################

# -------------------- PREPARAR TRAIN EXÓGENAS --------------------

train_exogenas_estacionarias<- cbind(train_stock_market_estacionaria, train_money_supply_estacionaria, train_unemployment_estacionaria)
train_exogenas_estacionarias<- na.omit(train_exogenas_estacionarias)

#Cambiamos el train del ipc para que el start de las exgoneas y el del ipc sean iguales

#El IPC empieza en 1998 segundo trimestres, y las exogenas en 1999 segundo trimestre. Hya que quitar un año
#Alinear fechas con train IPC
train_ipc_estacionaria_ARIMAX <- window(train_ipc_estacionaria, start = c(1999, 2))   # empieza 1999 trimestre 2


# -------------------- PREPARAR TEST EXÓGENAS ---------------------

# --------- STOCK MARKET (1ª diferencia + log) ---------
# Transformación logarítmica del test
stock_market_test_log <- log(test_stock_market)
train_stock_market_log <- log(train_stock_market)
# Usar el último valor del TRAIN *en log*, no sin log
ultimo_train_log <- as.numeric(tail(train_stock_market_log, 1))

# Concatenar para mantener continuidad temporal
stock_market_test_full <- ts(
  c(ultimo_train_log, as.numeric(stock_market_test_log)),
  start = end(train_stock_market_log),
  frequency = 4
)

# Aplicar primera diferencia
test_stock_market_estacionaria <- diff(stock_market_test_full)

# Mantener solo el rango del test
test_stock_market_estacionaria <- window(
  test_stock_market_estacionaria,
  start = start(test_stock_market),
  end = end(test_stock_market)
)

# Verificar
print(test_stock_market_estacionaria)
range(test_stock_market_estacionaria)

# --------- MONEY SUPPLY (2ª diferencia) ---------
money_supply_test_full <- ts(c(tail(train_money_supply, 2), test_money_supply),
                             start = start(tail(train_money_supply, 2)),
                             frequency = 4)
test_money_supply_estacionaria <- diff(diff(money_supply_test_full))  # 2ª diferencia
test_money_supply_estacionaria <- window(test_money_supply_estacionaria, 
                                         start = start(test_money_supply), 
                                         end = end(test_money_supply))

# --------- UNEMPLOYMENT RATE (2ª diferencia con lag = 4) ---------
# Para diferencias con lag=4 necesitamos los últimos 4+2=6 valores del train
unemployment_test_full <- ts(c(tail(train_unemployment, 6), test_unemployment),
                             start = start(tail(train_unemployment, 6)),
                             frequency = 4)
test_unemployment_estacionaria <- diff(diff(unemployment_test_full, lag = 4))  # 2ª diferencia con lag 4
test_unemployment_estacionaria <- window(test_unemployment_estacionaria, 
                                         start = start(test_unemployment), 
                                         end = end(test_unemployment))

# -------------------- COMBINAR TODAS LAS EXÓGENAS -----------------
test_exogenas_estacionarias <- cbind(
  test_stock_market_estacionaria,
  test_money_supply_estacionaria,
  test_unemployment_estacionaria
)
colnames(test_exogenas_estacionarias) <- colnames(train_exogenas_estacionarias)



###############################################################################################################33
# -------------------- AJUSTAR MODELO ARIMAX - AUTOARIMA  (IPC) ----------------------
modelo_arimax_autoarima_ipc <- auto.arima(
  train_ipc_estacionaria_ARIMAX,
  seasonal = FALSE,
  d = 0,
  xreg = train_exogenas_estacionarias,)
summary(modelo_arimax_autoarima_ipc)
# Calcular criterios
aic_arimax_autoarima_IPC <- AIC(modelo_arimax_autoarima_ipc)
bic_arimax_autoarima_IPC <- BIC(modelo_arimax_autoarima_ipc)
aicc_arimax_autoarima_IPC <- modelo_arimax_autoarima_ipc$aicc
cat("AIC:", aic_arimax_autoarima_IPC, "BIC:", bic_arimax_autoarima_IPC, "AICc:", aicc_arimax_autoarima_IPC, "\n")

# -------------------- PREDICCION ARIMAX AUTOARIMA -----------------

prediccion_arimax_autoarima_ipc <- forecast(
  modelo_arimax_autoarima_ipc,
  xreg = test_exogenas_estacionarias,
  h = length(test_ipc))
summary(prediccion_arimax_autoarima_ipc)


#-----------     REVERTIR
#Revertimos el forecast (IPC TRAIN)
# La serie original fue diferenciada 1 vez, usamos diffinv
forecast_arimax_autoarima_ipc_revertida <- diffinv(prediccion_arimax_autoarima_ipc$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arimax_autoarima_ipc_revertida <- forecast_arimax_autoarima_ipc_revertida[-1]

#---------      ACCURACY
accuracy_arimax_autoarim_ipc<- forecast::accuracy(forecast_arimax_autoarima_ipc_revertida, test_ipc)
accuracy_arimax_autoarim_ipc


#----------       GRAFICO
train_ts6 <- ts(as.numeric(train_ipc), start=c(1999,2), end=c(2020,4), frequency=4)
test_ts6  <- ts(as.numeric(test_ipc), start=c(2021,1), frequency=4)
forecast_ts6 <- ts(forecast_arimax_autoarima_ipc_revertida,
                   start = end(train_ts6) + c(0,1), frequency = 4)

df_plot6 <- data.frame(
  Fecha = c(time(train_ts6), time(forecast_ts6), time(test_ts6)),
  Valor = c(as.numeric(train_ts6), as.numeric(forecast_ts6), as.numeric(test_ts6)),
  Tipo  = c(rep("Train", length(train_ts6)),
            rep("Predicción", length(forecast_ts6)),
            rep("Test", length(test_ts6))))

# Graficar
ggplot(df_plot6, aes(x=Fecha, y=Valor, color=Tipo)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=c("Train"=paleta[3], "Predicción"=paleta[4], "Test"=paleta[2])) +
  labs(title="Predicción IPC ARIMAX AutoARIMA", x="Año", y="IPC") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(hjust=0.5), legend.position="top")



#######################################################################################################
# -------------------- AJUSTAR MODELO ARIMAX MANUAL  (IPC) -----------------

#--------------------    AJUSTAR MODELO   -------------------------------
modelo_arimax_arima_ipc <- Arima(
  train_ipc_estacionaria_ARIMAX,
  order = c(1, 0, 0),
  xreg = train_exogenas_estacionarias)
summary(modelo_arimax_arima_ipc)

#--------------------     PREDICCION      ------------------------------
prediccion_arimax_arima_ipc <- forecast(
  modelo_arimax_arima_ipc,
  xreg = test_exogenas_estacionarias,
  h = nrow(test_ipc))

#-------------------   REVERTIR       -------------------------------------
forecast_arimax_arima_ipc_revertida <- diffinv(prediccion_arimax_arima_ipc$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arimax_arima_ipc_revertida <- forecast_arimax_arima_ipc_revertida[-1]

#-------------------     ACCURACY
accuracy_arimax_arima_ipc<- forecast::accuracy(forecast_arimax_arima_ipc_revertida, test_ipc)
accuracy_arimax_arima_ipc


# -------------------- GRAFICO 
train_ts7 <- ts(as.numeric(train_ipc),  start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts7 <- ts(as.numeric(test_ipc), start = c(2021, 1), frequency = 4)
forecast_ts7 <- ts(as.numeric(forecast_arimax_arima_ipc_revertida),  start = end(train_ts7) + c(0, 1), frequency = 4)

df_plot7 <- data.frame(
  Fecha = c(time(train_ts7), time(forecast_ts7), time(test_ts7)),
  Valor = c(as.numeric(train_ts7), as.numeric(forecast_ts7), as.numeric(test_ts7)),
  Tipo  = c(rep("Train", length(train_ts7)),
            rep("Predicción", length(forecast_ts7)),
            rep("Test", length(test_ts7))))

ggplot(df_plot7, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3], "Predicción" = paleta[4], "Test" = paleta[2])) +
  labs(title = "Predicción IPC ARIMAX Manual", x = "Año", y = "IPC") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")





####################################################################################################################################33
######################################     CORRELACIONES IPC     ###############################################################

###################################     ARIMAX - AUTOARIMA      ###################################################
# -------------------- ANALIZAR CORRELACIÓN --------------------
# Aplanar la serie dependiente (IPC)
ipc_vec <- as.vector(t(train_ipc_estacionaria_ARIMAX))  # filas x columnas → vector trimestral

# Aplanar las exógenas en el mismo orden y asegurar orden por fecha
exogenas_vec <- as.data.frame(train_exogenas_estacionarias)
exogenas_vec <- exogenas_vec[order(rownames(exogenas_vec)), ]

# Correlación
correlations <- cor(cbind(ipc_vec, exogenas_vec))
print("Correlaciones entre IPC y exógenas:")
print(correlations)

# Seleccionar solo exógenas con |cor| > umbral
umbral <- 0.06
exogenas_significativas_IPC <- exogenas_vec[, abs(correlations["ipc_vec", -1]) > umbral, drop=FALSE]

print("Exógenas seleccionadas para el modelo de IPC:")
print(colnames(exogenas_significativas_IPC))

# Seleccionar solo las variables significativas
train_exogenas_significativas_IPC <- train_exogenas_estacionarias[, colnames(exogenas_significativas_IPC), drop = FALSE]
test_exogenas_significativas_IPC <- test_exogenas_estacionarias[, colnames(exogenas_significativas_IPC), drop = FALSE]


#------------------      MODELO      -----------------------------
#CORRELACIONADAS
modelo_arimax_autoarima_ipc_correlacionada <- auto.arima(
  train_ipc_estacionaria_ARIMAX,
  seasonal = FALSE,
  d = 0,           # ya está estacionaria
  xreg = train_exogenas_significativas_IPC)
summary(modelo_arimax_autoarima_ipc_correlacionada)

#-----------------      PREDICCION     -----------------------
prediccion_arimax_autoarima_ipc_correlacionada<- forecast(
  modelo_arimax_autoarima_ipc_correlacionada,
  xreg = test_exogenas_significativas_IPC,
  h = length(test_ipc))
summary(prediccion_arimax_autoarima_ipc_correlacionada)

#-----------------    REVERTIR      ------------------------
forecast_arimax_autoarima_ipc_revertida_cor <- diffinv(prediccion_arimax_autoarima_ipc_correlacionada$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arimax_autoarima_ipc_revertida_cor <- forecast_arimax_autoarima_ipc_revertida_cor[-1]

#----------------      ACCURACY      ------------------------
accuracy_arimax_autoarim_ipc_cor<- forecast::accuracy(forecast_arimax_autoarima_ipc_revertida_cor, test_ipc)
accuracy_arimax_autoarim_ipc_cor

#-----------------    GRAFICO
train_ts8 <- ts(as.numeric(train_ipc), start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts8 <- ts(as.numeric(test_ipc), start = c(2021, 1), frequency = 4)
forecast_ts8 <- ts(as.numeric(forecast_arimax_autoarima_ipc_revertida_cor), start = end(train_ts8) + c(0, 1), frequency = 4)

df_plot8 <- data.frame(
  Fecha = c(time(train_ts8), time(forecast_ts8), time(test_ts8)),
  Valor = c(as.numeric(train_ts8), as.numeric(forecast_ts8), as.numeric(test_ts8)),
  Tipo  = c(rep("Train", length(train_ts8)),
            rep("Predicción", length(forecast_ts8)),
            rep("Test", length(test_ts8))))

ggplot(df_plot8, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3], "Predicción" = paleta[4], "Test" = paleta[2])) +
  labs(title = "Predicción IPC ARIMAX AutoARIMA Correlacionadas", x = "Año", y = "IPC") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")



###################################     ARIMAX - ARIMA (Manual)      ###################################################

#------------        MODELO       --------------------------
modelo_arimax_arima_ipc_correlacionada <- Arima(
  train_ipc_estacionaria_ARIMAX,
  order = c(1, 0, 0),
  xreg = train_exogenas_significativas_IPC
)
summary(modelo_arimax_arima_ipc_correlacionada)

#-------------      PREDICCION    ------------------------
prediccion_arimax_arima_ipc_correlacionada <- forecast(
  modelo_arimax_arima_ipc_correlacionada,
  xreg = test_exogenas_significativas_IPC,
  h = nrow(test_ipc))

#-------------     REVERTIR    ------------------------------
forecast_arimax_arima_ipc_revertida_cor<- diffinv(prediccion_arimax_arima_ipc_correlacionada$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arimax_arima_ipc_revertida_cor <- forecast_arimax_arima_ipc_revertida_cor[-1]

#----------------      ACCURACY      ------------------------
accuracy_arimax_arima_ipc_cor<- forecast::accuracy(forecast_arimax_arima_ipc_revertida_cor, test_ipc)
accuracy_arimax_arima_ipc_cor

#-----------------    GRAFICO
train_ts9 <- ts(as.numeric(train_ipc), start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts9 <- ts(as.numeric(test_ipc), start = c(2021, 1), frequency = 4)
forecast_ts9 <- ts(as.numeric(forecast_arimax_arima_ipc_revertida_cor), start = end(train_ts9) + c(0, 1), frequency = 4)

#-----------------  Crear dataframe para ggplot -----------------
df_plot9 <- data.frame(
  Fecha = c(time(train_ts9), time(forecast_ts9), time(test_ts9)),
  Valor = c(as.numeric(train_ts9), as.numeric(forecast_ts9), as.numeric(test_ts9)),
  Tipo  = c(rep("Train", length(train_ts9)),
            rep("Predicción", length(forecast_ts9)),
            rep("Test", length(test_ts9))))

#-----------------  Graficar -----------------
ggplot(df_plot9, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3], "Predicción" = paleta[4], "Test" = paleta[2])) +
  labs(title = "Predicción IPC ARIMAX Manual Correlacionadas", x = "Año", y = "IPC") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")





################################################################################################################################
#######################################################################################################################################
##############################################################################################
################                      ARIMAX PIB
##############################################################################################

################ ---------    PIB   ---------  #########

# -------------------- PREPARAR TRAIN EXÓGENAS --------------------
# Alinear fechas con train PIB
train_pib_estacionaria_ARIMAX <- window(train_pib_estacionaria, start = c(1999, 2))  # ajustar según inicio exógenas



##############################################################################################
#---------------------         ARIMAX- AUTOARIMA  PIB         --------------------------

# -------------------- LOG + DIFERENCIAS --------------------
train_pib_log <- log(train_pib)  # log para estabilizar varianza
# Primera diferencia
train_pib_diff1 <- diff(train_pib_log, differences = 1)
# Segunda diferencia con lag estacional (4)
train_pib_estacionaria_ARIMAX <- diff(train_pib_diff1, lag = 4)

# -------------------- AJUSTAR MODELO ARIMAX ----------------------
modelo_arimax_autoarima_pib <- auto.arima(
  train_pib_estacionaria_ARIMAX,
  seasonal = FALSE,
  d = 0,  # ya estacionaria
  xreg = train_exogenas_estacionarias
)

# -------------------- PREDICCIÓN -----------------
prediccion_arimax <- forecast(
  modelo_arimax_autoarima_pib,
  xreg = test_exogenas_estacionarias,
  h = nrow(test_exogenas_estacionarias)
)

# -------------------- REVERTIR DIFERENCIAS -----------------
# Revertir lag estacional (segunda diferencia)
revert_lag <- diffinv(prediccion_arimax$mean, differences = 1, lag = 4, xi = tail(train_pib_diff1, 4))

# Revertir primera diferencia
revert_diff1 <- diffinv(revert_lag, differences = 1, xi = tail(train_pib_log, 1))

# Revertir log
forecast_revertido <- exp(revert_diff1)
forecast_revertido <- window(forecast_revertido, start = c(2021, 1))
forecast_revertido

# --------------------  ACCURACY -----------------
accuracy_arimax_autoarima_pib <- forecast::accuracy(forecast_revertido, test_pib)
print(accuracy_arimax_autoarima_pib)


# ---------------      GRAFICAR            -----------------
train_ts10 <- ts(as.numeric(train_pib),  start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts10 <- ts(as.numeric(test_pib), start = c(2021, 1), frequency = 4)
forecast_ts10 <- ts(as.numeric(forecast_revertido),  start = end(train_ts10) + c(0, 1), frequency = 4)

df_plot10 <- data.frame(
  Fecha = c(time(train_ts10), time(forecast_ts10), time(test_ts10)),
  Valor = c(as.numeric(train_ts10), as.numeric(forecast_ts10), as.numeric(test_ts10)),
  Tipo  = c(rep("Train", length(train_ts10)),
            rep("Predicción", length(forecast_ts10)),
            rep("Test", length(test_ts10))))

ggplot(df_plot10, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3], "Predicción" = paleta[4], "Test" = paleta[2])) +
  labs(title = "Predicción PIB ARIMAX AutoARIMA", x = "Año", y = "PIB") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")


##############################################################################################
#---------------------         ARIMAX- ARIMA (Manual) PIB         --------------------------

# -------------------- LOG + DIFERENCIAS --------------------
train_pib_log <- log(train_pib)
train_pib_diff1 <- diff(train_pib_log, differences = 1)
train_pib_estacionaria_ARIMAX <- diff(train_pib_diff1, lag = 4)  # diferencia estacional

# -------------------- MODELO ARIMAX MANUAL -----------------
modelo_arimax_arima_pib <- Arima(
  train_pib_estacionaria_ARIMAX,
  order = c(1,0,0),
  xreg = train_exogenas_estacionarias
)

# -------------------- PREDICCIÓN -----------------
prediccion_arimax_arima_pib <- forecast(
  modelo_arimax_arima_pib,
  xreg = test_exogenas_estacionarias,
  h = nrow(test_exogenas_estacionarias)
)

# -------------------- REVERTIR DIFERENCIAS -----------------
# Revertir lag estacional
revert_lag <- diffinv(prediccion_arimax_arima_pib$mean, differences = 1, lag = 4, xi = tail(train_pib_diff1, 4))
# Revertir primera diferencia
revert_diff1 <- diffinv(revert_lag, differences = 1, xi = tail(train_pib_log, 1))
# Revertir log
forecast_arimax_arima_pib_revertida <- exp(revert_diff1)
forecast_arimax_arima_pib_revertida <- window(forecast_arimax_arima_pib_revertida, start = c(2021, 1))
forecast_arimax_arima_pib_revertida


# -------------------- ACCURACY -----------------
accuracy_arimax_arima_pib <- forecast::accuracy(forecast_arimax_arima_pib_revertida, test_pib)
print(accuracy_arimax_arima_pib)


# --------------------    GRAFICO
train_ts11 <- ts(as.numeric(train_pib),  start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts11 <- ts(as.numeric(test_pib), start = c(2021, 1), frequency = 4)
forecast_ts11 <- forecast_arimax_arima_pib_revertida

df_plot11 <- data.frame(
  Fecha = c(time(train_ts11), time(forecast_ts11), time(test_ts11)),
  Valor = c(as.numeric(train_ts11), as.numeric(forecast_ts11), as.numeric(test_ts11)),
  Tipo  = c(rep("Train", length(train_ts11)),
            rep("Predicción", length(forecast_ts11)),
            rep("Test", length(test_ts11))))

ggplot(df_plot11, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3], "Predicción" = paleta[4], "Test" = paleta[2])) +
  labs(title = "Predicción PIB ARIMAX Manual", x = "Año", y = "PIB") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top")






###############################################################################################################################
################################     CORRELACIONADAS PIB       ################################################################


# -------------------- ANALIZAR CORRELACIÓN PARA PIB --------------------
# Aplanar la serie dependiente (PIB)
pib_vec <- as.vector(t(train_pib_estacionaria_ARIMAX))  # filas x columnas → vector trimestral

# Aplanar las exógenas en el mismo orden y asegurar orden por fecha
exogenas_vec <- as.data.frame(train_exogenas_estacionarias)
exogenas_vec <- exogenas_vec[order(rownames(exogenas_vec)), ]

# Correlación
correlaciones_pib <- cor(cbind(pib_vec, exogenas_vec))
print("Correlaciones entre PIB y exógenas:")
print(correlaciones_pib)

# Seleccionar solo exógenas con |cor| > umbral
umbral <- 0.025
exogenas_significativas_PIB <- exogenas_vec[, abs(correlaciones_pib["pib_vec", -1]) > umbral, drop=FALSE]

print("Exógenas seleccionadas para el modelo de PIB:")
print(colnames(exogenas_significativas_PIB))

# Seleccionar solo las variables significativas
train_exogenas_significativas_PIB <- train_exogenas_estacionarias[, colnames(exogenas_significativas_PIB), drop = FALSE]
test_exogenas_significativas_PIB <- test_exogenas_estacionarias[, colnames(exogenas_significativas_PIB), drop = FALSE]


##################################################################################################################################
#--------------------------------     MODELO ARIMAX - AUTOARIMA PIB    ------------------------------------------------------
# ------------ MODELO
modelo_arimax_autoarima_pib_correlacionada <- auto.arima(
  train_pib_estacionaria_ARIMAX,
  seasonal = FALSE,
  d = 0,
  xreg = train_exogenas_significativas_PIB
)

# ------------ PREDICCION
prediccion_arimax_autoarima_pib_cor <- forecast(
  modelo_arimax_autoarima_pib_correlacionada,
  xreg = test_exogenas_significativas_PIB,
  h = length(test_pib)
)

#---   Revertir diferencias
# Revertir lag estacional (segunda diferencia)
revert_lag <- diffinv(prediccion_arimax_autoarima_pib_cor$mean, differences = 1, lag = 4, xi = tail(train_pib_diff1, 4))

# Revertir primera diferencia
revert_diff1 <- diffinv(revert_lag, differences = 1, xi = tail(train_pib_log, 1))

# Revertir log
forecast_revertido_cor <- exp(revert_diff1)
forecast_revertido_cor <- window(forecast_revertido_cor, start = c(2021,1))

# -------------------- EVALUAR ACCURACY -----------------
accuracy_arimax_autoarima_pib_cor <- forecast::accuracy(forecast_revertido_cor, test_pib)
print(accuracy_arimax_autoarima_pib_cor)


# --------------------   GRAFICO
train_ts13 <- ts(as.numeric(train_pib), start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts13  <- ts(as.numeric(test_pib),  start = c(2021, 1), frequency = 4)
forecast_ts13 <- ts(as.numeric(forecast_revertido_cor),
                    start = end(train_ts13) + c(0, 1),
                    frequency = 4)

# Combinar en un único dataframe
df_plot13 <- data.frame(
  Fecha = c(time(train_ts13), time(forecast_ts13), time(test_ts13)),
  Valor = c(as.numeric(train_ts13), as.numeric(forecast_ts13), as.numeric(test_ts13)),
  Tipo  = c(rep("Train", length(train_ts13)),
            rep("Predicción", length(forecast_ts13)),
            rep("Test", length(test_ts13)))
)

# Graficar con ggplot2
ggplot(df_plot13, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3],
                                "Predicción" = paleta[4],
                                "Test" = paleta[2])) +
  labs(title = "Predicción PIB - ARIMAX AutoArima (Correlacionadas)",
       x = "Año", y = "PIB") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")


##################################################################################################################################
#--------------------------------     MODELO ARIMAX - ARIMA (Manual) PIB    ------------------------------------------------------
########## CORRELACIONADAS

modelo_arimax_arima_pib_cor <- Arima(
  train_pib_estacionaria_ARIMAX,
  order = c(1,0,0),
  xreg = train_exogenas_significativas_PIB
)

# Predicción
prediccion_arimax_arima_pib_cor <- forecast(
  modelo_arimax_arima_pib_cor,
  xreg = test_exogenas_significativas_PIB,
  h = nrow(test_exogenas_significativas_PIB)
)

# Revertir diferencias
revert_lag_cor <- diffinv(prediccion_arimax_arima_pib_cor$mean, differences = 1, lag = 4, xi = tail(train_pib_diff1, 4))
revert_diff1_cor <- diffinv(revert_lag_cor, differences = 1, xi = tail(train_pib_log, 1))
forecast_arimax_arima_pib_revertida_cor <- exp(revert_diff1_cor)
forecast_arimax_arima_pib_revertida_cor <- window(forecast_arimax_arima_pib_revertida_cor, start= c(2021,1))

# Accuracy
accuracy_arimax_arima_pib_cor <- forecast::accuracy(forecast_arimax_arima_pib_revertida_cor, test_pib)
print(accuracy_arimax_arima_pib_cor)

#-------- GRAFICO
train_ts14 <- ts(as.numeric(train_pib), start = c(1999, 2), end = c(2020, 4), frequency = 4)
test_ts14  <- ts(as.numeric(test_pib),  start = c(2021, 1), frequency = 4)
forecast_ts14 <- ts(as.numeric(forecast_arimax_arima_pib_revertida_cor),
                    start = end(train_ts14) + c(0, 1),
                    frequency = 4)

# Combinar en un único dataframe
df_plot14 <- data.frame(
  Fecha = c(time(train_ts14), time(forecast_ts14), time(test_ts14)),
  Valor = c(as.numeric(train_ts14), as.numeric(forecast_ts14), as.numeric(test_ts14)),
  Tipo  = c(rep("Train", length(train_ts14)),
            rep("Predicción", length(forecast_ts14)),
            rep("Test", length(test_ts14)))
)

# Graficar
ggplot(df_plot14, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = paleta[3],
                                "Predicción" = paleta[4],
                                "Test" = paleta[2])) +
  labs(title = "Predicción PIB - ARIMAX ARIMA (Correlacionadas)",
       x = "Año", y = "PIB") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")






##########################################################################################################################################################################
#################################################################################################################################################################################
############################################            RESULTADOS   MODELOS        ####################################################################################################
#Seleccion mejor modelo para cada variable:

accuracy_final_ipc <- data.frame(
  Modelo = c("AutoARIMA", "ARIMA", "SARIMA", "ARIMAX AutoARIMA", "ARIMAX ARIMA", 
             "ARIMAX AutoARIMA Corr", "ARIMAX ARIMA Corr"),
  ME   = c(acc_autoarima["Test set","ME"],
           acc_arima["Test set","ME"],
           acc_sarima_ipc["Test set","ME"],
           accuracy_arimax_autoarim_ipc["Test set","ME"],
           accuracy_arimax_arima_ipc["Test set","ME"],
           accuracy_arimax_autoarim_ipc_cor["Test set","ME"],
           accuracy_arimax_arima_ipc_cor["Test set","ME"]),
  RMSE = c(acc_autoarima["Test set","RMSE"],
           acc_arima["Test set","RMSE"],
           acc_sarima_ipc["Test set","RMSE"],
           accuracy_arimax_autoarim_ipc["Test set","RMSE"],
           accuracy_arimax_arima_ipc["Test set","RMSE"],
           accuracy_arimax_autoarim_ipc_cor["Test set","RMSE"],
           accuracy_arimax_arima_ipc_cor["Test set","RMSE"]),
  MAE  = c(acc_autoarima["Test set","MAE"],
           acc_arima["Test set","MAE"],
           acc_sarima_ipc["Test set","MAE"],
           accuracy_arimax_autoarim_ipc["Test set","MAE"],
           accuracy_arimax_arima_ipc["Test set","MAE"],
           accuracy_arimax_autoarim_ipc_cor["Test set","MAE"],
           accuracy_arimax_arima_ipc_cor["Test set","MAE"]),
  MAPE = c(acc_autoarima["Test set","MAPE"],
           acc_arima["Test set","MAPE"],
           acc_sarima_ipc["Test set","MAPE"],
           accuracy_arimax_autoarim_ipc["Test set","MAPE"],
           accuracy_arimax_arima_ipc["Test set","MAPE"],
           accuracy_arimax_autoarim_ipc_cor["Test set","MAPE"],
           accuracy_arimax_arima_ipc_cor["Test set","MAPE"]))
accuracy_final_ipc <- accuracy_final_ipc[order(accuracy_final_ipc$RMSE), ]
accuracy_final_ipc 
#Autoarima (El mejor)--> Se ha usado Arima (Mas completo y mas de que hablar)


accuracy_final_pib <- data.frame(
  Modelo = c("AutoARIMA", "ARIMA", "SARIMA", "ARIMAX AutoARIMA", "ARIMAX ARIMA", 
             "ARIMAX AutoARIMA Corr", "ARIMAX ARIMA Corr"),
  ME   = c(acc_autoarima_pib["Test set","ME"],
           acc_arima_pib["Test set","ME"],
           acc_sarima_pib["Test set","ME"],
           accuracy_arimax_autoarima_pib["Test set","ME"],
           accuracy_arimax_arima_pib["Test set","ME"],
           accuracy_arimax_autoarima_pib_cor["Test set","ME"],
           accuracy_arimax_arima_pib_cor["Test set","ME"]),
  RMSE = c(acc_autoarima_pib["Test set","RMSE"],
           acc_arima_pib["Test set","RMSE"],
           acc_sarima_pib["Test set","RMSE"],
           accuracy_arimax_autoarima_pib["Test set","RMSE"],
           accuracy_arimax_arima_pib["Test set","RMSE"],
           accuracy_arimax_autoarima_pib_cor["Test set","RMSE"],
           accuracy_arimax_arima_pib_cor["Test set","RMSE"]),
  MAE  = c(acc_autoarima_pib["Test set","MAE"],
           acc_arima_pib["Test set","MAE"],
           acc_sarima_pib["Test set","MAE"],
           accuracy_arimax_autoarima_pib["Test set","MAE"],
           accuracy_arimax_arima_pib["Test set","MAE"],
           accuracy_arimax_autoarima_pib_cor["Test set","MAE"],
           accuracy_arimax_arima_pib_cor["Test set","MAE"]),
  MAPE = c(acc_autoarima_pib["Test set","MAPE"],
           acc_arima_pib["Test set","MAPE"],
           acc_sarima_pib["Test set","MAPE"],
           accuracy_arimax_autoarima_pib["Test set","MAPE"],
           accuracy_arimax_arima_pib["Test set","MAPE"],
           accuracy_arimax_autoarima_pib_cor["Test set","MAPE"],
           accuracy_arimax_arima_pib_cor["Test set","MAPE"]))
accuracy_final_pib <- accuracy_final_pib[order(accuracy_final_pib$RMSE), ]
accuracy_final_pib
#Arimax Arima Correlacinadas


########------------------------ GRAFICOS ACCURACY

#   ---->  SARIMA IPC
df_arima <- accuracy_final_ipc[accuracy_final_ipc$Modelo == "ARIMA", c("ME","RMSE","MAE","MAPE")]
df_arima_long <- melt(df_arima, variable.name = "Métrica", value.name = "Valor")

# Gráfico usando tu paleta
ggplot(df_arima_long, aes(x = Métrica, y = Valor, fill = Métrica)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Valor, 3)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = paleta[c(2,4,1,3)]) +
  labs(title = "Métricas del modelo IPC – ARIMA",
       y = "Valor del error", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12))


#  ---> ARIMAX SARIMA PIB

df_arima_pib <- accuracy_final_pib[accuracy_final_pib$Modelo == "ARIMAX ARIMA", c("ME","RMSE","MAE","MAPE")]
df_arima_pib_long <- melt(df_arima_pib, variable.name = "Métrica", value.name = "Valor")

# Gráfico con tu paleta
ggplot(df_arima_pib_long, aes(x = Métrica, y = Valor, fill = Métrica)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Valor, 3)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = paleta[1:4]) +
  labs(title = "Métricas del modelo PIB – ARIMAX ARIMA",
       y = "Valor del error", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12))





#=================================================================================================================================================================================


# ###################################################################################################################################
# #########################               CROSS - VALIDATION PARA IPC Y PIB                   ###############################
# ###################################################################################################################################


##########################################################
### CROSS-VALIDATION IPC (ARIMA, SARIMA y ARIMAX)
##########################################################

h <- 1
train_size <- 20  # Ajusta según tus datos
ipc_ts <- ts(na.omit(as.numeric(datos_limpios_AUS$Consumer.Price.Index..CPI.)),
             start = c(1996,1), frequency = 4)
n <- length(ipc_ts)  # ipc_ts = serie original IPC

# Vectores de predicciones
pred_autoarima_ipc_cv    <- rep(NA, n - train_size)
pred_arima_manual_ipc_cv <- rep(NA, n - train_size)
pred_sarima_manual_ipc_cv<- rep(NA, n - train_size)
pred_arimax_auto_ipc_cv  <- rep(NA, n - train_size)
pred_arimax_manual_ipc_cv<- rep(NA, n - train_size)

# Vectores para guardar AIC
aic_autoarima_ipc_cv    <- rep(NA, n - train_size)
aic_arima_manual_ipc_cv <- rep(NA, n - train_size)
aic_sarima_manual_ipc_cv<- rep(NA, n - train_size)
aic_arimax_auto_ipc_cv  <- rep(NA, n - train_size)
aic_arimax_manual_ipc_cv<- rep(NA, n - train_size)

# Rolling forecast cross-validation
for(i in train_size:(n-1)) {
  
  # --- Serie de entrenamiento ---
  train_ipc_cv <- window(ipc_ts, end = time(ipc_ts)[i])
  train_ipc_est_cv <- diff(train_ipc_cv, differences = 1)
  last_val <- tail(train_ipc_cv, 1)
  
  ##############################
  #  AUTOARIMA
  ##############################
  fit_autoarima <- auto.arima(train_ipc_est_cv, seasonal = FALSE, d = 0)
  aic_autoarima_ipc_cv[i - train_size + 1] <- fit_autoarima$aic
  fc_autoarima <- forecast(fit_autoarima, h = h)
  pred_autoarima_ipc_cv[i - train_size + 1] <- diffinv(fc_autoarima$mean, differences = 1, xi = last_val)[2]
  
  ##############################
  # ARIMA MANUAL
  ##############################
  fit_arima_manual <- try(arima(train_ipc_est_cv, order = c(1,0,0)), silent = TRUE)
  if(!inherits(fit_arima_manual, "try-error")) {
    aic_arima_manual_ipc_cv[i - train_size + 1] <- fit_arima_manual$aic
    fc_arima_manual <- forecast(fit_arima_manual, h = h)
    pred_arima_manual_ipc_cv[i - train_size + 1] <- diffinv(fc_arima_manual$mean, differences = 1, xi = last_val)[2]
  }
  
  ##############################
  # SARIMA MANUAL
  ##############################
  fit_sarima_manual <- try(arima(train_ipc_est_cv,
                                 order = c(0,0,1),
                                 seasonal = list(order = c(1,0,0), period = 4),
                                 method = "ML"), silent = TRUE)
  if(!inherits(fit_sarima_manual, "try-error")) {
    aic_sarima_manual_ipc_cv[i - train_size + 1] <- fit_sarima_manual$aic
    fc_sarima_manual <- forecast(fit_sarima_manual, h = h)
    pred_sarima_manual_ipc_cv[i - train_size + 1] <- diffinv(fc_sarima_manual$mean, differences = 1, xi = last_val)[2]
  }
  
  ##############################
  # ARIMAX AUTOARIMA
  ##############################
  len_train_est <- length(train_ipc_est_cv)
  if(nrow(train_exogenas_estacionarias) >= len_train_est) {
    X_train_cv <- train_exogenas_estacionarias[(nrow(train_exogenas_estacionarias) - len_train_est + 1):nrow(train_exogenas_estacionarias), , drop=FALSE]
  } else { X_train_cv <- NULL }
  
  X_next_cv <- NULL
  if(!is.null(X_train_cv) && nrow(test_exogenas_estacionarias) >= 1) {
    X_next_cv <- test_exogenas_estacionarias[1, , drop=FALSE]
  }
  
  fit_arimax_auto <- try(auto.arima(train_ipc_est_cv, xreg = X_train_cv, seasonal = FALSE, d = 0), silent = TRUE)
  if(!inherits(fit_arimax_auto, "try-error")) {
    aic_arimax_auto_ipc_cv[i - train_size + 1] <- fit_arimax_auto$aic
    if(!is.null(X_next_cv)) {
      fc_arimax_auto <- forecast(fit_arimax_auto, xreg = X_next_cv, h = h)
      pred_arimax_auto_ipc_cv[i - train_size + 1] <- diffinv(fc_arimax_auto$mean, differences = 1, xi = last_val)[2]
    }
  }
  
  ##############################
  # ARIMAX MANUAL
  ##############################
  fit_arimax_manual <- try(Arima(train_ipc_est_cv, order = c(1,0,0), xreg = X_train_cv), silent = TRUE)
  if(!inherits(fit_arimax_manual, "try-error")) {
    aic_arimax_manual_ipc_cv[i - train_size + 1] <- fit_arimax_manual$aic
    if(!is.null(X_next_cv)) {
      fc_arimax_manual <- forecast(fit_arimax_manual, xreg = X_next_cv, h = h)
      pred_arimax_manual_ipc_cv[i - train_size + 1] <- diffinv(fc_arimax_manual$mean, differences = 1, xi = last_val)[2]
    }
  }
  
  # Remover la primera fila de test_exogenas para la siguiente iteración
  if(!is.null(test_exogenas_estacionarias) && nrow(test_exogenas_estacionarias) > 1){
    test_exogenas_estacionarias <- test_exogenas_estacionarias[-1, , drop=FALSE]
  }
}

# Calcular errores
###################
actual_cv <- window(ipc_ts, start = time(ipc_ts)[train_size + 1])

metrics <- function(errors, actual) {
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae  <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

df_metrics_ipc_cv <- data.frame(
  Modelo = c("AUTO.ARIMA_CV", "ARIMA_manual_CV", "SARIMA_manual_CV", "ARIMAX_auto_CV", "ARIMAX_manual_CV"),
  RMSE = c(
    metrics(actual_cv - pred_autoarima_ipc_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_arima_manual_ipc_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_sarima_manual_ipc_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_arimax_auto_ipc_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_arimax_manual_ipc_cv, actual_cv)["RMSE"] ),
  MAE = c(
    metrics(actual_cv - pred_autoarima_ipc_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_arima_manual_ipc_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_sarima_manual_ipc_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_arimax_auto_ipc_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_arimax_manual_ipc_cv, actual_cv)["MAE"] ),
  MAPE = c(
    metrics(actual_cv - pred_autoarima_ipc_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_arima_manual_ipc_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_sarima_manual_ipc_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_arimax_auto_ipc_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_arimax_manual_ipc_cv, actual_cv)["MAPE"] ),
  AIC = c(
    mean(aic_autoarima_ipc_cv, na.rm = TRUE),
    mean(aic_arima_manual_ipc_cv, na.rm = TRUE),
    mean(aic_sarima_manual_ipc_cv, na.rm = TRUE),
    mean(aic_arimax_auto_ipc_cv, na.rm = TRUE),
    mean(aic_arimax_manual_ipc_cv, na.rm = TRUE))
)

print(df_metrics_ipc_cv)




##########################################################
###  CROSS-VALIDATION PIB  (ARIMA, SARIMA, ARIMAX)
##########################################################

# Parámetros
h <- 2         # pasos a predecir
train_size <- 20 # tamaño inicial del train

# Series originales (sin outliers, sin diferenciar)
gdp_ts_trimestral <- ts(na.omit(as.numeric(datos_limpios_AUS$GDP.billion.currency.units)), start = c(1996,1), frequency = 4)
money_supply_ts_trimestral <- ts(na.omit(as.numeric(datos_limpios_AUS$Money.supply.billion.currency.units)), start = c(1996,1), frequency = 4)
stock_market_ts_trimestral <- ts(na.omit(as.numeric(datos_limpios_AUS$Stock.market.index)), start = c(1996,1), frequency = 4)
unemployment_ts_trimestral <- ts(na.omit(as.numeric(datos_limpios_AUS$Unemployment.rate.percent)), start = c(1996,1), frequency = 4)

n <- length(gdp_ts_trimestral)

# Crear vectores de predicciones
pred_arima_manual_cv     <- rep(NA, n - train_size)
pred_sarima_manual_cv    <- rep(NA, n - train_size)
pred_autoarima_cv        <- rep(NA, n - train_size)
pred_arimax_auto_cv      <- rep(NA, n - train_size)
pred_arimax_manual_cv    <- rep(NA, n - train_size)

# Crear vectores para AIC
aic_arima_manual_cv     <- rep(NA, n - train_size)
aic_sarima_manual_cv    <- rep(NA, n - train_size)
aic_autoarima_cv        <- rep(NA, n - train_size)
aic_arimax_auto_cv      <- rep(NA, n - train_size)
aic_arimax_manual_cv    <- rep(NA, n - train_size)

##########################################################
# Rolling forecast cross-validation
##########################################################
for(i in train_size:(n-1)) {
  
  # --- Series de entrenamiento hasta el punto i ---
  train_PIB_cv <- window(gdp_ts_trimestral, end = time(gdp_ts_trimestral)[i])
  
  # Exógenas hasta ese punto
  train_MS_cv  <- window(money_supply_ts_trimestral, end = time(money_supply_ts_trimestral)[i])
  train_SM_cv  <- window(stock_market_ts_trimestral, end = time(stock_market_ts_trimestral)[i])
  train_UR_cv  <- window(unemployment_ts_trimestral, end = time(unemployment_ts_trimestral)[i])
  
  # Exógenas para el siguiente paso
  next_MS <- money_supply_ts_trimestral[i + 1]
  next_SM <- stock_market_ts_trimestral[i + 1]
  next_UR <- unemployment_ts_trimestral[i + 1]
  
  # Matriz de exógenas completas
  X_train_cv <- as.matrix(cbind(train_MS_cv, train_SM_cv, train_UR_cv))
  X_next_cv  <- matrix(c(next_MS, next_SM, next_UR), nrow = 1)
  
  ##############################
  # ARIMA MANUAL (2,0,2)
  ##############################
  fit_manual <- try(arima(diff(diff(log(train_PIB_cv), lag=4)), order = c(2,0,2)), silent = TRUE)
  if(!inherits(fit_manual, "try-error")) {
    aic_arima_manual_cv[i - train_size + 1] <- fit_manual$aic
    fc_manual <- forecast(fit_manual, h = h)
    
    # Revertir diferencias
    last_vals_lag4 <- tail(log(train_PIB_cv), 4)
    last_val_diff1 <- tail(diff(log(train_PIB_cv), lag=4), 1)
    pred_diff1_inv <- diffinv(fc_manual$mean, differences = 1, xi = last_val_diff1)
    pred_inv <- diffinv(pred_diff1_inv, differences = 1, lag = 4, xi = last_vals_lag4)
    
    pred_arima_manual_cv[i - train_size + 1] <- exp(pred_inv[5])
  }
  
  ##############################
  # SARIMA MANUAL (2,0,2)(0,0,1)[4]
  ##############################
  fit_sarima_manual_cv <- try(arima(log(train_PIB_cv),
                                    order = c(2,0,2),
                                    seasonal = list(order = c(0,0,1), period = 4)),
                              silent = TRUE)
  if(!inherits(fit_sarima_manual_cv, "try-error")) {
    aic_sarima_manual_cv[i - train_size + 1] <- fit_sarima_manual_cv$aic
    fc_sarima_manual_cv <- forecast(fit_sarima_manual_cv, h = h)
    pred_sarima_manual_cv[i - train_size + 1] <- exp(as.numeric(fc_sarima_manual_cv$mean))
  }
  
  ##############################
  # AUTO.ARIMA (sin exógenas)
  ##############################
  fit_auto <- auto.arima(log(train_PIB_cv), seasonal = FALSE)
  aic_autoarima_cv[i - train_size + 1] <- fit_auto$aic
  pred_autoarima_cv[i - train_size + 1] <- exp(as.numeric(forecast(fit_auto, h = h)$mean))
  
  ##############################
  # ARIMAX AUTO (3 exógenas)
  ##############################
  fit_arimax_auto <- try(auto.arima(log(train_PIB_cv),
                                    xreg = X_train_cv,
                                    seasonal = FALSE,
                                    stepwise = FALSE,
                                    approximation = TRUE),
                         silent = TRUE)
  if(!inherits(fit_arimax_auto, "try-error")) {
    aic_arimax_auto_cv[i - train_size + 1] <- fit_arimax_auto$aic
    fc_arimax_auto <- forecast(fit_arimax_auto, xreg = X_next_cv, h = h)
    pred_arimax_auto_cv[i - train_size + 1] <- exp(as.numeric(fc_arimax_auto$mean))
  }
  
  ##############################
  # ARIMAX MANUAL
  ##############################
  train_PIB_log_cv <- log(train_PIB_cv)
  train_PIB_est_cv <- diff(diff(train_PIB_log_cv, lag = 4))
  
  # Exógenas diferenciadas
  train_MS_est_cv <- diff(train_MS_cv, differences = 1)
  train_SM_est_cv <- diff(train_SM_cv, differences = 1)
  train_UR_est_cv <- diff(train_UR_cv, differences = 1)
  
  # Alinear longitudes
  len_PIB <- length(train_PIB_est_cv)
  train_MS_est_cv <- tail(train_MS_est_cv, len_PIB)
  train_SM_est_cv <- tail(train_SM_est_cv, len_PIB)
  train_UR_est_cv <- tail(train_UR_est_cv, len_PIB)
  
  X_train_est_cv <- as.matrix(cbind(train_MS_est_cv, train_SM_est_cv, train_UR_est_cv))
  
  # Para el siguiente paso
  X_next_est_cv <- matrix(c(
    tail(diff(train_MS_cv, differences=1),1),
    tail(diff(train_SM_cv, differences=1),1),
    tail(diff(train_UR_cv, differences=1),1)
  ), nrow = 1)
  
  fit_arimax_manual <- try(arima(train_PIB_est_cv, order = c(2,0,2), xreg = X_train_est_cv), silent=TRUE)
  if(!inherits(fit_arimax_manual, "try-error")) {
    aic_arimax_manual_cv[i - train_size + 1] <- fit_arimax_manual$aic
    fc_arimax_manual <- predict(fit_arimax_manual, n.ahead = h, newxreg = X_next_est_cv)
    
    # Revertir la diferencia simple
    last_val_diff1 <- tail(diff(train_PIB_log_cv, lag=4), 1)
    pred_diff1_inv <- diffinv(fc_arimax_manual$pred, differences = 1, xi = last_val_diff1)
    
    # Revertir la diferencia estacional
    last_vals_lag4 <- tail(train_PIB_log_cv, 4)
    pred_inv <- diffinv(pred_diff1_inv, differences = 1, lag = 4, xi = last_vals_lag4)
    
    # Guardar predicción
    pred_arimax_manual_cv[i - train_size + 1] <- exp(pred_inv[5])
  }
}

##########################################################
# Cálculo de errores y métricas
##########################################################
actual_cv <- window(gdp_ts_trimestral, start = time(gdp_ts_trimestral)[train_size + 1])

metrics <- function(errors, actual) {
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae  <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actual), na.rm = TRUE) * 100
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

df_metrics_cv <- data.frame(
  Modelo = c("ARIMA_manual_CV", "SARIMA_manual_CV", "AUTO.ARIMA_CV",
             "ARIMAX_auto_CV", "ARIMAX_manual_CV"),
  RMSE = c(
    metrics(actual_cv - pred_arima_manual_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_sarima_manual_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_autoarima_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_arimax_auto_cv, actual_cv)["RMSE"],
    metrics(actual_cv - pred_arimax_manual_cv, actual_cv)["RMSE"]),
  MAE = c(
    metrics(actual_cv - pred_arima_manual_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_sarima_manual_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_autoarima_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_arimax_auto_cv, actual_cv)["MAE"],
    metrics(actual_cv - pred_arimax_manual_cv, actual_cv)["MAE"]),
  MAPE = c(
    metrics(actual_cv - pred_arima_manual_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_sarima_manual_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_autoarima_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_arimax_auto_cv, actual_cv)["MAPE"],
    metrics(actual_cv - pred_arimax_manual_cv, actual_cv)["MAPE"]),
  AIC = c(
    mean(aic_arima_manual_cv, na.rm = TRUE),
    mean(aic_sarima_manual_cv, na.rm = TRUE),
    mean(aic_autoarima_cv, na.rm = TRUE),
    mean(aic_arimax_auto_cv, na.rm = TRUE),
    mean(aic_arimax_manual_cv, na.rm = TRUE))
)

print(df_metrics_cv)





           
#=========================================================================================================================================================================

#######################################################################################################################################################################
####################################            PREDICCION FINAL             ######################################################################


#-------------------------                CPI (Modelo ARIMA)                   --------------------------------------------

#Serie orginal 
series_IPC_trimestrales <- readRDS("DATOS/Series_Temporales/Trimestrales/cpi_ts_trimestral.rds")

#Aplicaremos primera diferencia (Como hicimos anteriormente)
IPC_estacionaria <- diff(series_IPC_trimestrales, differences = 1)
tsdisplay(IPC_estacionaria)

#Aplicamos el modelos osbre la series diferenciada
modelo_final_arima_IPC <- arima(IPC_estacionaria, order=c(1,0,0))  # d=0 porque ya diferenciamos anteriormente. y no necesitamos diferenciarlo otra vez
summary(modelo_final_arima_IPC)
# Extraer AIC, AICc y BIC
aic_final_IPC <-modelo_final_arima_IPC$aic    # AIC
aic_final_IPC
k <- length(modelo_final_arima_IPC$coef)  # ar + intercept
n <- length(series_IPC_trimestrales) # tamaño de la muestra
aicc_final_IPC <- aic_final_IPC + (2 * k * (k + 1)) / (n - k - 1)
aicc_final_IPC
bic_final_IPC <- aicc_final_IPC + k * log(n)
bic_final_IPC


# Validación de residuales
hist(residuals(modelo_final_arima_IPC), main="Histograma de residuales ARIMA IPC", xlab="Residual", col=paleta[2])
#png("checkresiduals_IPC.png", width = 1800, height = 1400, res = 150)
checkresiduals(modelo_final_arima_IPC)
#dev.off()

checkresiduals(modelo_final_arima_IPC)

boxtest_arima_ipc <- Box.test(residuals(modelo_final_arima_IPC), lag = round(log(length(series_IPC_trimestrales))), type="Ljung-Box")
if (boxtest_arima_ipc$p.value > 0.05) {
  cat("Residuos ARIMA IPC parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}
#Residuos ARIMA IPC parecen ruido blanco

#-----------------      PREDICCIONES
prediccion_arima_IPC <- forecast(modelo_final_arima_IPC, h=2, level=90)
autoplot(prediccion_arima_IPC) + ggtitle("Predicción IPC con ARIMA") + ylab("IPC") + xlab("Trimestre") + theme_minimal()

#-----------------      REVERTIR
# La serie original fue diferenciada 1 vez, usamos diffinv
forecast_arima_ipc_revertida1 <- diffinv(prediccion_arima_IPC$mean, differences = 1, xi=tail(series_IPC_trimestrales, 1))
forecast_arima_ipc_revertida1 <- forecast_arima_ipc_revertida1[-1]
prediccion_final_Arima_IPC<- forecast_arima_ipc_revertida1


# ---------------- JUNTAR DF (ORIGINAL + PREDICCION)
# Obtener el tiempo final de la serie
tiempo_final <- end(series_IPC_trimestrales)  # año y trimestre
# Crear serie de predicciones como ts, frecuencia 4 (trimestral)
predicciones_ts <- ts(prediccion_final_Arima_IPC, start = c(tiempo_final[1], tiempo_final[2]+1), frequency = 4)
# Unir series históricas y predicciones
df_IPC_Completo <- ts(c(series_IPC_trimestrales, predicciones_ts), start = start(series_IPC_trimestrales), frequency = 4)
df_IPC_Completo


#-----------------      GRAFICO FINAL
h <- 2  # horizonte de predicción
prediccion_arima_IPC <- forecast(modelo_final_arima_IPC, h = h, level = 90)

df_total <- data.frame(
  Trimestre = as.numeric(time(series_IPC_trimestrales)),
  IPC = as.numeric(series_IPC_trimestrales))

# Revertir la diferencia usando diffinv
ultimo_valor <- tail(series_IPC_trimestrales,1)
pred_revertida <- diffinv(prediccion_arima_IPC$mean, differences = 1, xi = ultimo_valor)[-1]

# Revertir bandas de confianza
lower_rev <- diffinv(prediccion_arima_IPC$lower[,1], differences = 1, xi = ultimo_valor)[-1]
upper_rev <- diffinv(prediccion_arima_IPC$upper[,1], differences = 1, xi = ultimo_valor)[-1]

tiempo_final <- end(series_IPC_trimestrales)

# Crear serie de predicciones como ts
predicciones_ts <- ts(pred_revertida,
                      start = c(tiempo_final[1], tiempo_final[2]+1),
                      frequency = 4)

# Data frame de predicción
df_pred <- data.frame(
  Trimestre = time(predicciones_ts),
  Pred = as.numeric(predicciones_ts),
  Lower = as.numeric(diffinv(prediccion_arima_IPC$lower[,1], differences = 1, xi = tail(series_IPC_trimestrales,1))[-1]),
  Upper = as.numeric(diffinv(prediccion_arima_IPC$upper[,1], differences = 1, xi = tail(series_IPC_trimestrales,1))[-1])
)
p_main <- ggplot(df_total, aes(x = Trimestre, y = IPC)) +
  geom_line(color = paleta[2], linewidth = 1) +
  geom_line(data = df_pred, aes(x = Trimestre, y = Pred), color = paleta[4], linetype = "dashed", inherit.aes = FALSE) +
  geom_ribbon(data = df_pred, aes(x = Trimestre, ymin = Lower, ymax = Upper), fill = paleta[2], alpha = 0.2, inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicción IPC con ARIMA (1,0,0)",  # Título general
    y = "IPC",
    x = "Trimestre")

# Zoom últimos trimestres
x_min <- 2021
x_max <- 2023

# Etiquetas trimestrales
df_quarters <- expand.grid(year = 2021:2023, q = 1:4)
df_quarters$pos <- df_quarters$year + (df_quarters$q-1)/4
df_quarters <- df_quarters[order(df_quarters$pos), ]
breaks_all <- df_quarters$pos
labels_all <- paste0(df_quarters$year, " Q", df_quarters$q)

p_zoom <- ggplot() +
  geom_line(data = df_total, aes(x = Trimestre, y = IPC), color = paleta[2]) +
  geom_line(data = df_pred, aes(x = Trimestre, y = Pred), color = paleta[4], linetype = "dashed", inherit.aes = FALSE) +
  geom_ribbon(data = df_pred, aes(x = Trimestre, ymin = Lower, ymax = Upper), fill = paleta[2], alpha = 0.2, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  scale_x_continuous(
    breaks = breaks_all[breaks_all >= x_min & breaks_all <= x_max],
    labels = labels_all[breaks_all >= x_min & breaks_all <= x_max]
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.title = element_blank(),
        axis.text.y = element_text(size=8))

# Combinar gráfico principal y zoom (zoom abajo a la derecha)
final_plot <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_zoom, x = 0.62, y = 0.078, width = 0.35, height = 0.35)

#png("Prediccion_FINAL_IPC.png", width = 1200, height = 800, res = 150)
final_plot
#dev.off()

######################################################################################################################################
#-------------------------                PIB (Modelo  Arima)                   --------------------------------------------

#Serie orginal 
series_PIB_trimestrales <- readRDS("DATOS/Series_Temporales/Trimestrales/gdp_ts_trimestral.rds")

#Aplicaremos segunda diferencia y lag=4 (Como hicimos anteriormente)
PIB_estacionaria_log<- log(series_PIB_trimestrales)
PIB_log_diff_seasonal <- diff(PIB_estacionaria_log, lag = 4)
PIB_estacionaria <- diff(PIB_log_diff_seasonal, differences = 1)
tsdisplay(PIB_estacionaria)


#Aplicamos el modelos osbre la series diferenciada
modelo_final_arimax_arima_PIB <- arima(PIB_estacionaria, order=c(2,0,2))  # d=0 porque ya diferenciamos anteriormente. y no necesitamos diferenciarlo otra vez
summary(modelo_final_arimax_arima_PIB)


# Validación de residuales
hist(residuals(modelo_final_arimax_arima_PIB), main="Histograma de residuales Arimax-arima PIB", xlab="Residual", col=paleta[2])
#png("checkresiduals_PIB.png", width = 1800, height = 1400, res = 150)
checkresiduals(modelo_final_arimax_arima_PIB)
#dev.off()

boxtest_arima_pib<- Box.test(residuals(modelo_final_arimax_arima_PIB), lag = round(log(length(series_PIB_trimestrales))), type="Ljung-Box")
if (boxtest_arima_pib$p.value > 0.05) {
  cat("Residuos ARIMAx-Arima PIB parecen ruido blanco\n")
} else {
  cat("Residuos ARIMA IPC muestran autocorrelación\n")
}
#Residuos ARIMA IPC parecen ruido blanco

#-----------------      PREDICCIONES
prediccion_final_arimax_arima_PIB <- forecast(modelo_final_arimax_arima_PIB, h=2, level=90)


# -------------------- REVERTIR DIFERENCIAS -----------------
PIB_estacionaria_log
# Predicción diferenciada (segunda diferencia con lag=4)
pred <- prediccion_final_arimax_arima_PIB$mean
revert_diff1 <- diffinv(pred, differences = 1, xi = tail(PIB_log_diff_seasonal, 1))
revert_diff_seasonal <- diffinv(revert_diff1, differences = 1, lag = 4, xi = tail(PIB_estacionaria_log, 4))
prediccion_final_Arimax_arima_PIB <- exp(revert_diff_seasonal)[c(6,7)]

# ---------------- JUNTAR DF (ORIGINAL + PREDICCION)
# Obtener el tiempo final de la serie
tiempo_final1 <- end(series_PIB_trimestrales)  # año y trimestre
# Crear serie de predicciones como ts, frecuencia 4 (trimestral)
predicciones_ts1 <- ts(prediccion_final_Arimax_arima_PIB, start = c(tiempo_final1[1], tiempo_final1[2]+1), frequency = 4)
# Unir series históricas y predicciones
df_PIB_Completo <- ts(c(series_PIB_trimestrales, predicciones_ts1), start = start(series_PIB_trimestrales), frequency = 4)
df_PIB_Completo


#-----------------      GRAFICO FINAL
h <- 2  # horizonte de predicción
df_total_PIB <- data.frame(
  Trimestre = as.numeric(time(series_PIB_trimestrales)),
  PIB = as.numeric(series_PIB_trimestrales)
)

# Revertir las diferencias para los intervalos de confianza
ultimo_valor_PIB_log_diff <- tail(PIB_log_diff_seasonal, 1)
ultimo_valor_PIB_log <- tail(PIB_estacionaria_log, 4)

# Banda inferior y superior revertidas
lower_rev1 <- diffinv(prediccion_final_arimax_arima_PIB$lower[,1], differences = 1, xi = ultimo_valor_PIB_log_diff)
lower_rev2 <- diffinv(lower_rev1, differences = 1, lag = 4, xi = ultimo_valor_PIB_log)
lower_rev_final <- exp(lower_rev2)[c(6,7)]

upper_rev1 <- diffinv(prediccion_final_arimax_arima_PIB$upper[,1], differences = 1, xi = ultimo_valor_PIB_log_diff)
upper_rev2 <- diffinv(upper_rev1, differences = 1, lag = 4, xi = ultimo_valor_PIB_log)
upper_rev_final <- exp(upper_rev2)[c(6,7)]

# Crear serie temporal de predicciones revertidas
tiempo_final <- end(series_PIB_trimestrales)
predicciones_ts_final <- ts(prediccion_final_Arimax_arima_PIB,
                            start = c(tiempo_final[1], tiempo_final[2]+1),
                            frequency = 4)

df_pred_PIB <- data.frame(
  Trimestre = time(predicciones_ts_final),
  Pred = as.numeric(predicciones_ts_final),
  Lower = as.numeric(lower_rev_final),
  Upper = as.numeric(upper_rev_final))

# Gráfico principal
p_main_PIB <- ggplot(df_total_PIB, aes(x = Trimestre, y = PIB)) +
  geom_line(color = paleta[2], linewidth = 1) +
  geom_line(data = df_pred_PIB, aes(x = Trimestre, y = Pred), color = paleta[4], linetype = "dashed", inherit.aes = FALSE) +
  geom_ribbon(data = df_pred_PIB, aes(x = Trimestre, ymin = Lower, ymax = Upper), fill =paleta[2], alpha = 0.2, inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicción PIB con ARIMA (2,0,2)",
    y = "PIB",
    x = "Trimestre")

#Zoom últimos trimestres
x_min <- 2021
x_max <- 2023

# Etiquetas trimestrales
df_quarters <- expand.grid(year = 2021:2023, q = 1:4)
df_quarters$pos <- df_quarters$year + (df_quarters$q - 1) / 4
df_quarters <- df_quarters[order(df_quarters$pos), ]
breaks_all <- df_quarters$pos
labels_all <- paste0(df_quarters$year, " Q", df_quarters$q)

p_zoom_PIB <- ggplot() +
  geom_line(data = df_total_PIB, aes(x = Trimestre, y = PIB), color = paleta[2]) +
  geom_line(data = df_pred_PIB, aes(x = Trimestre, y = Pred), color = paleta[4], linetype = "dashed", inherit.aes = FALSE) +
  geom_ribbon(data = df_pred_PIB, aes(x = Trimestre, ymin = Lower, ymax = Upper), fill = paleta[2], alpha = 0.2, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  scale_x_continuous(
    breaks = breaks_all[breaks_all >= x_min & breaks_all <= x_max],
    labels = labels_all[breaks_all >= x_min & breaks_all <= x_max]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 8))

#Combinar gráfico principal y zoom (zoom abajo a la derecha)
final_plot_PIB <- ggdraw() +
  draw_plot(p_main_PIB) +
  draw_plot(p_zoom_PIB, x = 0.62, y = 0.078, width = 0.35, height = 0.35)

#png("Prediccion_FINAL_PIB.png", width = 1200, height = 800, res = 150)
final_plot_PIB
#dev.off()




##########################################################################################################################################################
#####################         ----------  COMPARACION EXPERTOS       ------------               ############################################

IPC_Q4_2021 <- as.numeric(window(df_IPC_Completo, start = c(2021, 4), end = c(2021, 4)))
IPC_Q4_2022 <- as.numeric(window(df_IPC_Completo, start = c(2022, 4), end = c(2022, 4)))

IPC_cambio_Q4 <- ((IPC_Q4_2022 - IPC_Q4_2021) / IPC_Q4_2021) * 100
IPC_cambio_Q4

PIB_Q4_2021 <- as.numeric(window(df_PIB_Completo, start = c(2021, 4), end = c(2021, 4)))
PIB_Q4_2022 <- as.numeric(window(df_PIB_Completo, start = c(2022, 4), end = c(2022, 4)))

PIB_cambio_Q4 <- ((PIB_Q4_2022 - PIB_Q4_2021) / PIB_Q4_2021) * 100
PIB_cambio_Q4


#Valores porcentuales Expertos
#IPC : 4,25% ---> 126.
#PIB: 5,5% --> 614,03

#no TIENE SENTIDO PORQUE EL Q2 DE 2022 ES 126 Y LOS EXPERTOS PARA Q4 2022 PREDIJIERON 127.

#VALORES PORCETUALES NUESTROS:
#IPC: 5.2% 
#PIB: 12 %

#Valor real :
#IPC: 
#PIB: 




###############################################################################################################################################################
######################################################################################################################################################################
##############################       GRAFICOS  EXPERTOS Y DATOS REALES          ######################################################################################


########## -------------------------------               PIB                ------------------------------------------ #################

#png("Nuestra_Prediccion_vs_Reales_PIB1.png", width = 1800, height = 1400, res= 150)
# Serie temporal del PIB
pib_ts <- df_PIB_Completo

# Convertir a dataframe
pib_clean <- data.frame(
  PIB = as.numeric(pib_ts),
  tiempo_index = time(pib_ts)
)

# Año y trimestre
pib_clean$Año <- floor(pib_clean$tiempo_index)
pib_clean$trimestre_num <- round((pib_clean$tiempo_index - pib_clean$Año) * 4) + 1
pib_clean$Trimestre <- paste0("Qtr", pib_clean$trimestre_num)

# Tiempo decimal para eje X
pib_clean$tiempo <- pib_clean$Año + (pib_clean$trimestre_num - 1)/4

# Filtrar desde 2012
pib_clean <- pib_clean %>% filter(Año >= 2012)

# Marcar últimos 2 como predicción
pib_clean$tipo <- "Observado"
n <- nrow(pib_clean)
pib_clean$tipo[(n-1):n] <- "Predicción"

# Valores reales últimos 2
valores_reales <- c(636.381, 640.751)
df_reales <- data.frame(
  tiempo = pib_clean$tiempo[(n-1):n],
  PIB = valores_reales,
  tipo = "Real",
  Año = pib_clean$Año[(n-1):n],
  Trimestre = pib_clean$Trimestre[(n-1):n]
)

# Etiquetas predicción
df_pred_labels <- pib_clean %>% 
  filter(tipo == "Predicción") %>%
  mutate(label = sprintf("%.2f", PIB))

# Etiquetas reales
df_real_labels <- df_reales %>%
  mutate(label = sprintf("%.2f", PIB))

# Desplazamiento etiquetas
df_pred_labels$desplazamiento <- 22.5  # arriba
df_real_labels$desplazamiento <- -14   # abajo

# Gráfico
ggplot() +
  geom_line(data = pib_clean, aes(x = tiempo, y = PIB, color = tipo), linewidth = 0.9) + # línea serie
  geom_point(data = filter(pib_clean, tipo == "Predicción"),
             aes(x = tiempo, y = PIB, color = tipo), size = 4.5, shape = 17) + # puntos predicción
  geom_point(data = df_reales,
             aes(x = tiempo, y = PIB, color = tipo), size = 4.5, shape = 16) + # puntos reales
  geom_line(data = df_reales, aes(x = tiempo, y = PIB, color = tipo), linewidth = 1.2) + # línea reales
  geom_text_repel(data = df_pred_labels,
                  aes(x = tiempo, y = PIB, label = label),
                  nudge_y = df_pred_labels$desplazamiento,
                  size = 4, color = paleta[3], fontface = "bold") + # etiquetas predicción
  geom_text_repel(data = df_real_labels,
                  aes(x = tiempo, y = PIB, label = label),
                  nudge_y = df_real_labels$desplazamiento,
                  size = 4, color = paleta[2], fontface = "bold") + # etiquetas reales
  scale_color_manual(
    values = c("Observado" = paleta[6], "Predicción" = paleta[3], "Real" = paleta[2]),
    name = "Tipo de dato" # leyenda
  ) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) + # eje X
  labs(
    title = "Serie Temporal del PIB: Predicciones vs Valores Reales", # título
    subtitle = "Series Trimestrales (Últimos dos trimestres predichos del 2022)", # subtítulo
    x = "Año", # eje X
    y = "PIB (millones de €)", # eje Y
    caption = "Nota: Triángulos naranjas = predicciones, Círculos verdes = valores reales" # nota
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 28), # título grande
    plot.subtitle = element_text(size = 20, color = paleta[6], face = "bold"), # subtítulo grande
    legend.position = "top", # leyenda arriba
    legend.text = element_text(size = 18), # tamaño leyenda
    legend.title = element_text(size = 20, face = "bold"), # título leyenda
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14), # tamaño eje
    plot.caption = element_text(hjust = 0.5, size = 16), # nota centrada
    plot.caption.position = "panel"
  )
#dev.off()




########## -------------------------------               IPC               ------------------------------------------ #################


#png("Nuestra_Prediccion_vs_Reales_IPC1.png", width = 1800, height = 1400, res= 150)
# Serie temporal del IPC
ipc_ts <- df_IPC_Completo

# Convertir a dataframe
ipc_clean <- data.frame(
  IPC = as.numeric(ipc_ts),
  tiempo_index = time(ipc_ts)
)

# Año y trimestre
ipc_clean$Año <- floor(ipc_clean$tiempo_index)
ipc_clean$trimestre_num <- round((ipc_clean$tiempo_index - ipc_clean$Año) * 4) + 1
ipc_clean$Trimestre <- paste0("Qtr", ipc_clean$trimestre_num)

# Tiempo decimal para eje X
ipc_clean$tiempo <- ipc_clean$Año + (ipc_clean$trimestre_num - 1)/4

# Filtrar desde 2012
ipc_clean <- ipc_clean %>% filter(Año >= 2012)

# Marcar últimos 2 como predicción
ipc_clean$tipo <- "Observado"
n <- nrow(ipc_clean)
ipc_clean$tipo[(n-1):n] <- "Predicción"

# Valores reales últimos 2
valores_reales <- c(128.4, 130.8)
df_reales <- data.frame(
  tiempo = ipc_clean$tiempo[(n-1):n],
  IPC = valores_reales,
  tipo = "Real",
  Año = ipc_clean$Año[(n-1):n],
  Trimestre = ipc_clean$Trimestre[(n-1):n]
)

# Etiquetas predicción
df_pred_labels <- ipc_clean %>% 
  filter(tipo == "Predicción") %>%
  mutate(label = sprintf("%.2f", IPC))

# Etiquetas reales
df_real_labels <- df_reales %>%
  mutate(label = sprintf("%.2f", IPC))

# Desplazamiento etiquetas
df_pred_labels$desplazamiento <- 5   # arriba
df_real_labels$desplazamiento <- -5  # abajo

# Gráfico
ggplot() +
  geom_line(data = ipc_clean, aes(x = tiempo, y = IPC, color = tipo), linewidth = 0.9) + # línea serie
  geom_point(data = filter(ipc_clean, tipo == "Predicción"),
             aes(x = tiempo, y = IPC, color = tipo), size = 4.5, shape = 17) + # puntos predicción
  geom_point(data = df_reales,
             aes(x = tiempo, y = IPC, color = tipo), size = 4.5, shape = 16) + # puntos reales
  geom_line(data = df_reales, aes(x = tiempo, y = IPC, color = tipo), linewidth = 1.2) + # línea reales
  geom_text_repel(data = df_pred_labels,
                  aes(x = tiempo, y = IPC, label = label),
                  nudge_y = -3,
                  size = 4, color = paleta[3], fontface = "bold") + # etiquetas predicción
  geom_text_repel(data = df_real_labels,
                  aes(x = tiempo, y = IPC, label = label),
                  nudge_y = 3,
                  size = 4, color = paleta[2], fontface = "bold") + # etiquetas reales
  scale_color_manual(
    values = c("Observado" = paleta[6], "Predicción" = paleta[3], "Real" = paleta[2]),
    name = "Tipo de dato" # leyenda
  ) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1), labels = seq(2012, 2023, by = 1)) + # eje X
  labs(
    title = "Serie Temporal del IPC: Predicciones vs Valores Reales", # título
    subtitle = "Series Trimestrales (Últimos dos trimestres predichos)", # subtítulo
    x = "Año", # eje X
    y = "IPC", # eje Y
    caption = "Nota: Triángulos naranjas = predicciones, Círculos verdes = valores reales" # nota
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 28), # título grande
    plot.subtitle = element_text(size = 20, color = paleta[6], face = "bold"), # subtítulo grande
    legend.position = "top", # leyenda arriba
    legend.text = element_text(size = 18), # tamaño leyenda
    legend.title = element_text(size = 20, face = "bold"), # título leyenda
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14), # tamaño eje
    plot.caption = element_text(hjust = 0.5, size = 16), # nota centrada
    plot.caption.position = "panel"
  )

#dev.off()

