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

paleta <- c("#c88fb2",  "#8db41c",  "#93044e","#D1006F",  "#F5F0E6",  "#4D4D4D")


#=========================
#Cargar datos:
#=========================
datos_limpios_AUS<- read.csv("DATOS/limpios/Datos_Limpios_Australia.csv")



#Cargar series temporales ESTACIONARIAS:
train_money_supply_estacionaria <- readRDS("Series_Temporales_ESTACIONARIAS/Train_Money_Supply_ts_ESTACIONARIA.rds")
train_unemployment_estacionaria <- readRDS("Series_Temporales_ESTACIONARIAS/Train_Unemployment_ts_ESTACIONARIA.rds")
train_gdp_estacionaria <- readRDS("Series_Temporales_ESTACIONARIAS/PIB_Train_ts_ESTACIONARIA.rds")
train_ipc_estacionaria <- readRDS("Series_Temporales_ESTACIONARIAS/IPC_Train_ts_ESTACIONARIA.rds")
train_stock_market_estacionaria <- readRDS("Series_Temporales_ESTACIONARIAS/Train_Stock_Market_ts_ESTACIONARIA.rds")
train_ipc<- readRDS("Series_Temporales/IPC_Train_ts.rds")
test_ipc<- readRDS("Series_Temporales/IPC_Test_ts.rds")
train_pib<- readRDS("Series_Temporales/PIB_Train_ts.rds")
test_pib<- readRDS("Series_Temporales/PIB_Test_ts.rds")
train_pib_log<- readRDS("Series_Temporales/Train_PIB_log")


#Cargar series originales (Sin diferencias):

train_stock_market <- readRDS("Series_Temporales/Train_stock_market.rds")
train_money_supply <- readRDS("Series_Temporales/Train_money_supply.rds")
train_unemployment<- readRDS("Series_Temporales/Train_unemployment.rds")

test_stock_market <- readRDS("Series_Temporales/Test_stock_market.rds")
test_money_supply <- readRDS("Series_Temporales/Test_money_supply.rds")
test_unemployment<- readRDS("Series_Temporales/Test_unemployment.rds")


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
                           seasonal = list(order = c(0, 0, 0), period = 4),
                           method = "ML")
summary(modelo_sarima_ipc)
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
ggplot(accuracy_long, aes(x = Modelo, y = Valor, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Métrica, scales = "free_y") +  # cada métrica en un panel
  theme_minimal() +
  ylab("Valor") +
  ggtitle("Comparación de Accuracy: Modelos de IPC") +
  theme(legend.position = "none") +
  geom_text(aes(label = round(Valor, 2)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(paleta[1], paleta[2], paleta[3]))




#........................................ PIB ..................................
#--------------------------------------------------------------------------------


#-----------------      AUTOARIMA ---- PIB        ----------------------
###########################################################################
#-------  MODELO
modelo_autoarima_pib <- auto.arima(train_gdp_estacionaria, seasonal = FALSE, d = 0)
summary(modelo_autoarima_pib)

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
                              lag=4,
                              xi = tail(diff(log(train_pib),lag=4), 4))

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
hist(residuals(modelo_arima_pib), main="Histograma de residuales ARIMA PIB", xlab="Residual", col=paleta[3])
checkresiduals(modelo_arima_pib)

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
  geom_text(aes(label = round(Valor, 2)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(paleta[1], paleta[2], paleta[3]))








##############################################################################################
################                      ARIMAX 
##############################################################################################

################ ---------    ipc   ---------  #########

# -------------------- PREPARAR TRAIN EXÓGENAS --------------------

train_exogenas_estacionarias<- cbind(train_money_supply_estacionaria, train_stock_market_estacionaria, train_unemployment_estacionaria)
train_exogenas_estacionarias<- na.omit(train_exogenas_estacionarias)

#Cambairmos el train del ipc para que el start de las exgoneas y el del ipc sean iguales

#El IPC empieza en 1998 segundo trimestres, y las exogenas en 1999 segundo trimestre. Hya que quitar un año
#Alinear fechas con train IPC
train_ipc_estacionaria_ARIMAX <- window(train_ipc_estacionaria, start = c(1999, 2))   # empieza 1999 trimestre 2

 
# -------------------- AJUSTAR MODELO ARIMAX ----------------------

# Modelo ARIMAX
modelo_arimax_ipc <- auto.arima(
  train_ipc_estacionaria_ARIMAX,
  seasonal = FALSE,
  d = 0,           # ya está estacionaria
  xreg = train_exogenas_estacionarias
)
summary(modelo_arimax_ipc)

# Calcular criterios
aic_arimax_IPC <- AIC(modelo_arimax_ipc)
bic_arimax_IPC <- BIC(modelo_arimax_ipc)
aicc_arimax_IPC <- modelo_arimax_ipc$aicc

cat("AIC:", aic_arimax_IPC, "BIC:", bic_arimax_IPC, "AICc:", aicc_arimax_IPC, "\n")


# -------------------- PREPARAR TEST EXÓGENAS ---------------------

# Preparación de variables exógenas del test para ARIMAX, Se añaden los últimos valores del train para poder aplicar las diferencias y mantener coherencia con las series del train

# --------- STOCK MARKET ---------
# Log-transformación + 1ª diferencia
stock_market_test_full <- ts(c(tail(train_stock_market, 1), test_stock_market),
                start = start(tail(train_stock_market, 1)),
                frequency = 4)
test_stock_market_estacionaria <- diff(stock_market_test_full)  # 1ª diferencia (ya log-transformada en el train)
test_stock_market_estacionaria <- window(test_stock_market_estacionaria, start = start(test_stock_market), end = end(test_stock_market)) #start(test_stock_market), end ("")
print(test_stock_market_estacionaria)

#--------- MONEY SUPPLY ---------
# Segunda diferencia
money_supply_test_full <- ts(c(tail(train_money_supply, 2), test_money_supply),
              start = start(tail(train_money_supply, 2)),
              frequency = 4)
test_money_supply_estacionaria <- diff(diff(money_supply_test_full))  # 2ª diferencia
test_money_supply_estacionaria <- window(test_money_supply_estacionaria, start = start(test_money_supply), end = end(test_money_supply))
print(test_money_supply_estacionaria)

# --------- UNEMPLOYMENT RATE ---------
# Segunda diferencia con lag=4
unemployment_test_full <- ts(c(tail(train_unemployment, 6), test_unemployment),
              start = start(tail(train_unemployment, 6)),
              frequency = 4)
test_unemployment_estacionaria <- diff(diff(unemployment_test_full, lag = 4))  # 2ª diferencia con lag 4
test_unemployment_estacionaria <- window(test_unemployment_estacionaria, start = start(test_unemployment), end = end(test_unemployment))
print(test_unemployment_estacionaria)

# -------------------- COMBINAR TODAS LAS EXÓGENAS -----------------
test_exogenas_estacionarias <- cbind(test_stock_market_estacionaria, test_money_supply_estacionaria, test_unemployment_estacionaria) #xreg
colnames(test_exogenas_estacionarias) <- colnames(train_exogenas_estacionarias)


# -------------------- PREDICCION ARIMAX  -----------------

prediccion_arimax_ipc <- forecast(
  modelo_arimax_ipc,
  xreg = test_exogenas_estacionarias,
  h = length(test_ipc)
)

summary(prediccion_arimax_ipc)

#-----------     REVERTIR
#Revertimos el forecast (IPC TRAIN)
# La serie original fue diferenciada 1 vez, usamos diffinv
forecast_arimax_ipc_revertida <- diffinv(prediccion_arimax_ipc$mean, differences = 1, xi=tail(train_ipc, 1))
forecast_arimax_ipc_revertida <- forecast_arimax_ipc_revertida[-1]

#-- ACCURACY
accuracy_arimax_ipc<- forecast::accuracy(forecast_arimax_ipc_revertida, test_ipc)
accuracy_arimax_ipc



############## ARIMAX   -   ARIMA MANUAL

modelo_arimax_arima_ipc <- arima(
  train_ipc_estacionaria_ARIMAX,
  order = c(1, 0, 0),
  xreg = train_exogenas_estacionarias
)
summary(modelo_arimax_ipc)


prediccion_arima_ipc <- forecast(
  modelo_arima_ipc,
  xreg = test_exogenas_estacionarias,
  h = length(test_ipc)
)

prediccion_arimax_arima_ipc <- forecast(
  modelo_arimax_arima_ipc,
  xreg = test_exogenas_estacionarias,
  h = length(test_ipc)
)















# ======================================================
# VALIDACIÓN CRUZADA (tsCV) PARA IPC Y PIB
# ======================================================

library(forecast)
library(ggplot2)

# Paleta de colores
paleta <- c("#1b9e77", "#d95f02", "#7570b3")

#-------------------------------------------------------
# Función auxiliar para calcular RMSE desde tsCV
#-------------------------------------------------------
calc_rmse_tscv <- function(ts_data, f_model, h = 2, nombre = "Modelo") {
  errores <- forecast::tsCV(ts_data, f_model, h = h)
  rmse <- sqrt(colMeans(errores^2, na.rm = TRUE))  # RMSE correcto
  data.frame(Modelo = nombre,
             Horizonte = 1:h,
             RMSE = rmse)
}

#-------------------------------------------------------
# 1️⃣ AUTOARIMA (sin estacionalidad)
#-------------------------------------------------------
f_autoarima <- function(x, h) {
  forecast(forecast::auto.arima(x, seasonal = FALSE, d = 0, D = 0), h = h)
}

#-------------------------------------------------------
# 2️⃣ ARIMA manual
#-------------------------------------------------------
f_arima_ipc <- function(x, h) {
  forecast(forecast::arima(x, order = c(1, 0, 0)), h = h)
}

f_arima_pib <- function(x, h) {
  forecast(forecast::arima(x, order = c(2, 0, 2)), h = h)
}

#-------------------------------------------------------
# 3️⃣ SARIMA (estacional automático)
#-------------------------------------------------------
f_sarima <- function(x, h) {
  forecast(forecast::auto.arima(x,
                                stepwise = FALSE,
                                approximation = FALSE,
                                seasonal = TRUE), h = h)
}

# ======================================================
# ⚙️ IPC
# ======================================================

rmse_auto_ipc  <- calc_rmse_tscv(train_ipc_estacionaria, f_autoarima, h = 2, nombre = "AutoARIMA")
#rmse_arima_ipc <- calc_rmse_tscv(train_ipc_estacionaria, f_arima_ipc,  h = 2, nombre = "ARIMA")
rmse_sarima_ipc <- calc_rmse_tscv(train_ipc_estacionaria, f_sarima,   h = 2, nombre = "SARIMA")

rmse_total_ipc <- rbind(rmse_auto_ipc, rmse_sarima_ipc)

ggplot(rmse_total_ipc, aes(x = Horizonte, y = RMSE, color = Modelo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(paleta[1], paleta[2], paleta[3])) +
  theme_minimal() +
  labs(title = "Validación cruzada (RMSE) - Modelos IPC",
       x = "Horizonte (h)",
       y = "RMSE")

# ======================================================
# ⚙️ PIB
# ======================================================

rmse_auto_pib  <- calc_rmse_tscv(train_gdp_estacionaria, f_autoarima,  h = 2, nombre = "AutoARIMA")
rmse_arima_pib <- calc_rmse_tscv(train_gdp_estacionaria, f_arima_pib,  h = 2, nombre = "ARIMA")
rmse_sarima_pib <- calc_rmse_tscv(train_gdp_estacionaria, f_sarima,    h = 2, nombre = "SARIMA")

rmse_total_pib <- rbind(rmse_auto_pib, rmse_arima_pib, rmse_sarima_pib)

ggplot(rmse_total_pib, aes(x = Horizonte, y = RMSE, color = Modelo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(paleta[1], paleta[2], paleta[3])) +
  theme_minimal() +
  labs(title = "Validación cruzada (RMSE) - Modelos PIB",
       x = "Horizonte (h)",
       y = "RMSE")













#MIRAR LOS AIC, AICC DE CADA MODELOS,


















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