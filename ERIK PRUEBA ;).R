
library("readxl")
library("dplyr")
library("tidyr")
library("naniar")
library("VIM")
library("simputation")
library("missForest")
library("nortest")
library("EnvStats")
# 1. Cargar datos
pib_ipc <- read.csv("DATOS/pib_ipc_paises_punto2.csv")

# 2. Filtrar solo Australia
aus <- pib_ipc %>% filter(Country == "Australia")
summary(aus)
str(aus)
vis_miss(aus)
# 3. Revisar valores NA
colSums(is.na(aus))

# Supongamos que tus columnas son: "Quarter" (trimestre) y "GDP" (PIB)
# Primero revisamos que la columna de trimestre esté en el formato correcto
head(aus)
aus_limpio<-na.omit(aus)
# Crear una serie temporal trimestral

aus_gdp_ts <- ts(aus_limpio$GDP.billion.currency.units, start = c(1996, 1), frequency = 4)

aus_cpi_ts <- ts(aus_limpio$Consumer.Price.Index..CPI., start = c(1996, 1), frequency = 4)

# Visualizar la serie temporal
plot(aus_gdp_ts, main="PIB de Australia (Trimestral)", ylab="PIB", xlab="Año")

plot(aus_cpi_ts, main="PIC de Australia (Trimestral)", ylab="PIB", xlab="Año")


aus_limpio$GDP_ts <- as.numeric(aus_gdp_ts)
aus_limpio$CPI_ts <- as.numeric(aus_cpi_ts)

aus_limpio<-aus_limpio[,-c(6,7)]
# Revisar
head(aus_limpio)
str(aus_limpio)


imputTS 11
forecast tsoutliers(), tsclean()
quitar tendencia, con diferencias.
(cuanto a variado) si no mola otra vez diferencias
estacionalidad, en el grafico de autocoreelacion algun pico con lag, el dto esta 

############################# FUNCIÓN PARA DETECTAR OUTLIERS ###################
###############################################
### FUNCIÓN PARA DETECTAR OUTLIERS ADAPTADA ###
###############################################

# Detectar outliers en PIB
gdp_outliers <- tsoutliers(aus_gdp_ts)
print(gdp_outliers)

# Detectar outliers en CPI
cpi_outliers <- tsoutliers(aus_cpi_ts)
print(cpi_outliers)


# Limpieza automática: reemplaza outliers y suaviza
gdp_clean <- tsclean(aus_gdp_ts)
cpi_clean <- tsclean(aus_cpi_ts)

# Diferencia de orden 1 (quita tendencia lineal)
gdp_diff <- diff(gdp_clean, differences = 2)
cpi_diff <- diff(cpi_clean, differences = 2)

ggAcf(gdp_diff)
ggAcf(cpi_diff)
# Si quieres quitar también estacionalidad trimestral:
gdp_diff_season <- diff(gdp_clean, lag = 4)
cpi_diff_season <- diff(cpi_clean, lag = 1)

ggAcf(gdp_diff_season)
ggAcf(cpi_diff_season)
dev.off()  # Cierra cualquier dispositivo gráfico abierto

par(mfrow=c(2,1))
plot(aus_gdp_ts, main="PIB Original", ylab="PIB")
lines(gdp_clean, col="red", lwd=2)

plot(aus_cpi_ts, main="CPI Original", ylab="CPI")
lines(cpi_clean, col="red", lwd=2)



