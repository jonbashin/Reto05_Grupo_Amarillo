
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
library(astsa)

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

#Guardamos la base de datos limpia
#write.csv(df_final, file = "Datos_Limpios_Australia.csv", row.names = FALSE)



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
library(astsa)

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

#Guardamos la base de datos limpia
#write.csv(df_final, file = "Datos_Limpios_Australia.csv", row.names = FALSE)



