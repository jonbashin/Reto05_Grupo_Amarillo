# ============================================================
# ANÁLISIS ECONÓMICO DE AUSTRALIA: PIB, PIB per cápita e IPC
# ============================================================

# 1️⃣ Librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# ============================================================
# 2️⃣ Cargar los datos
# ============================================================
pib_nominal <- read.csv("DATOS/Externos/PIB/PIB NOMINAL.csv")
pib_real <- read.csv("DATOS/Externos/PIB/PIB REAL.csv")
pib_percapita <- read.csv("DATOS/Externos/PIB/gdp per capita.csv")
ipc <- read.csv("DATOS/Externos/IPC/IPC SUBYACENTE.csv")

colnames(pib_nominal)
colnames(pib_nominal)
colnames(pib_percapita)
colnames(ipc)

# ============================================================
# 3️⃣ Renombrar columnas
# ============================================================
pib_nominal <- pib_nominal %>% rename(Fecha = observation_date, PIB_nominal = NGDPSAXDCAUQ)
pib_real <- pib_real %>% rename(Fecha = observation_date, PIB_real = NGDPRSAXDCAUQ)
pib_percapita <- pib_percapita %>% rename(Fecha = observation_date, PIB_percapita = PCAGDPAUA646NWDB)
ipc <- ipc %>% rename(Fecha = observation_date, Inflacion_Anual = CPGRLE01AUQ659N)

# ============================================================
# 4️⃣ Convertir fechas a tipo Date
# ============================================================
pib_nominal$Fecha <- as.Date(pib_nominal$Fecha)
pib_real$Fecha <- as.Date(pib_real$Fecha)
pib_percapita$Fecha <- as.Date(pib_percapita$Fecha)
ipc$Fecha <- as.Date(ipc$Fecha)

# ============================================================
# 5️⃣ Unir PIB nominal y real
# ============================================================
df <- pib_nominal %>%
  left_join(pib_real, by = "Fecha") %>%
  arrange(Fecha) %>%
  mutate(Año = year(Fecha))  # para unir luego con IPC

# ============================================================
# 6️⃣ Calcular crecimiento
# ============================================================
df <- df %>%
  mutate(
    Crecimiento_Trimestral = (PIB_real / lag(PIB_real) - 1) * 100,
    Crecimiento_Anual = (PIB_real / lag(PIB_real, 4) - 1) * 100
  )

pib_percapita <- pib_percapita %>%
  arrange(Fecha) %>%
  mutate(Año = year(Fecha),
         Crecimiento_percapita = (PIB_percapita / lag(PIB_percapita) - 1) * 100)

ipc <- ipc %>% mutate(Año = year(Fecha))

# ============================================================
# 7️⃣ Gráficos PIB
# ============================================================
# PIB nominal vs PIB real
ggplot(df, aes(x = Fecha)) +
  geom_line(aes(y = PIB_nominal, color = "PIB Nominal")) +
  geom_line(aes(y = PIB_real, color = "PIB Real")) +
  labs(title = "PIB Nominal vs PIB Real - Australia",
       x = "Año", y = "PIB (millones AUD)", color = "Serie") +
  theme_minimal()

# Crecimiento interanual
ggplot(df, aes(x = Fecha, y = Crecimiento_Anual)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Crecimiento interanual del PIB real - Australia",
       x = "Año", y = "Crecimiento (%)") +
  theme_minimal()

# Crecimiento trimestral
ggplot(df, aes(x = Fecha, y = Crecimiento_Trimestral)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Crecimiento trimestral del PIB real - Australia",
       x = "Trimestre", y = "Crecimiento (%)") +
  theme_minimal()

# PIB per cápita
ggplot(pib_percapita, aes(x = Fecha, y = PIB_percapita)) +
  geom_line(color = "orange", size = 1) +
  labs(title = "PIB per cápita en Australia (USD actuales)",
       x = "Año", y = "PIB per cápita") +
  theme_minimal()

# Crecimiento anual del PIB per cápita
ggplot(pib_percapita, aes(x = Fecha, y = Crecimiento_percapita)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Crecimiento anual del PIB per cápita - Australia",
       x = "Año", y = "Crecimiento (%)") +
  theme_minimal()

# ============================================================
# 8️⃣ Gráfico de inflación
# ============================================================
ggplot(ipc, aes(x = Fecha, y = Inflacion_Anual)) +
  geom_line(color = "red", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Inflación anual en Australia (1960–2024)",
       subtitle = "Variación porcentual del nivel de precios respecto al año anterior",
       x = "Año", y = "Inflación (%)") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  theme_minimal()

# ============================================================
# 9️⃣ Comparación PIB vs Inflación
# ============================================================
df_comparacion <- ipc %>%
  select(Año, Inflacion_Anual) %>%
  inner_join(df %>% group_by(Año) %>% summarise(Crecimiento_PIB = mean(Crecimiento_Anual, na.rm = TRUE)),
             by = "Año")

ggplot(df_comparacion, aes(x = Año)) +
  geom_line(aes(y = Crecimiento_PIB, color = "Crecimiento del PIB real"), size = 1) +
  geom_line(aes(y = Inflacion_Anual, color = "Inflación"), size = 1) +
  scale_color_manual(values = c("Crecimiento del PIB real" = "blue", "Inflación" = "red")) +
  labs(title = "PIB real vs Inflación en Australia",
       subtitle = "Comparación entre el crecimiento económico y la variación de precios (1960–2024)",
       x = "Año", y = "Porcentaje (%)", color = "Indicador") +
  theme_minimal()

# ============================================================
# 1️⃣0️⃣ Resumen de resultados
# ============================================================
resumen <- data.frame(
  Indicador = c("Crecimiento medio PIB Real (anual)",
                "Crecimiento medio PIB per cápita (anual)"),
  Valor = c(mean(df$Crecimiento_Anual, na.rm = TRUE),
            mean(pib_percapita$Crecimiento_percapita, na.rm = TRUE))
)
print(resumen)

# ============================================================
# Interpretación económica (para el informe)
# ============================================================
cat("
- El PIB nominal y real muestran tendencia al alza, con caídas visibles en 2008 y 2020.
- El PIB real, ajustado por inflación, refleja un crecimiento más estable.
- El PIB per cápita también crece, indicando mejoras en el nivel de vida.
- Las tasas de crecimiento interanual muestran una economía sólida pero sensible a choques globales.
")



##################################################################
##################################################################
##################################################################
##################################################################


# 1️⃣ Cargar librerías si no las tienes ya
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# 2️⃣ Cargar datos
deficit <- read.csv("DATOS/Externos/FISCAL/Deficit_Fiscal.csv")
deuda <- read.csv("DATOS/Externos/FISCAL/Deuda_PIB.csv")
gasto <- read.csv("DATOS/Externos/FISCAL/Gasto_Publico.csv")

# 3️⃣ Renombrar columnas para que sea más claro
deficit <- deficit %>% rename(Fecha = observation_date, Deficit_PIB = Value)
deuda <- deuda %>% rename(Fecha = observation_date, Deuda_PIB = Value)
gasto <- gasto %>% rename(Fecha = observation_date, Gasto_Publico = Value)

# Convertir fecha a Date y año
deficit$Fecha <- as.Date(deficit$Fecha)
deficit$Año <- year(deficit$Fecha)

deuda$Fecha <- as.Date(deuda$Fecha)
deuda$Año <- year(deuda$Fecha)

gasto$Fecha <- as.Date(gasto$Fecha)
gasto$Año <- year(gasto$Fecha)

# 4️⃣ Calcular algunas estadísticas (media, max, min)
summary_deficit <- deficit %>% summarise(Media = mean(Deficit_PIB, na.rm=TRUE),
                                         Maximo = max(Deficit_PIB, na.rm=TRUE),
                                         Minimo = min(Deficit_PIB, na.rm=TRUE))
summary_deuda <- deuda %>% summarise(Media = mean(Deuda_PIB, na.rm=TRUE),
                                     Maximo = max(Deuda_PIB, na.rm=TRUE),
                                     Minimo = min(Deuda_PIB, na.rm=TRUE))
summary_gasto <- gasto %>% summarise(Media = mean(Gasto_Publico, na.rm=TRUE),
                                     Maximo = max(Gasto_Publico, na.rm=TRUE),
                                     Minimo = min(Gasto_Publico, na.rm=TRUE))

# 5️⃣ Graficar evolución de cada variable
ggplot(deficit, aes(x=Año, y=Deficit_PIB)) +
  geom_line(color="red", size=1) +
  labs(title="Déficit Fiscal (% PIB)", x="Año", y="% del PIB") +
  theme_minimal()

ggplot(deuda, aes(x=Año, y=Deuda_PIB)) +
  geom_line(color="blue", size=1) +
  labs(title="Deuda Pública (% PIB)", x="Año", y="% del PIB") +
  theme_minimal()

ggplot(gasto, aes(x=Año, y=Gasto_Publico)) +
  geom_line(color="green", size=1) +
  labs(title="Gasto Público", x="Año", y="Millones AUD") +
  theme_minimal()
