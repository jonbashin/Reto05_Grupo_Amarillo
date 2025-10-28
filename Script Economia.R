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
# ========================================
# ANÁLISIS COMPLETO: POLÍTICA FISCAL AUSTRALIA
# ========================================

# 1️⃣ CARGAR LIBRERÍAS
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# 2️⃣ CARGAR DATOS
deficit_fiscal <- read.csv("DATOS/Externos/POLÍTICA FISCAL/DEFICIT FISCAL.csv")
deuda <- read.csv("DATOS/Externos/POLÍTICA FISCAL/DEUDA EN % DE PIB.csv")
gasto_publico <- read.csv("DATOS/Externos/POLÍTICA FISCAL/GASTO PÚBLICO.csv")

# 3️⃣ RENOMBRAR COLUMNAS
deficit_fiscal <- deficit_fiscal %>% 
  rename(Fecha = observation_date, Deficit_Fiscal = GGNLBAAUA188N)

deuda <- deuda %>% 
  rename(Fecha = observation_date, Deuda_PIB = GGGDTAAUA188N)

gasto_publico <- gasto_publico %>% 
  rename(Fecha = observation_date, Gasto_Publico = NAEXKP03AUQ189S)

# 4️⃣ CONVERTIR FECHAS
deficit_fiscal$Fecha <- as.Date(deficit_fiscal$Fecha)
deuda$Fecha <- as.Date(deuda$Fecha)
gasto_publico$Fecha <- as.Date(gasto_publico$Fecha)

# 5️⃣ FUSIONAR DATASETS
politica_fiscal <- deficit_fiscal %>%
  full_join(deuda, by = "Fecha") %>%
  full_join(gasto_publico, by = "Fecha") %>%
  arrange(Fecha) %>%
  # Filtrar solo período con datos completos (desde 1989)
  filter(Fecha >= as.Date("1989-01-01"))

politica_fiscal$Año <- year(politica_fiscal$Fecha)

# 6️⃣ ESTADÍSTICAS DESCRIPTIVAS
cat("\n========== ESTADÍSTICAS DESCRIPTIVAS ==========\n")
cat("\nDéficit Fiscal (% PIB):\n")
print(summary(politica_fiscal$Deficit_Fiscal))
cat("\nDeuda Pública (% PIB):\n")
print(summary(politica_fiscal$Deuda_PIB))




# ========================================
# GRÁFICO 4: RELACIÓN ENTRE DÉFICIT Y DEUDA (SCATTER)
# ========================================
ggplot(politica_fiscal, aes(x = Deficit_Fiscal, y = Deuda_PIB)) +
  geom_point(size = 4, alpha = 0.6, color = "#8e44ad") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "#cccccc", alpha = 0.2, size = 1) +
  labs(
    title = "Relación: Déficit Fiscal vs Deuda Pública",
    x = "Déficit Fiscal (% PIB)",
    y = "Deuda Pública (% PIB)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

# ========================================
# GRÁFICO 5: PROMEDIO ANUAL DE DÉFICIT
# ========================================
resumen_anual <- politica_fiscal %>%
  group_by(Año) %>%
  summarise(
    Media_Deficit = mean(Deficit_Fiscal, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(resumen_anual, aes(x = Año, y = Media_Deficit)) +
  geom_col(fill = ifelse(resumen_anual$Media_Deficit < 0, "#E20074", "#6DBC00"), 
           alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(
    title = "Déficit Fiscal Promedio Anual de Australia",
    x = "Año",
    y = "% del PIB"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ========================================
# CORRELACIÓN Y RESUMEN
# ========================================
cat("\n========== CORRELACIÓN DÉFICIT-DEUDA ==========\n")
correlacion <- cor(politica_fiscal$Deficit_Fiscal, politica_fiscal$Deuda_PIB, 
                   use = "complete.obs")
print(paste("Correlación:", round(correlacion, 3)))

cat("\n========== AÑOS CLAVE ==========\n")
cat("\nMáyor déficit:\n")
print(politica_fiscal[which.min(politica_fiscal$Deficit_Fiscal), c("Año", "Deficit_Fiscal")])

cat("\nMayor deuda:\n")
print(politica_fiscal[which.max(politica_fiscal$Deuda_PIB), c("Año", "Deuda_PIB")])

cat("\n========== CONTEXTO HISTÓRICO ==========\n")
cat("
1988-2007: Años de estabilidad y superávits fiscales
  → Deuda pública baja (~10-20% PIB)

2008-2009: Crisis Financiera Global
  → Déficit alcanza -4% del PIB
  → Comienzo del aumento de deuda pública

2020: Pandemia COVID-19
  → Déficit máximo: -7% del PIB
  → Gasto público masivo en estímulo económico

2020-2024: Recuperación parcial
  → Deuda pública alcanza máximos históricos (~50% PIB)
  → Déficit se modera pero permanece negativo
")

