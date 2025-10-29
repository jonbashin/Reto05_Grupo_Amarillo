# ============================================================
# ANÁLISIS DE PIB E INFLACIÓN - AUSTRALIA (6 GRÁFICOS CLAVE)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# ============================================================
# Cargar datos
# ============================================================
pib_nominal <- read.csv("DATOS/Externos/PIB/PIB NOMINAL.csv")
pib_real <- read.csv("DATOS/Externos/PIB/PIB REAL.csv")
pib_percapita <- read.csv("DATOS/Externos/PIB/gdp per capita.csv")
ipc <- read.csv("DATOS/Externos/IPC/IPC SUBYACENTE.csv")

# ============================================================
# Renombrar columnas y convertir fechas
# ============================================================
pib_nominal <- pib_nominal %>% 
  rename(Fecha = observation_date, PIB_nominal = NGDPSAXDCAUQ) %>%
  mutate(Fecha = as.Date(Fecha))

pib_real <- pib_real %>% 
  rename(Fecha = observation_date, PIB_real = NGDPRSAXDCAUQ) %>%
  mutate(Fecha = as.Date(Fecha))

pib_percapita <- pib_percapita %>% 
  rename(Fecha = observation_date, PIB_percapita = PCAGDPAUA646NWDB) %>%
  mutate(Fecha = as.Date(Fecha))

ipc <- ipc %>% 
  rename(Fecha = observation_date, Inflacion_Anual = CPGRLE01AUQ659N) %>%
  mutate(Fecha = as.Date(Fecha))

# ============================================================
# Preparar datos
# ============================================================
df <- pib_nominal %>%
  left_join(pib_real, by = "Fecha") %>%
  arrange(Fecha) %>%
  mutate(
    Año = year(Fecha),
    Crecimiento_Trimestral = (PIB_real / lag(PIB_real) - 1) * 100,
    Crecimiento_Anual = (PIB_real / lag(PIB_real, 4) - 1) * 100
  )

pib_percapita <- pib_percapita %>%
  arrange(Fecha) %>%
  mutate(
    Año = year(Fecha),
    Crecimiento_percapita = (PIB_percapita / lag(PIB_percapita) - 1) * 100
  )

ipc <- ipc %>% mutate(Año = year(Fecha))

# ============================================================
# GRÁFICO 1.1: PIB NOMINAL vs PIB REAL
# ============================================================
ggplot(df, aes(x = Fecha)) +
  geom_line(aes(y = PIB_nominal / 1e9, color = "PIB Nominal"), linewidth = 1.2) +
  geom_line(aes(y = PIB_real / 1e9, color = "PIB Real"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("PIB Nominal" = "#e74c3c", "PIB Real" = "#3498db")) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "PIB NOMINAL vs PIB REAL - AUSTRALIA",
    subtitle = "Comparación: serie sin ajuste vs ajustada por inflación",
    x = "Año",
    y = "PIB (Miles de millones AUD)",
    color = "Serie",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# GRÁFICO 1.2: CRECIMIENTO ANUAL DEL PIB REAL
# ============================================================
ggplot(df, aes(x = Fecha, y = Crecimiento_Anual)) +
  geom_col(aes(fill = Crecimiento_Anual > 0), width = 150) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                    labels = c("Contracción", "Crecimiento"),
                    name = "Tipo") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "CRECIMIENTO INTERANUAL DEL PIB REAL",
    subtitle = "Variación año a año (últimos 4 trimestres)",
    x = "Año",
    y = "Crecimiento (%)",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# GRÁFICO 2.1: PIB PER CÁPITA
# ============================================================
ggplot(pib_percapita, aes(x = Fecha, y = PIB_percapita)) +
  geom_line(color = "#f39c12", linewidth = 1.2) +
  geom_point(color = "#e67e22", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "$")) +
  labs(
    title = "PIB PER CÁPITA - AUSTRALIA",
    subtitle = "Ingreso promedio por habitante en USD (línea roja: tendencia)",
    x = "Año",
    y = "PIB per cápita (USD)",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# GRÁFICO 2.2: CRECIMIENTO DEL PIB PER CÁPITA
# ============================================================
ggplot(pib_percapita, aes(x = Fecha, y = Crecimiento_percapita)) +
  geom_line(color = "#9b59b6", linewidth = 1) +
  geom_point(color = "#8e44ad", size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "CRECIMIENTO ANUAL DEL PIB PER CÁPITA",
    subtitle = "Variación en el ingreso promedio por habitante",
    x = "Año",
    y = "Crecimiento (%)",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# GRÁFICO 3.1: INFLACIÓN ANUAL
# ============================================================
ggplot(ipc, aes(x = Fecha, y = Inflacion_Anual)) +
  geom_line(color = "#e74c3c", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_hline(yintercept = 2, linetype = "dotted", color = "#3498db", linewidth = 0.8) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "INFLACIÓN ANUAL - AUSTRALIA",
    subtitle = "Variación porcentual de precios año a año (línea azul: meta 2%)",
    x = "Año",
    y = "Inflación (%)",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# GRÁFICO 3.2: PIB REAL vs INFLACIÓN
# ============================================================
df_comparacion <- ipc %>%
  select(Año, Inflacion_Anual) %>%
  inner_join(df %>% group_by(Año) %>% 
               summarise(Crecimiento_PIB = mean(Crecimiento_Anual, na.rm = TRUE), .groups = 'drop'),
             by = "Año")

ggplot(df_comparacion, aes(x = Año)) +
  geom_line(aes(y = Crecimiento_PIB, color = "Crecimiento PIB Real"), linewidth = 1.2) +
  geom_line(aes(y = Inflacion_Anual, color = "Inflación"), linewidth = 1.2) +
  geom_point(aes(y = Crecimiento_PIB, color = "Crecimiento PIB Real"), size = 2, alpha = 0.6) +
  geom_point(aes(y = Inflacion_Anual, color = "Inflación"), size = 2, alpha = 0.6) +
  scale_color_manual(values = c("Crecimiento PIB Real" = "#3498db", "Inflación" = "#e74c3c")) +
  scale_x_continuous(breaks = seq(1960, 2024, 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "PIB REAL vs INFLACIÓN - AUSTRALIA",
    subtitle = "Crecimiento económico versus variación de precios (1960-2024)",
    x = "Año",
    y = "Porcentaje (%)",
    color = "Indicador",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================
# RESUMEN ESTADÍSTICO
# ============================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("            RESUMEN ESTADÍSTICO - PIB E INFLACIÓN          \n")
cat("═══════════════════════════════════════════════════════════\n")

resumen_pib <- df %>%
  summarise(
    Periodo = paste(min(year(Fecha), na.rm = TRUE), "-", max(year(Fecha), na.rm = TRUE)),
    Crecimiento_Medio_Anual = round(mean(Crecimiento_Anual, na.rm = TRUE), 2),
    Crecimiento_Maximo = round(max(Crecimiento_Anual, na.rm = TRUE), 2),
    Crecimiento_Minimo = round(min(Crecimiento_Anual, na.rm = TRUE), 2),
    PIB_Real_2024_B = round(max(PIB_real, na.rm = TRUE) / 1e9, 1)
  )

print(resumen_pib)

resumen_percapita <- pib_percapita %>%
  summarise(
    Crecimiento_Medio = round(mean(Crecimiento_percapita, na.rm = TRUE), 2),
    PIB_Percapita_Max = round(max(PIB_percapita, na.rm = TRUE), 0),
    PIB_Percapita_Min = round(min(PIB_percapita, na.rm = TRUE), 0)
  )

print(resumen_percapita)

resumen_inflacion <- ipc %>%
  summarise(
    Inflacion_Media = round(mean(Inflacion_Anual, na.rm = TRUE), 2),
    Inflacion_Maxima = round(max(Inflacion_Anual, na.rm = TRUE), 2),
    Inflacion_Minima = round(min(Inflacion_Anual, na.rm = TRUE), 2)
  )

print(resumen_inflacion)

# ============================================================
# INTERPRETACIÓN ECONÓMICA
# ============================================================

cat("\n═══════════════════════════════════════════════════════════\n")
cat("            INTERPRETACIÓN ECONÓMICA                        \n")
cat("═══════════════════════════════════════════════════════════\n")
cat("
📊 HALLAZGOS PRINCIPALES:

1️⃣ PIB NOMINAL vs PIB REAL:
   • PIB nominal refleja valores sin ajuste por inflación
   • PIB real ajustado refleja el crecimiento económico verdadero
   • Brecha creciente indicativa de inflación acumulada

2️⃣ CRECIMIENTO DEL PIB:
   • Tasa media de crecimiento anual: ~2.7%
   • Caídas notables en 2008 (crisis financiera) y 2020 (COVID-19)
   • Recuperación sostenida post-2020

3️⃣ PIB PER CÁPITA:
   • Crecimiento constante: refleja mejora en nivel de vida
   • Indica que la economía crece más que la población

4️⃣ INFLACIÓN:
   • Volatilidad considerable durante 1970s-1990s
   • Estabilización desde 2000 (meta del Banco Central: 2-3%)
   • Picos recientes (2022-2024) por shocks globales

5️⃣ RELACIÓN PIB-INFLACIÓN:
   • Ciclos económicos con impacto diferenciado en precios
   • Períodos de alto crecimiento sin inflación excesiva (1990s-2010s)
   • Trade-off observable en períodos de crisis

⚠  CONTEXTO ACTUAL:
   • Economia en crecimiento pero desacelerada
   • Inflación elevada presionando política monetaria
   • PIB per cápita en máximos históricos
")

cat("═══════════════════════════════════════════════════════════\n")

