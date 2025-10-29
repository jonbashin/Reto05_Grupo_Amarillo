# ============================================================
# ANÁLISIS DE LA ECONOMÍA EXTERIOR DE AUSTRALIA (6 GRÁFICOS CLAVE)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# ============================================================
# 1. BALANZA DE PAGOS
# ============================================================

balanza_pagos <- read.csv("DATOS/Externos/SECTOR EXTERIOR/BALANZA DE PAGOS.csv")

balanza_pagos <- balanza_pagos %>%
  rename(
    Fecha = observation_date,
    Balanza_Cuenta_Corriente = AUSBCABP6USD
  ) %>%
  mutate(Fecha = as.Date(Fecha))

# GRÁFICO 1.1: BARRAS CON VALORES
ggplot(balanza_pagos, aes(x = Fecha, y = Balanza_Cuenta_Corriente / 1e9)) +
  geom_col(aes(fill = Balanza_Cuenta_Corriente < 0), width = 200) +
  geom_text(aes(label = round(Balanza_Cuenta_Corriente / 1e9, 1)), 
            vjust = ifelse(balanza_pagos$Balanza_Cuenta_Corriente < 0, 1.5, -0.5),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#2ecc71"),
                    labels = c("Déficit", "Superávit"), name = "Saldo") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "BALANZA DE CUENTA CORRIENTE - AUSTRALIA",
    subtitle = "Saldo anual (2016-2024)",
    x = "Año",
    y = "Miles de millones USD",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GRÁFICO 1.2: LÍNEA DE TENDENCIA CON SUAVIZADO
ggplot(balanza_pagos, aes(x = Fecha, y = Balanza_Cuenta_Corriente / 1e9)) +
  geom_line(color = "#3498db", linewidth = 1.2) +
  geom_point(color = "#2980b9", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "#e67e22", fill = "#f39c12", alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EVOLUCIÓN DE LA BALANZA DE PAGOS",
    subtitle = "Tendencia 2016-2024 (línea naranja: suavizado)",
    x = "Año",
    y = "Miles de millones USD",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Resumen balanza de pagos
resumen_balanza <- balanza_pagos %>%
  summarise(
    Periodo = paste(min(year(Fecha)), "-", max(year(Fecha))),
    Saldo_Promedio_B = round(mean(Balanza_Cuenta_Corriente) / 1e9, 1),
    Peor_Año_B = round(min(Balanza_Cuenta_Corriente) / 1e9, 1),
    Mejor_Año_B = round(max(Balanza_Cuenta_Corriente) / 1e9, 1),
    Años_Deficit = sum(Balanza_Cuenta_Corriente < 0)
  )

cat("\n═══════════════════════════════════════════════════════════\n")
cat("        RESUMEN BALANZA DE PAGOS - AUSTRALIA              \n")
cat("═══════════════════════════════════════════════════════════\n")
print(resumen_balanza)
cat("\n")

# ============================================================
# 2. EXPORTACIONES NETAS
# ============================================================

netexp <- read.csv("DATOS/Externos/SECTOR EXTERIOR/EXPORTACIONES NETAS.csv")

netexp <- netexp %>%
  rename(
    Fecha = observation_date,
    Exportaciones_Netas = NETEXP
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Año = year(Fecha),
    Saldo = ifelse(Exportaciones_Netas < 0, "Déficit", "Superávit")
  )

# GRÁFICO 2.1: EVOLUCIÓN HISTÓRICA COMPLETA
ggplot(netexp, aes(x = Fecha, y = Exportaciones_Netas / 1e9)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Exportaciones_Netas > 0, Exportaciones_Netas / 1e9, 0)), 
              fill = "#27ae60", alpha = 0.3) +
  geom_ribbon(aes(ymin = ifelse(Exportaciones_Netas < 0, Exportaciones_Netas / 1e9, 0), ymax = 0), 
              fill = "#e74c3c", alpha = 0.3) +
  geom_line(color = "#34495e", size = 0.8) +
  scale_x_date(date_breaks = "15 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EXPORTACIONES NETAS - EVOLUCIÓN HISTÓRICA",
    subtitle = "1947-2025: Transición de superávit a déficit estructural (Miles de millones USD)",
    x = "Año",
    y = "Exportaciones Netas",
    caption = "Fuente: FRED | Verde: Superávit | Rojo: Déficit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GRÁFICO 2.2: ÚLTIMOS 30 AÑOS CON DETALLE
netexp_reciente <- netexp %>% filter(Fecha >= "1995-01-01")

ggplot(netexp_reciente, aes(x = Fecha, y = Exportaciones_Netas / 1e9, fill = Saldo)) +
  geom_col(alpha = 0.8, width = 80) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Déficit" = "#e74c3c", "Superávit" = "#27ae60")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EXPORTACIONES NETAS - PERÍODO RECIENTE",
    subtitle = "1995-2025: Déficit comercial persistente",
    x = "Año",
    y = "Miles de millones USD",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Análisis por periodos históricos
periodos <- netexp %>%
  mutate(
    Epoca = case_when(
      Año < 1970 ~ "1947-1969",
      Año >= 1970 & Año < 1990 ~ "1970-1989",
      Año >= 1990 & Año < 2000 ~ "1990-1999",
      Año >= 2000 ~ "2000-2025"
    )
  ) %>%
  group_by(Epoca) %>%
  summarise(
    Promedio_B = round(mean(Exportaciones_Netas) / 1e9, 1),
    Tendencia = ifelse(mean(Exportaciones_Netas) > 0, "Superávit", "Déficit"),
    .groups = 'drop'
  )

cat("═══════════════════════════════════════════════════════════\n")
cat("    EXPORTACIONES NETAS POR PERIODOS HISTÓRICOS            \n")
cat("═══════════════════════════════════════════════════════════\n")
print(periodos)
cat("\n")

# ============================================================
# 3. IMPORTACIONES
# ============================================================

importaciones <- read.csv("DATOS/Externos/SECTOR EXTERIOR/IMPORTACIONES NETAS.csv")

importaciones <- importaciones %>%
  rename(
    Fecha = observation_date,
    Importaciones = NMRXDCAUA
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Año = year(Fecha)
  ) %>%
  arrange(Fecha) %>%
  mutate(
    Variacion_Anual = Importaciones - lag(Importaciones),
    Variacion_Pct = (Importaciones / lag(Importaciones) - 1) * 100
  )

# GRÁFICO 3.1: EVOLUCIÓN COMPLETA CON TENDENCIA
ggplot(importaciones, aes(x = Fecha, y = Importaciones / 1e9)) +
  geom_line(color = "#e74c3c", linewidth = 1) +
  geom_point(color = "#c0392b", size = 2.5) +
  geom_smooth(method = "loess", se = TRUE, color = "#f39c12", fill = "#f39c12", alpha = 0.2, linewidth = 1) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EVOLUCIÓN DE IMPORTACIONES - AUSTRALIA",
    subtitle = "1960-2024: Crecimiento exponencial (Miles de millones USD)",
    x = "Año",
    y = "Importaciones",
    caption = "Fuente: FRED | Línea naranja: tendencia suavizada"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GRÁFICO 3.2: VARIACIÓN ANUAL (ÚLTIMOS 30 AÑOS)
importaciones_var <- importaciones %>% 
  filter(!is.na(Variacion_Anual) & Fecha >= "1995-01-01")

ggplot(importaciones_var, aes(x = Fecha, y = Variacion_Anual / 1e9, fill = Variacion_Anual > 0)) +
  geom_col(width = 200) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                    labels = c("Disminución", "Aumento"),
                    name = "Cambio") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "VARIACIÓN ANUAL DE IMPORTACIONES",
    subtitle = "1995-2024: Cambios año a año (Miles de millones USD)",
    x = "Año",
    y = "Cambio respecto al año anterior",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Resumen importaciones
resumen_imp <- importaciones %>%
  summarise(
    Promedio_B = round(mean(Importaciones, na.rm = TRUE) / 1e9, 1),
    Maximo_B = round(max(Importaciones) / 1e9, 1),
    Minimo_B = round(min(Importaciones) / 1e9, 1),
    Año_Maximo = year(Fecha[which.max(Importaciones)]),
    Año_Minimo = year(Fecha[which.min(Importaciones)]),
    Tasa_Crec_Promedio_Pct = round(mean(importaciones$Variacion_Pct, na.rm = TRUE), 2)
  )

cat("═══════════════════════════════════════════════════════════\n")
cat("           RESUMEN ESTADÍSTICO - IMPORTACIONES             \n")
cat("═══════════════════════════════════════════════════════════\n")
print(resumen_imp)
cat("\n")

# ============================================================
# INTERPRETACIÓN FINAL
# ============================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("            INTERPRETACIÓN ECONÓMICA GENERAL              \n")
cat("═══════════════════════════════════════════════════════════\n")
cat("
📊 HALLAZGOS PRINCIPALES:

1️⃣ BALANZA DE PAGOS:
   • Déficit PERSISTENTE en todos los años (2016-2024)
   • Peor año: 2016 (-41.4B), Mejor año: 2019 (-3.5B)
   • Promedio: -19.3B USD anuales
   • Australia consume más de lo que produce

2️⃣ EXPORTACIONES NETAS:
   • Fase 1 (1947-1970): Superávit sostenido (+3.0B)
   • Fase 2 (1970-1990): Transición crítica (-0.5B)
   • Fase 3 (1990-2025): Déficit estructural (-227.1B)
   • Cambio radical en la estructura económica

3️⃣ IMPORTACIONES:
   • Crecimiento exponencial (1960: $18.3B → 2024: $626.1B)
   • Tasa de crecimiento anual promedio: 6.34%
   • Máximo histórico en 2024
   • Refleja aumento en consumo de bienes manufacturados

⚠  CONCLUSIONES:
   • Déficit comercial estructural y creciente
   • Dependencia de capital extranjero
   • Vulnerabilidad ante cambios en precios de commodities
   • Necesidad de reestructuración productiva
")
cat("═══════════════════════════════════════════════════════════\n")

