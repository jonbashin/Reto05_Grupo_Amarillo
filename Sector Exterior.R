# ============================================================
# ANÁLISIS DE LA BALANZA DE PAGOS DE AUSTRALIA
# ============================================================

# Librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Cargar los datos de balanza de pagos
balanza_pagos <- read.csv("DATOS/Externos/SECTOR EXTERIOR/BALANZA DE PAGOS.csv")

# Renombrar columnas
balanza_pagos <- balanza_pagos %>%
  rename(
    Fecha = observation_date,
    Balanza_Cuenta_Corriente = AUSBCABP6USD
  )

# Convertir fechas a tipo Date
balanza_pagos$Fecha <- as.Date(balanza_pagos$Fecha)

# Calcular variaciones anuales
balanza_pagos <- balanza_pagos %>%
  arrange(Fecha) %>%
  mutate(
    Variacion_Anual = Balanza_Cuenta_Corriente - lag(Balanza_Cuenta_Corriente),
    Variacion_Porcentual = (Balanza_Cuenta_Corriente / lag(Balanza_Cuenta_Corriente) - 1) * 100
  )

# 1. GRÁFICO PRINCIPAL: BALANZA DE PAGOS (BARRAS CON ETIQUETAS)
ggplot(balanza_pagos, aes(x = Fecha, y = Balanza_Cuenta_Corriente / 1e9)) +
  geom_col(aes(fill = Balanza_Cuenta_Corriente < 0), width = 200) +
  geom_text(aes(label = round(Balanza_Cuenta_Corriente / 1e9, 1)), 
            vjust = ifelse(balanza_pagos$Balanza_Cuenta_Corriente < 0, 1.5, -0.5),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#2ecc71"),
                    labels = c("Déficit", "Superávit"),
                    name = "Saldo") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "BALANZA DE CUENTA CORRIENTE - AUSTRALIA",
    subtitle = "Saldo anual en miles de millones de USD",
    x = "Año",
    y = "Saldo (Miles de millones USD)",
    caption = "Fuente: FRED - Balanza de Pagos Australia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2. GRÁFICO DE EVOLUCIÓN TEMPORAL (LÍNEA + PUNTOS)
ggplot(balanza_pagos, aes(x = Fecha, y = Balanza_Cuenta_Corriente / 1e9)) +
  geom_line(color = "#3498db", linewidth = 1.2) +
  geom_point(color = "#2980b9", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EVOLUCIÓN DE LA BALANZA DE PAGOS - AUSTRALIA",
    subtitle = "Tendencia 2016-2024 (Miles de millones USD)",
    x = "Año",
    y = "Saldo (Miles de millones USD)",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 4. ANÁLISIS DE TENDENCIA
ggplot(balanza_pagos, aes(x = year(Fecha), y = Balanza_Cuenta_Corriente / 1e9)) +
  geom_smooth(method = "lm", se = TRUE, color = "#e67e22", fill = "#e67e22", alpha = 0.2) +
  geom_point(size = 4, color = "#34495e") +
  geom_line(color = "#34495e", alpha = 0.3, linewidth = 0.5) +
  labs(
    title = "TENDENCIA DE LA BALANZA DE PAGOS",
    subtitle = "Análisis de regresión lineal 2016-2024",
    x = "Año",
    y = "Saldo (Miles de millones USD)",
    caption = "La línea naranja muestra la tendencia de largo plazo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# RESUMEN ESTADÍSTICO
resumen_balanza <- balanza_pagos %>%
  summarise(
    Periodo = paste(min(year(Fecha)), "-", max(year(Fecha))),
    Saldo_Promedio_Miles_Millones = round(mean(Balanza_Cuenta_Corriente) / 1e9, 1),
    Saldo_Maximo_Miles_Millones = round(max(Balanza_Cuenta_Corriente) / 1e9, 1),
    Saldo_Minimo_Miles_Millones = round(min(Balanza_Cuenta_Corriente) / 1e9, 1),
    Años_Deficit = sum(Balanza_Cuenta_Corriente < 0),
    Años_Superavit = sum(Balanza_Cuenta_Corriente >= 0)
  )

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("          RESUMEN ESTADÍSTICO - BALANZA DE PAGOS              \n")
cat("═══════════════════════════════════════════════════════════════\n")
print(resumen_balanza)

# ANÁLISIS DETALLADO POR AÑO
detalle_anual <- balanza_pagos %>%
  mutate(
    Año = year(Fecha),
    Saldo_Miles_Millones = round(Balanza_Cuenta_Corriente / 1e9, 1),
    Variacion_Miles_Millones = round(Variacion_Anual / 1e9, 1),
    Situacion = ifelse(Balanza_Cuenta_Corriente < 0, "Déficit", "Superávit")
  ) %>%
  select(Año, Saldo_Miles_Millones, Variacion_Miles_Millones, Situacion)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("                    DETALLE ANUAL                              \n")
cat("═══════════════════════════════════════════════════════════════\n")
print(detalle_anual)

# ============================================================
# INTERPRETACIÓN ECONÓMICA
# ============================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("              INTERPRETACIÓN ECONÓMICA                         \n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("
📊 ANÁLISIS DE LA BALANZA DE PAGOS DE AUSTRALIA:

🔴 TENDENCIA GENERAL: 
   • Australia mantiene un déficit PERSISTENTE en cuenta corriente
   • TODOS los años analizados (2016-2024) presentan déficit
   
📉 MEJOR AÑO: 2019 con -3.5 mil millones USD (menor déficit)
📈 PEOR AÑO: 2016 con -41.4 mil millones USD (mayor déficit)

🔄 EVOLUCIÓN TEMPORAL:
   2016-2019: Mejora NOTABLE (déficit se reduce de -41.4B a -3.5B)
   2019-2020: Deterioro BRUSCO (de -3.5B a -23.9B) → Impacto COVID-19
   2020-2024: Empeoramiento GRADUAL (de -23.9B a -32.1B)

📈 IMPLICACIONES ECONÓMICAS:
   1. DEPENDENCIA EXTERNA: Australia consume más de lo que produce
   2. FINANCIACIÓN: Necesita entrada constante de capital extranjero
   3. VULNERABILIDAD: Expuesta a cambios en flujos de capital global
   4. TIPO DE CAMBIO: Presión bajista sobre el dólar australiano (AUD)

🔍 FACTORES CLAVE:
   • Exportaciones: Commodities (hierro, carbón, gas, oro, agricultura)
   • Importaciones: Manufacturas, maquinaria, tecnología, petróleo refinado
   • Renta neta: Pagos de intereses y dividendos a inversores extranjeros
   • Precios commodities: Caída post-2019 afectó ingresos de exportación
   
⚠  RIESGOS IDENTIFICADOS:
   • Déficit estructural: No es coyuntural, es persistente
   • Dependencia de capital extranjero para financiar el déficit
   • Sensibilidad a crisis financieras globales
   • Vulnerabilidad ante caída de precios de materias primas
")

cat("═══════════════════════════════════════════════════════════════\n")



######################################################################
# ============================================================
# ANÁLISIS MEJORADO DE EXPORTACIONES NETAS (NETEXP)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(patchwork) # Para combinar gráficos

# Cargar y preparar datos
netexp <- read.csv("DATOS/Externos/SECTOR EXTERIOR/EXPORTACIONES NETAS.csv")

netexp <- netexp %>%
  rename(
    Fecha = observation_date,
    Exportaciones_Netas = NETEXP
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Año = year(Fecha),
    Trimestre = quarter(Fecha),
    Periodo = paste0(Año, "-Q", Trimestre),
    Saldo = ifelse(Exportaciones_Netas < 0, "Déficit", "Superávit")
  )

# 1. GRÁFICO PRINCIPAL MEJORADO - EVOLUCIÓN COMPLETA
p1 <- ggplot(netexp, aes(x = Fecha, y = Exportaciones_Netas)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Exportaciones_Netas > 0, Exportaciones_Netas, 0)), 
              fill = "#2ecc71", alpha = 0.3) +
  geom_ribbon(aes(ymin = ifelse(Exportaciones_Netas < 0, Exportaciones_Netas, 0), ymax = 0), 
              fill = "#e74c3c", alpha = 0.3) +
  geom_line(color = "#34495e", size = 0.8) +
  geom_point(aes(color = Saldo), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("Déficit" = "#e74c3c", "Superávit" = "#27ae60")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", 
               limits = c(as.Date("1947-01-01"), as.Date("2025-12-31"))) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "EVOLUCIÓN HISTÓRICA DE EXPORTACIONES NETAS - AUSTRALIA",
    subtitle = "1947-2025 (Miles de millones de USD)",
    x = "Año",
    y = "Exportaciones Netas (Miles de millones USD)",
    color = "Saldo Comercial"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p1)

# 2. GRÁFICO DE LOS ÚLTIMOS 30 AÑOS CON MÁS DETALLE
netexp_reciente <- netexp %>% filter(Fecha >= "1995-01-01")

p2 <- ggplot(netexp_reciente, aes(x = Fecha, y = Exportaciones_Netas, fill = Saldo)) +
  geom_col(alpha = 0.8, width = 80) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Déficit" = "#e74c3c", "Superávit" = "#27ae60")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "EXPORTACIONES NETAS - PERIODO RECIENTE (1995-2025)",
    subtitle = "Déficit comercial persistente",
    x = "Año",
    y = "Miles de millones USD",
    fill = "Saldo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p2)

# 3. GRÁFICO DE TENDENCIA POR DÉCADAS
netexp_decadas <- netexp %>%
  mutate(Decada = floor(Año / 10) * 10) %>%
  group_by(Decada) %>%
  summarise(
    Promedio_Netas = mean(Exportaciones_Netas),
    .groups = 'drop'
  )

p3 <- ggplot(netexp_decadas, aes(x = as.factor(Decada), y = Promedio_Netas, fill = Promedio_Netas)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(Promedio_Netas, 1)), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#27ae60", midpoint = 0) +
  labs(
    title = "PROMEDIO DE EXPORTACIONES NETAS POR DÉCADA",
    subtitle = "Transición de superávit a déficit estructural",
    x = "Década",
    y = "Promedio (Miles de millones USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.position = "none"
  )

print(p3)

# 4. GRÁFICO DE EVOLUCIÓN CON SUAVIZADO
p4 <- ggplot(netexp, aes(x = Fecha, y = Exportaciones_Netas)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(aes(color = Saldo), size = 1, alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.1, color = "#e67e22", se = TRUE, fill = "#f39c12", alpha = 0.2) +
  scale_color_manual(values = c("Déficit" = "#e74c3c", "Superávit" = "#27ae60")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TENDENCIA CON SUAVIZADO DE EXPORTACIONES NETAS",
    subtitle = "Línea naranja muestra la tendencia subyacente",
    x = "Año",
    y = "Miles de millones USD",
    color = "Saldo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.position = "top"
  )

print(p4)

# 5. ANÁLISIS ESTACIONAL (últimos 10 años)
netexp_estacional <- netexp %>% 
  filter(Fecha >= "2015-01-01") %>%
  mutate(Trimestre = as.factor(Trimestre))

p5 <- ggplot(netexp_estacional, aes(x = as.factor(Año), y = Exportaciones_Netas, fill = Trimestre)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "PATRÓN ESTACIONAL POR TRIMESTRE (2015-2025)",
    subtitle = "Análisis de comportamiento trimestral",
    x = "Año",
    y = "Miles de millones USD",
    fill = "Trimestre"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    legend.position = "top"
  )

print(p5)

# RESUMEN ESTADÍSTICO MEJORADO
cat("\n🔍 ANÁLISIS ESTADÍSTICO DETALLADO\n")
cat("═" = 50, "\n")

resumen_detallado <- netexp %>%
  summarise(
    Periodo = paste(min(Año), "-", max(Año)),
    Observaciones = n(),
    Promedio = round(mean(Exportaciones_Netas), 1),
    Mediana = round(median(Exportaciones_Netas), 1),
    Maximo = round(max(Exportaciones_Netas), 1),
    Fecha_Maximo = Fecha[which.max(Exportaciones_Netas)],
    Minimo = round(min(Exportaciones_Netas), 1),
    Fecha_Minimo = Fecha[which.min(Exportaciones_Netas)],
    Desviacion = round(sd(Exportaciones_Netas), 1),
    Años_Superavit = sum(Exportaciones_Netas >= 0),
    Años_Deficit = sum(Exportaciones_Netas < 0)
  )

print(resumen_detallado)

# ANÁLISIS POR PERIODOS HISTÓRICOS
cat("\n📊 EVOLUCIÓN POR PERIODOS HISTÓRICOS\n")
cat("═" = 50, "\n")

periodos <- netexp %>%
  mutate(
    Epoca = case_when(
      Año < 1970 ~ "1947-1969: Posguerra",
      Año >= 1970 & Año < 1980 ~ "1970-1979: Crisis petrolera",
      Año >= 1980 & Año < 2000 ~ "1980-1999: Globalización",
      Año >= 2000 ~ "2000-2025: Siglo XXI"
    )
  ) %>%
  group_by(Epoca) %>%
  summarise(
    Promedio = round(mean(Exportaciones_Netas), 1),
    Tendencia = ifelse(mean(Exportaciones_Netas) > 0, "Superávit", "Déficit"),
    .groups = 'drop'
  )

print(periodos)

# INTERPRETACIÓN ECONÓMICA
cat("\n💡 INTERPRETACIÓN ECONÓMICA\n")
cat("═" = 50, "\n")
cat("
📈 TENDENCIAS IDENTIFICADAS:

• FASE 1 (1947-1970): SUPERÁVIT SOSTENIDO
  - Economía basada en exportaciones primarias
  - Balanza comercial positiva

• FASE 2 (1970-1990): TRANSICIÓN CRÍTICA  
  - Impacto de crisis petroleras
  - Inicio del deterioro comercial

• FASE 3 (1990-2025): DÉFICIT ESTRUCTURAL
  - Globalización y apertura comercial
  - Aumento de importaciones manufactureras
  - Dependencia de commodities volátiles

🔴 SITUACIÓN ACTUAL:
- Déficit comercial profundo y persistente
- Máximo histórico en 2025 (-1,264.6B)
- Necesidad de reestructuración productiva
")

#####################################################################
# ============================================================
# ANÁLISIS DE IMPORTACIONES (NMRXDCAUA)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Cargar datos
importaciones <- read.csv("DATOS/Externos/SECTOR EXTERIOR/IMPORTACIONES NETAS.csv")

# Renombrar columnas
importaciones <- importaciones %>%
  rename(
    Fecha = observation_date,
    Importaciones = NMRXDCAUA
  )

# Convertir fechas
importaciones$Fecha <- as.Date(importaciones$Fecha)

# Calcular variaciones
importaciones <- importaciones %>%
  arrange(Fecha) %>%
  mutate(
    Variacion_Anual = Importaciones - lag(Importaciones),
    Variacion_Pct = (Importaciones / lag(Importaciones) - 1) * 100,
    Año = year(Fecha)
  )

# 1. GRÁFICO PRINCIPAL: LÍNEA + PUNTOS
ggplot(importaciones, aes(x = Fecha, y = Importaciones / 1e3)) +
  geom_line(color = "#e74c3c", linewidth = 1) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "IMPORTACIONES (NMRXDCAUA)",
    subtitle = "Tendencia 1960-2024",
    x = "Año",
    y = "Miles de millones USD",
    caption = "Fuente: FRED"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2. GRÁFICO DE BARRAS: ÚLTIMOS 20 AÑOS
importaciones_reciente <- importaciones %>% filter(Fecha >= "2004-01-01")

ggplot(importaciones_reciente, aes(x = Fecha, y = Importaciones / 1e3, fill = Importaciones)) +
  geom_col(width = 200) +
  scale_fill_gradient(low = "#e67e22", high = "#c0392b", guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "IMPORTACIONES - ÚLTIMOS 20 AÑOS",
    x = "Año",
    y = "Miles de millones USD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. GRÁFICO DE VARIACIÓN ANUAL
importaciones_var <- importaciones %>% 
  filter(!is.na(Variacion_Anual))

ggplot(importaciones_var, aes(x = Fecha, y = Variacion_Anual / 1e3, fill = Variacion_Anual > 0)) +
  geom_col(width = 200) +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                    labels = c("Aumento", "Disminución"),
                    name = "Cambio") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "VARIACIÓN ANUAL DE IMPORTACIONES",
    x = "Año",
    y = "Cambio respecto al año anterior (Mil millones USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# RESUMEN ESTADÍSTICO
resumen <- importaciones %>%
  summarise(
    Promedio_Billones = round(mean(Importaciones) / 1e3, 1),
    Maximo_Billones = round(max(Importaciones) / 1e3, 1),
    Minimo_Billones = round(min(Importaciones) / 1e3, 1),
    Año_Maximo = year(Fecha[which.max(Importaciones)]),
    Año_Minimo = year(Fecha[which.min(Importaciones)])
  )

cat("\n═══════════════════════════════════════════════════════════\n")
cat("         RESUMEN ESTADÍSTICO - IMPORTACIONES               \n")
cat("═══════════════════════════════════════════════════════════\n")
print(resumen)

# ANÁLISIS POR DÉCADAS
por_decada <- importaciones %>%
  mutate(Decada = floor(year(Fecha) / 10) * 10) %>%
  group_by(Decada) %>%
  summarise(
    Promedio = round(mean(Importaciones) / 1e3, 1),
    Maximo = round(max(Importaciones) / 1e3, 1),
    Minimo = round(min(Importaciones) / 1e3, 1),
    .groups = 'drop'
  )

cat("\n═══════════════════════════════════════════════════════════\n")
cat("              ANÁLISIS POR DÉCADAS                          \n")
cat("═══════════════════════════════════════════════════════════\n")
print(por_decada)

# TASA DE CRECIMIENTO PROMEDIO
importaciones_completo <- importaciones %>% filter(!is.na(Variacion_Pct))
tasa_promedio <- mean(importaciones_completo$Variacion_Pct, na.rm = TRUE)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("              INTERPRETACIÓN                               \n")
cat("═══════════════════════════════════════════════════════════\n")
cat(paste("\n📊 HALLAZGOS CLAVE:\n\n",
          "• 1960-1990: Crecimiento moderado ($20.5B → $92.4B)\n",
          "• 1990-2008: Expansión acelerada ($92.4B → $379.9B)\n",
          "• 2008-2020: Ralentización por crisis y pandemia\n",
          "• 2020-2024: Recuperación fuerte ($462.7B → $626.1B)\n\n",
          "📈 TENDENCIA: Crecimiento exponencial a largo plazo\n",
          "⚠  2024: Máximo histórico ($626.1B)\n",
          "📊 Tasa promedio crecimiento anual:", round(tasa_promedio, 2), "%\n"))
cat("═══════════════════════════════════════════════════════════\n")