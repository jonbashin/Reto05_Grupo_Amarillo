# ============================================================
# ANÁLISIS DE PIB E INFLACIÓN - AUSTRALIA (6 GRÁFICOS CLAVE)
# ============================================================
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(gridExtra)
library(purrr)

# ============================================================
# PALETA DE COLORES PERSONALIZADA
# ============================================================
paleta <- c("#c88fb2", "#8db41c", "#93044e", "#D1006F", "#F5F0E6", "#4D4D4D")

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
  scale_color_manual(values = c("PIB Nominal" = paleta[4], "PIB Real" = paleta[2])) +
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
  geom_hline(yintercept = 0, linetype = "solid", color = paleta[6], linewidth = 0.5) +
  scale_fill_manual(values = c("TRUE" = paleta[2], "FALSE" = paleta[3]),
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
  geom_line(color = paleta[1], linewidth = 1.2) +
  geom_point(color = paleta[1], size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = paleta[4], fill = paleta[4], alpha = 0.2) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "$")) +
  labs(
    title = "PIB PER CÁPITA - AUSTRALIA",
    subtitle = "Ingreso promedio por habitante en USD (línea rosa: tendencia)",
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
  geom_line(color = paleta[4], linewidth = 1) +
  geom_point(color = paleta[3], size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[6], linewidth = 0.5) +
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
  geom_line(color = paleta[3], linewidth = 1.2) +
  geom_point(color = paleta[4], size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[6], linewidth = 0.5) +
  geom_hline(yintercept = 2, linetype = "dotted", color = paleta[2], linewidth = 0.8) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "INFLACIÓN ANUAL - AUSTRALIA",
    subtitle = "Variación porcentual de precios año a año (línea verde: meta 2%)",
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
  scale_color_manual(values = c("Crecimiento PIB Real" = paleta[2], "Inflación" = paleta[3])) +
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
HALLAZGOS PRINCIPALES:

PIB NOMINAL vs PIB REAL:
   • PIB nominal refleja valores sin ajuste por inflación
   • PIB real ajustado refleja el crecimiento económico verdadero
   • Brecha creciente indicativa de inflación acumulada

CRECIMIENTO DEL PIB:
   • Tasa media de crecimiento anual: ~2.7%
   • Caídas notables en 2008 (crisis financiera) y 2020 (COVID-19)
   • Recuperación sostenida post-2020

PIB PER CÁPITA:
   • Crecimiento constante: refleja mejora en nivel de vida
   • Indica que la economía crece más que la población

INFLACIÓN:
   • Volatilidad considerable durante 1970s-1990s
   • Estabilización desde 2000 (meta del Banco Central: 2-3%)
   • Picos recientes (2022-2024) por shocks globales

RELACIÓN PIB-INFLACIÓN:
   • Ciclos económicos con impacto diferenciado en precios
   • Períodos de alto crecimiento sin inflación excesiva (1990s-2010s)
   • Trade-off observable en períodos de crisis

CONTEXTO ACTUAL:
   • Economia en crecimiento pero desacelerada
   • Inflación elevada presionando política monetaria
   • PIB per cápita en máximos históricos
")

cat("═══════════════════════════════════════════════════════════\n")

# ============================================================
#POLÍTICA MONETARIA
# ============================================================
#Cargar datos
reservas<-read.csv("DATOS\\Externos\\POLÍTICA MONETARIA\\RESERVAS DE DIVISAS.csv")
tipo_cambio<-read.csv("DATOS\\Externos\\POLÍTICA MONETARIA\\TIPO DE CAMBIO AUSTRALIANO.csv")
interes_anual<-read.csv("DATOS\\Externos\\POLÍTICA MONETARIA\\TIPO DE INTERÉS ANUAL (FRED).csv")
interes_trimestral<-read.csv("DATOS\\Externos\\POLÍTICA MONETARIA\\TIPO DE INTERÉS TRIMESTRAL (FRED).csv")

#Añadimos las columnas de Año y Mes
reservas<-reservas %>%
  mutate(Año = year(observation_date),
         Mes = month(observation_date),
         Dia = day(observation_date))
tipo_cambio<-tipo_cambio %>%
  mutate(Año = year(observation_date),
         Mes = month(observation_date),
         Dia = day(observation_date))
interes_anual<-interes_anual %>%
  mutate(Año = year(observation_date),
         Mes = month(observation_date),
         Dia = day(observation_date))
interes_trimestral<-interes_trimestral %>%
  mutate(Año = year(observation_date),
         Mes = month(observation_date),
         Dia = day(observation_date))

#Renombrar solo la columna de valor y quedarnos con fecha + valor
tipo_cambio<-tipo_cambio %>% select(observation_date, DEXUSAL) %>% rename(tipo_cambio = DEXUSAL)
interes_anual<-interes_anual %>% select(observation_date, IRLTLT01AUM156N) %>% rename(interes_anual = IRLTLT01AUM156N)
interes_trimestral<-interes_trimestral %>% select(observation_date, IR3TBB01AUM156N) %>% rename(interes_trimestral = IR3TBB01AUM156N)
reservas<-reservas %>% select(observation_date, AUINTDDL) %>% rename(reservas = AUINTDDL)

#Unimos los 4 datasets por fecha
tipo_interes<-merge(interes_anual, interes_trimestral)
cambio_interes<-merge(tipo_interes, tipo_cambio)
cambio_interes$observation_date <- as.Date(cambio_interes$observation_date)
cambio_interes<-cambio_interes %>%
  mutate(Año = year(observation_date),
         Mes = month(observation_date))

#Paleta de colores personalizada
paleta<-c("#c88fb2",  "#8db41c",  "#93044e","#D1006F",  "#F5F0E6","#4D4D4D")


#GRÁFICOS CON LOS DATASETS SEPARADOS
#Tasa de interés anual
interes_anual$observation_date <- as.Date(interes_anual$observation_date)
##png("Tasa Interés Anual.png", width = 800, height = 600, res = 100)
ggplot(interes_anual, aes(x = observation_date, y = interes_anual)) +
  geom_line(color = paleta[2], size = 1) +  # Verde de la nueva paleta
  labs(title = "Tasa de interés anual en Australia",
       x = "Fecha",
       y = "Tasa de interés anual (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#Tasa de interés trimestral
interes_trimestral$observation_date<-as.Date(interes_trimestral$observation_date)
#png("Tasa Interés Trimestral.png", width = 800, height = 600, res = 100)
ggplot(interes_trimestral, aes(x = observation_date, y = interes_trimestral)) +
  geom_line(color = paleta[4], size = 1) +  # Rosa/magenta
  labs(title = "Tasa de interés trimestral en Australia",
       x = "Fecha",
       y = "Tasa de interés trimestral (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#GRÁFICOS CON INTERÉS ANUAL Y TRIMESTRAL, Y TIPO DE CAMBIO JUNTOS
#Comparación entre los tipos de interés
#png("Comparación Tasas Interés.png", width = 800, height = 600, res = 100)
ggplot(cambio_interes) +
  geom_line(aes(x = observation_date, y = interes_anual, color = "Anual"), size = 1) +
  geom_line(aes(x = observation_date, y = interes_trimestral, color = "Trimestral"), size = 1) +
  scale_color_manual(
    values = c("Anual" = paleta[2],
               "Trimestral" = paleta[4]),
    name = "Tipo de interés"
  ) +
  labs(title = "Comparación de tasas de interés en Australia",
       x = "Fecha",
       y = "Tasa de interés (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = paleta[5]),
    panel.grid.minor = element_line(color = "#F5F0E6"),
    plot.title = element_text(color = paleta[6], face = "bold"),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6]),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(color = paleta[6]),
    legend.text = element_text(color = paleta[6])
  )
#dev.off()

#Evolución del tipo de cambio
#png("Evolución del tipo de cambio.png", width = 800, height = 600, res = 100)
ggplot(cambio_interes, aes(x = observation_date, y = tipo_cambio)) +
  geom_line(color = paleta[3], size = 1) +  # Morado intenso
  labs(title = "Evolución del tipo de cambio AUD/USD",
       x = "Fecha",
       y = "Tipo de cambio") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(color = paleta[6], face = "bold"),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#Promedio de interés anual por año
df<-cambio_interes %>%
  group_by(Año) %>%
  summarise(interes_promedio = mean(interes_anual, na.rm = TRUE))
#png("Promedio Anual Interés.png", width = 800, height = 600, res = 100)
ggplot(df, aes(x = factor(Año), y = interes_promedio)) +
  geom_col(fill = paleta[2]) +  # Verde
  geom_text(aes(label = round(interes_promedio, 2)),
            vjust = -0.5, color = paleta[6]) +
  labs(title = "Promedio anual de la tasa de interés",
       x = "Año",
       y = "Tasa de interés promedio (%)") +
  expand_limits(y = max(df$interes_promedio) * 1.1) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(color = paleta[6], face = "bold"),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()


# ============================================================
#POLÍTICA FISCAL
# ============================================================
#Cargar datos
deficit_fiscal <- read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\DEFICIT FISCAL.csv")
deuda <- read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\DEUDA EN % DE PIB.csv")
gasto_publico <- read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\GASTO PÚBLICO.csv")

#Juntamos los datasets
politica_fiscal <- merge(deficit_fiscal, deuda)
politica_fiscal <- merge(politica_fiscal, gasto_publico)

#Renombrar columnas para claridad (ajusta según tus CSV)
colnames(politica_fiscal) <- c("Fecha", "Déficit_Fiscal", "Deuda_PIB", "Gasto_Público")

#Convertir la fecha a formato fecha
politica_fiscal$Fecha <- as.Date(politica_fiscal$Fecha)

#Crear columna del Año
politica_fiscal <- politica_fiscal %>%
  mutate(Año = year(Fecha))

#Paleta de colores personalizada
paleta <- c("#c88fb2", "#8db41c", "#93044e", "#D1006F", "#F5F0E6", "#4D4D4D")


#GRÁFICO 1: Evolución del Déficit Fiscal
resumen_anual <- politica_fiscal %>%
  group_by(Año) %>%
  summarise(
    Media_Deficit = mean(Déficit_Fiscal, na.rm = TRUE),
    .groups = "drop"
  )

#png("Deficit Fiscal Anual.png", width = 800, height = 600, res = 100)
ggplot(resumen_anual, aes(x = Año, y = Media_Deficit)) +
  geom_col(fill = ifelse(resumen_anual$Media_Deficit < 0, paleta[4], paleta[2]), 
           alpha = 0.8) +  # rojo para negativo, verde para positivo
  geom_hline(yintercept = 0, color = paleta[6], size = 1) +
  labs(
    title = "Déficit Fiscal Promedio Anual de Australia",
    x = "Año",
    y = "% del PIB"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = paleta[6]),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()


#GRÁFICO 2: Evolución de la Deuda Pública (% de PIB)
#png("Deuda Pública.png", width = 800, height = 600, res = 100)
ggplot(politica_fiscal, aes(x = Fecha, y = Deuda_PIB)) +
  geom_line(color = paleta[2], size = 1.2) +
  labs(
    title = "Deuda pública de Australia (% del PIB)",
    x = "Fecha",
    y = "Deuda / PIB (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#GRÁFICO 3: Evolución del Gasto Público
#png("Evolución Gasto Público.png", width = 800, height = 600, res = 100)
ggplot(politica_fiscal, aes(x = Fecha, y = Gasto_Público)) +
  geom_line(color = paleta[3], size = 1) +
  geom_point(color = paleta[3], size = 1.5) +
  labs(title = "Evolución del Gasto Público de Australia",
       subtitle = "Como porcentaje del PIB",
       x = "Fecha", y = "Gasto Público (% PIB)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = paleta[6]),
    plot.subtitle = element_text(color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#GRÁFICO 4: Relación entre Déficit Fiscal y Gasto Público
#png("Déficit vs Gasto Público.png", width = 800, height = 600, res = 100)
ggplot(politica_fiscal, aes(x = Gasto_Público, y = Déficit_Fiscal)) +
  geom_point(color = paleta[3], size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = paleta[4]) +
  labs(
    title = "Relación entre gasto público y déficit fiscal",
    x = "Gasto público",
    y = "Déficit fiscal"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
#dev.off()

#GRÁFICO 5: Evolución del Déficit y la Deuda Pública
#Crear datos en formato largo para el gráfico
fiscal_long <- politica_fiscal %>%
  select(Fecha, Déficit_Fiscal, Deuda_PIB) %>%
  pivot_longer(cols = c(Déficit_Fiscal, Deuda_PIB), 
               names_to = "variable", 
               values_to = "valor")

#png("Evolución Déficit y Deuda.png", width = 800, height = 600, res = 100)
ggplot(fiscal_long, aes(x = Fecha, y = valor, color = variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Déficit_Fiscal" = paleta[4], 
                                "Deuda_PIB" = paleta[2]),
                     labels = c("Déficit fiscal", "Deuda / PIB")) +
  labs(
    title = "Evolución del déficit y la deuda pública en Australia",
    x = "Fecha",
    y = "% del PIB",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6]),
    legend.text = element_text(color = paleta[6])
  )
#dev.off()

#GRÁFICO 6: Relación entre Déficit y Deuda
#png("Déficit vs Deuda.png", width = 800, height = 600, res = 100)
g2 <- ggplot(politica_fiscal, aes(x = Déficit_Fiscal, y = Deuda_PIB)) +
  geom_point(color = paleta[3], size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = paleta[6], linetype = "dashed") +
  labs(title = "Relación entre Déficit Fiscal y Deuda Pública",
       x = "Déficit Fiscal (% PIB)", y = "Deuda Pública (% PIB)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = paleta[6]),
    axis.text = element_text(color = paleta[6]),
    axis.title = element_text(color = paleta[6])
  )
print(g2)
#dev.off()


# ============================================================
# ANÁLISIS DE LA ECONOMÍA EXTERIOR DE AUSTRALIA (6 GRÁFICOS CLAVE)
# ============================================================
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
  scale_fill_manual(values = c("TRUE" = paleta[3], "FALSE" = paleta[2]),
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
  geom_line(color = paleta[4], linewidth = 1.2) +
  geom_point(color = paleta[3], size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[3], linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = paleta[1], fill = paleta[1], alpha = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EVOLUCIÓN DE LA BALANZA DE PAGOS",
    subtitle = "Tendencia 2016-2024 (línea rosa: suavizado)",
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
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[6], size = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Exportaciones_Netas > 0, Exportaciones_Netas / 1e9, 0)), 
              fill = paleta[2], alpha = 0.3) +
  geom_ribbon(aes(ymin = ifelse(Exportaciones_Netas < 0, Exportaciones_Netas / 1e9, 0), ymax = 0), 
              fill = paleta[3], alpha = 0.3) +
  geom_line(color = paleta[6], size = 0.8) +
  scale_x_date(date_breaks = "15 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EXPORTACIONES NETAS - EVOLUCIÓN HISTÓRICA",
    subtitle = "1947-2025: Transición de superávit a déficit estructural (Miles de millones USD)",
    x = "Año",
    y = "Exportaciones Netas",
    caption = "Fuente: FRED | Verde: Superávit | Burdeos: Déficit"
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
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[6]) +
  scale_fill_manual(values = c("Déficit" = paleta[3], "Superávit" = paleta[2])) +
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
  geom_line(color = paleta[4], linewidth = 1) +
  geom_point(color = paleta[3], size = 2.5) +
  geom_smooth(method = "loess", se = TRUE, color = paleta[1], fill = paleta[1], alpha = 0.2, linewidth = 1) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "EVOLUCIÓN DE IMPORTACIONES - AUSTRALIA",
    subtitle = "1960-2024: Crecimiento exponencial (Miles de millones USD)",
    x = "Año",
    y = "Importaciones",
    caption = "Fuente: FRED | Línea rosa: tendencia suavizada"
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
  geom_hline(yintercept = 0, linetype = "dashed", color = paleta[6]) +
  scale_fill_manual(values = c("TRUE" = paleta[2], "FALSE" = paleta[3]),
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
HALLAZGOS PRINCIPALES:

BALANZA DE PAGOS:
   • Déficit PERSISTENTE en todos los años (2016-2024)
   • Peor año: 2016 (-41.4B), Mejor año: 2019 (-3.5B)
   • Promedio: -19.3B USD anuales
   • Australia consume más de lo que produce

EXPORTACIONES NETAS:
   • Fase 1 (1947-1970): Superávit sostenido (+3.0B)
   • Fase 2 (1970-1990): Transición crítica (-0.5B)
   • Fase 3 (1990-2025): Déficit estructural (-227.1B)
   • Cambio radical en la estructura económica

IMPORTACIONES:
   • Crecimiento exponencial (1960: $18.3B → 2024: $626.1B)
   • Tasa de crecimiento anual promedio: 6.34%
   • Máximo histórico en 2024
   • Refleja aumento en consumo de bienes manufacturados

CONCLUSIONES:
   • Déficit comercial estructural y creciente
   • Dependencia de capital extranjero
   • Vulnerabilidad ante cambios en precios de commodities
   • Necesidad de reestructuración productiva
")
cat("═══════════════════════════════════════════════════════════\n")



