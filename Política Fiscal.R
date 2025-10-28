#Cargar librerías
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(purrr)
library(lubridate)

#Cargar datos
deficit_fiscal<-read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\DEFICIT FISCAL.csv")
deuda<-read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\DEUDA EN % DE PIB.csv")
gasto_publico<-read.csv("DATOS\\Externos\\POLÍTICA FISCAL\\GASTO PÚBLICO.csv")

#Juntamos los datasets
politica_fiscal<-merge(deficit_fiscal, deuda)
politica_fiscal<-merge(politica_fiscal, gasto_publico)

#Renombrar columnas para claridad (ajusta según tus CSV)
colnames(politica_fiscal) <- c("Fecha", "Déficit_Fiscal", "Deuda_PIB", "Gasto_Público")

#Convertir la fecha a formato fecha
politica_fiscal$Fecha <- as.Date(politica_fiscal$Fecha)
#Crear columna del Año
politica_fiscal <- politica_fiscal %>%
  mutate(Año = year(Fecha))

#Evolución del Déficit Fiscal
resumen_anual <- politica_fiscal %>%
  group_by(Año) %>%
  summarise(
    Media_Deficit = mean(Déficit_Fiscal, na.rm = TRUE),
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

#Evolución de la Deuda Pública (% de PIB) en Australia
ggplot(politica_fiscal, aes(x = Fecha, y = Deuda_PIB)) +
  geom_line(color = "#6DBC00", size = 1.2) +
  labs(
    title = "Deuda pública de Australia (% del PIB)",
    x = "Fecha",
    y = "Deuda / PIB (%)"
  ) +
  theme_minimal(base_size = 14)

#Evolución del Gasto Público
ggplot(politica_fiscal, aes(x = Fecha, y = Gasto_Público)) +
  geom_line(color = "#27AE60", size = 1) +
  geom_point(color = "#27AE60", size = 1.5) +
  labs(title = "Evolución del Gasto Público de Australia",
       subtitle = "Como porcentaje del PIB",
       x = "Fecha", y = "Gasto Público (% PIB)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#Relación entre Déficit Fiscal y Gasto Público
ggplot(politica_fiscal, aes(x = Gasto_Público, y = Déficit_Fiscal)) +
  geom_point(color = "#7A1E5A", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#E20074") +
  labs(
    title = "Relación entre gasto público y déficit fiscal",
    x = "Gasto público",
    y = "Déficit fiscal (% del PIB)"
  ) +
  theme_minimal(base_size = 14)

#Comparación entre Déficit Fiscal Y Deuda 
fiscal_long<-politica_fiscal %>%
  select(Fecha, Déficit_Fiscal, Deuda_PIB) %>%
  pivot_longer(cols = c(Déficit_Fiscal, Deuda_PIB),
               names_to = "variable", values_to = "valor")

ggplot(fiscal_long, aes(x = Fecha, y = valor, color = variable)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Déficit_Fiscal" = "#E20074", "Deuda_PIB" = "#6DBC00"),
                     labels = c("Déficit fiscal", "Deuda / PIB")) +
  labs(
    title = "Evolución del déficit y la deuda pública en Australia",
    x = "Fecha",
    y = "% del PIB",
    color = ""
  ) +
  theme_minimal(base_size = 14)

#Relación entre Déficit y Deuda
g2 <- ggplot(politica_fiscal, aes(x = Déficit_Fiscal, y = Deuda_PIB)) +
  geom_point(color = "#8e44ad", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Relación entre Déficit Fiscal y Deuda Pública",
       x = "Déficit Fiscal (% PIB)", y = "Deuda Pública (% PIB)") +
  theme_minimal(base_size = 13)

ggplotly(g2)

# 8. RESÚMEN ESTADÍSTICO
# ---------------------------------------------------------

cat("RESUMEN ESTADÍSTICO - POLÍTICA FISCAL AUSTRALIA\n")
cat("=============================================\n\n")

# Estadísticas descriptivas
estadisticas <- politica_fiscal %>%
  select(-Fecha) %>%
  map_df(~data.frame(
    Media = mean(., na.rm = TRUE),
    Mediana = median(., na.rm = TRUE),
    Mínimo = min(., na.rm = TRUE),
    Máximo = max(., na.rm = TRUE),
    Desviación = sd(., na.rm = TRUE)
  ), .id = "Variable")

print(estadisticas)

# GRÁFICO DE RELACIÓN MEJORADO
p_relacion_mejorado <- ggplot(politica_fiscal, aes(x = Déficit_Fiscal, y = Deuda_PIB)) +
  geom_point(aes(color = year(Fecha), size = abs(Déficit_Fiscal)), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#8E44AD", linetype = "dashed") +
  geom_text(aes(label = ifelse(year(Fecha) %in% c(1990, 2000, 2010, 2020, 2023), 
                               as.character(year(Fecha)), "")), 
            hjust = -0.2, vjust = 0.5, size = 3) +
  scale_color_gradient2(low = "#3498DB", mid = "#2ECC71", high = "#E74C3C", 
                        midpoint = 2005, name = "Año") +
  scale_size_continuous(name = "Magnitud del Déficit") +
  labs(title = "RELACIÓN ENTRE DÉFICIT FISCAL Y DEUDA PÚBLICA: AUSTRALIA",
       subtitle = "Cada punto representa un año | Línea morada: tendencia general",
       x = "Déficit Fiscal (% PIB) →", 
       y = "Deuda/PIB (% PIB) ↑",
       caption = "Déficit negativo = Gastos > Ingresos\nDéficit positivo = Superávit") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(p_relacion_mejorado)

