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

#Evolución de la Deuda Pública (% de PIB) en Australia
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

#Evolución del Gasto Público
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

#Relación entre Déficit Fiscal y Gasto Público
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

#Evolución del Défict y la Deuda Pública
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


#Relación entre Déficit y Deuda
g2<-ggplot(politica_fiscal, aes(x = Déficit_Fiscal, y = Deuda_PIB)) +
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

ggplotly(g2)


