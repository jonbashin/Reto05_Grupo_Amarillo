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

#Evolución del Déficit Fiscal
ggplot(politica_fiscal, aes(x = Fecha, y = Déficit_Fiscal)) +
  geom_line(color = "#E20074", size = 1) +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  labs(
    title = "Evolución del déficit fiscal en Australia",
    x = "Fecha",
    y = "Déficit fiscal"
  ) +
  theme_minimal(base_size = 14)

#Evolución de la Deuda Pública (% de PIB) en Australia
ggplot(politica_fiscal, aes(x = Fecha, y = Deuda_PIB)) +
  geom_line(color = "#6DBC00", size = 1.2) +
  labs(
    title = "Deuda pública de Australia (% del PIB)",
    x = "Fecha",
    y = "Deuda / PIB (%)"
  ) +
  theme_minimal(base_size = 14)

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

#
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

# --- Gráfico 2: Relación entre Déficit y Deuda ---
g2 <- ggplot(politica_fiscal, aes(x = Déficit_Fiscal, y = Deuda_PIB)) +
  geom_point(color = "#8e44ad", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Relación entre Déficit Fiscal y Deuda Pública",
       x = "Déficit Fiscal (% PIB)", y = "Deuda Pública (% PIB)") +
  theme_minimal(base_size = 13)

ggplotly(g2)

# --- Gráfico 3: Relación entre Gasto Público y Déficit ---
g3 <- ggplot(politica_fiscal, aes(x = Gasto_Público, y = Déficit_Fiscal)) +
  geom_point(color = "#16a085", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  labs(title = "Relación entre Gasto Público y Déficit Fiscal",
       x = "Gasto Público (% PIB)", y = "Déficit Fiscal (% PIB)") +
  theme_minimal(base_size = 13)

ggplotly(g3)

