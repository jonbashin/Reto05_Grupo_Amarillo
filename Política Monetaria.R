#Cargar librerías
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(gridExtra)
library(purrr)
library(lubridate)

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
pal_laboral <- c(
  verde = "#6DBC00",
  magenta = "#E20074",
  berenjena = "#7A1E5A",
  beige = "#F4EDE4",
  gris_texto = "#333333",
  gris_fondo = "#E0E0E0"
)

#---------------
#GRÁFICOS CON LOS DATASETS SEPARADOS
#Tasa de interés anual
interes_anual$observation_date<-as.Date(interes_anual$observation_date)
ggplot(interes_anual, aes(x = observation_date, y = interes_anual)) +
  geom_line(color = pal_laboral["magenta"], size = 1) +
  labs(title = "Tasa de interés anual en Australia",
       x = "Fecha",
       y = "Tasa de interés anual (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal_laboral["gris_fondo"]),
    panel.background = element_rect(fill = pal_laboral["gris_fondo"]),
    plot.title = element_text(face = "bold", color = pal_laboral["gris_texto"]),
    axis.text = element_text(color = pal_laboral["gris_texto"]),
    axis.title = element_text(color = pal_laboral["gris_texto"])
  )

#Tasa de interés trimestral
interes_trimestral$observation_date<-as.Date(interes_trimestral$observation_date)
ggplot(interes_trimestral, aes(x = observation_date, y = interes_trimestral)) +
  geom_line(color = pal_laboral["magenta"], size = 1) +
  labs(title = "Tasa de interés trimestral en Australia",
       x = "Fecha",
       y = "Tasa de interés trimestral (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal_laboral["gris_fondo"]),
    panel.background = element_rect(fill = pal_laboral["gris_fondo"]),
    plot.title = element_text(face = "bold", color = pal_laboral["gris_texto"]),
    axis.text = element_text(color = pal_laboral["gris_texto"]),
    axis.title = element_text(color = pal_laboral["gris_texto"])
  )

#GRÁFICOS CON INTERÉS ANUAL Y TRIMESTRAL, Y TIPO DE CAMBIO JUNTOS
#Comparación entre los tipos de interés
ggplot(cambio_interes) +
  geom_line(aes(x = observation_date, y = interes_anual, color = "Anual"), size = 1) +
  geom_line(aes(x = observation_date, y = interes_trimestral, color = "Trimestral"), size = 1) +
  scale_color_manual(
    values = c(
      "Anual" = pal_laboral[["verde"]],
      "Trimestral" = pal_laboral[["magenta"]]
    ),
    name = "Tipo de interés"
  ) +
  labs(
    title = "Comparación de tasas de interés en Australia",
    x = "Fecha",
    y = "Tasa de interés (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal_laboral[["gris_fondo"]], color = NA),
    panel.background = element_rect(fill = pal_laboral[["gris_fondo"]], color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    plot.title = element_text(color = pal_laboral[["gris_texto"]], face = "bold"),
    axis.text = element_text(color = pal_laboral[["gris_texto"]]),
    axis.title = element_text(color = pal_laboral[["gris_texto"]]),
    legend.background = element_rect(fill = pal_laboral[["beige"]], color = NA),
    legend.title = element_text(color = pal_laboral[["gris_texto"]]),
    legend.text = element_text(color = pal_laboral[["gris_texto"]])
  )

#Evolución del tipo de cambio
ggplot(cambio_interes, aes(x = observation_date, y = tipo_cambio)) +
  geom_line(color = pal_laboral["berenjena"], size = 1) +
  labs(title = "Evolución del tipo de cambio AUD/USD",
       x = "Fecha",
       y = "Tipo de cambio") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal_laboral["gris_fondo"]),
    plot.title = element_text(color = pal_laboral["gris_texto"], face = "bold")
  )

#Promedio de interés anual por año
df<-cambio_interes %>%
  group_by(Año) %>%
  summarise(interes_promedio = mean(interes_anual, na.rm = TRUE))
ggplot(df, aes(x = factor(Año), y = interes_promedio)) +
  geom_col(fill = pal_laboral["verde"]) +
  geom_text(aes(label = round(interes_promedio, 2)), vjust = -0.5, color = pal_laboral["gris_texto"]) +
  labs(title = "Promedio anual de la tasa de interés en Australia",
       x = "Año",
       y = "Tasa de interés promedio (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = pal_laboral["gris_fondo"]),
    plot.title = element_text(color = pal_laboral["gris_texto"], face = "bold")
  )

