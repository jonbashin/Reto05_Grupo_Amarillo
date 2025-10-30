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
paleta<-c("#c88fb2",  "#8db41c",  "#93044e","#D1006F",  "#F5F0E6","#4D4D4D")

#---------------
#GRÁFICOS CON LOS DATASETS SEPARADOS
#Tasa de interés anual
interes_anual$observation_date <- as.Date(interes_anual$observation_date)
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

#Tasa de interés trimestral
interes_trimestral$observation_date<-as.Date(interes_trimestral$observation_date)
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


#GRÁFICOS CON INTERÉS ANUAL Y TRIMESTRAL, Y TIPO DE CAMBIO JUNTOS
#Comparación entre los tipos de interés
ggplot(cambio_interes) +
  geom_line(aes(x = observation_date, y = interes_anual, color = "Anual"), size = 1) +
  geom_line(aes(x = observation_date, y = interes_trimestral, color = "Trimestral"), size = 1) +
  scale_color_manual(
    values = c("Anual" = paleta[2],  # Verde
               "Trimestral" = paleta[4]), # Rosa/magenta
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

#Evolución del tipo de cambio
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

#Promedio de interés anual por año
df<-cambio_interes %>%
  group_by(Año) %>%
  summarise(interes_promedio = mean(interes_anual, na.rm = TRUE))
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
