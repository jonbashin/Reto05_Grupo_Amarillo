
# PRINCIPAL R


#Cargar librerias
# Lista de librerías necesarias
librerias <- c(
  "readxl", "dplyr", "ggplot2", "plotly", "lubridate", "tidyr", "naniar",
  "zoo", "readr", "scales", "gridExtra", "purrr", "forecast", "VIM", 
  "tseries", "astsa", "Metrics", "reshape2", "cowplot", "ggrepel"
)

# Función para instalar y cargar librerías
instalar_y_cargar <- function(pkgs){
  for(pkg in pkgs){
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

instalar_y_cargar(librerias)



# ----------LIMPIEZA
{
  source('Scripts/Scripts_preprocesamiento/LimpiezaDatos1.R', encoding = "UTF-8")
}
rm(list=ls())


# ----------ANALISIS EXPLORATORIO
{ 
  source('Scripts/Scripts_EDA/Script_Analisis_Exploratorio.R', encoding = "UTF-8")

}
rm(list=ls())


# ---------ECONOMIA
{
  source('Scripts/Script_Economia/Script Economia.R', encoding = "UTF-8")
}
rm(list=ls())


# ----------MODELADO
{
  source('Scripts/Scripts_modelos/Modelado.R', encoding = "UTF-8")
}

rm(list=ls())