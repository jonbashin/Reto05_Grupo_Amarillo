# ---------------------------------------------------------------
# RETO 5 InfluxDB + R
# EQUIPO: Amarillo
# ---------------------------------------------------------------
#-------------------------------------------------
#Libreria
library(influxdbclient)
library(influxdbr)
library(json64)
library(jsonlite)
require(httr)
require(readr)
#----------------------------------------------

#CARGAR DATOS
data<- read.csv("Escritorio/DATA SCIENCE/Datos_Limpios_Australia.csv")
summary(data)
data <- data[,1:8 ]

#--------------------------------
#CONECTAR R CON INFLUXDB

client <- InfluxDBClient$new(
  url = "http://localhost:8086",
  token = "J6Wm44qLlZ5GJaXr-0y91Qp7PpNHKE2zYxzX8SkBH0G6Xv2SOADcU07-b-QThNiCmYoZTk6JzWoynGQdSLlaDw==",
  org = "mondragon"
)
client$health()

#-------------------------
#CREACION DE BUCKET VIA API

INFLUX_TOKEN = "vuestro_token"
Influx_Authorization_Header <- paste("Token", INFLUX_TOKEN)
INFLUX_URL = "http://localhost:8086/api/v2/buckets"

INFLUX_ORG_ID <- '{
  "name": "20251017_bucket",
  "orgID": "organization_id_vuestro"
}'

response <- POST(INFLUX_URL,
                 body = INFLUX_ORG_ID,
                 accept("application/csv"),
                 content_type("application/json"),
                 add_headers(Authorization = Influx_Authorization_Header)
)

#-------------------------------------
#CREACION DE BUCKET

INFLUX_TOKEN = "J6Wm44qLlZ5GJaXr-0y91Qp7PpNHKE2zYxzX8SkBH0G6Xv2SOADcU07-b-QThNiCmYoZTk6JzWoynGQdSLlaDw=="
Influx_Authorization_Header <- paste("Token", INFLUX_TOKEN)
INFLUX_URL = "http://localhost:8086/api/v2/buckets"

INFLUX_ORG_ID <- '{
  "name": "20251017_bucket",
  "orgID": "628dd83ec0ebac07"
}'

response <- POST(INFLUX_URL,
                 body = INFLUX_ORG_ID,
                 accept("application/csv"),
                 content_type("application/json"),
                 add_headers(Authorization = Influx_Authorization_Header)
)

#-----------------------------------
#DATA FORMATING

data$day<- 1

data$fechas <- as.Date(with(data, paste(Year,Month,day,sep = "-")), "%Y-%m-%d")

data$times<- "00:00:01.0"

data$mifecha <- paste(data$fechas, data$times)

data$mifecha<- strptime(data$mifecha, "%Y-%m-%d %H:%M:%S", tz="GMT")

data$mifecha<- as.POSIXct(data$mifecha)

data$Country<- as.character(data$Country)

data$Code<- as.character(data$Code)

data$ContinentCode<- as.character(data$ContinentCode)

#---------------------------------------
#ESTRUCTURAS DE DATOS 
response <- client$write(
  data[1:50, ],
  bucket = "20251017_bucket",
  precision = "s",
  measurementCol = "Country",
  tagCols = c("Code", "Country", "ContinentCode"),
  fieldCols = c("Stock.market.index", "Unemployment.rate.percent"),
  timeCol = "mifecha"
)

str(data)


#-------------------
#CONSULTAS
#----------------------
#EVOLUCION TEMPORAL DEL INDICE BURSATIL
query1 <- 'from(bucket: "20251017_bucket")
  |> range(start: 1996-01-01T00:00:00Z, stop: 2022-12-31T23:59:59Z)
  |> filter(fn: (r) => r._field == "Stock.market.index")
  |> aggregateWindow(every: 3mo, fn: mean, createEmpty: false)
  |> yield(name: "Quarterly_Stock_Index")'


response <- POST(
  url = "http://localhost:8086/api/v2/query?org=mondragon",
  add_headers(
    Authorization = paste("Token", "J6Wm44qLlZ5GJaXr-0y91Qp7PpNHKE2zYxzX8SkBH0G6Xv2SOADcU07-b-QThNiCmYoZTk6JzWoynGQdSLlaDw=="),
    "Accept" = "application/csv",
    "Content-Type" = "application/vnd.flux"
  ),
  body = query1
)

# Leer el resultado como data frame
result1 <- read.csv(text = content(response, "text"), stringsAsFactors = FALSE)
head(result1)

#------------------------------------------

# CONSULTA 2: EVOLUCIÓN MENSUAL DEL DESEMPLEO
query2 <- 'from(bucket: "20251017_bucket")
  |> range(start: 1996-01-01T00:00:00Z, stop: 2022-12-31T23:59:59Z)
  |> filter(fn: (r) => r._field == "Unemployment.rate.percent")
  |> aggregateWindow(every: 1mo, fn: mean, createEmpty: false)
  |> yield(name: "Monthly_Unemployment")'

response2 <- POST(
  url = "http://localhost:8086/api/v2/query?org=mondragon",
  add_headers(
    Authorization = paste("Token", "J6Wm44qLlZ5GJaXr-0y91Qp7PpNHKE2zYxzX8SkBH0G6Xv2SOADcU07-b-QThNiCmYoZTk6JzWoynGQdSLlaDw=="),
    "Accept" = "application/csv",
    "Content-Type" = "application/vnd.flux"
  ),
  body = query2
)

result2 <- read.csv(text = content(response2, "text"), stringsAsFactors = FALSE)
head(result2)

#-------------------
# CONSULTA 3: RELACIÓN DESEMPLEO / ÍNDICE BURSÁTIL
query3 <- 'from(bucket: "20251017_bucket")
  |> range(start: 1996-01-01T00:00:00Z, stop: 2022-12-31T23:59:59Z)
  |> filter(fn: (r) => r._field == "Unemployment.rate.percent" or r._field == "Stock.market.index")
  |> aggregateWindow(every: 1mo, fn: mean, createEmpty: false)
  |> pivot(rowKey:["_time"], columnKey:["_field"], valueColumn:"_value")
  |> map(fn: (r) => ({ r with Ratio_Unemployment_to_Stock: r.Unemployment.rate.percent / r.Stock.market.index }))
  |> yield(name: "Unemployment_vs_Stock")'

response3 <- POST(
  url = "http://localhost:8086/api/v2/query?org=mondragon",
  add_headers(
    Authorization = paste("Token", "J6Wm44qLlZ5GJaXr-0y91Qp7PpNHKE2zYxzX8SkBH0G6Xv2SOADcU07-b-QThNiCmYoZTk6JzWoynGQdSLlaDw=="),
    "Accept" = "application/csv",
    "Content-Type" = "application/vnd.flux"
  ),
  body = query3
)

result3 <- read.csv(text = content(response3, "text"), stringsAsFactors = FALSE)
head(result3)
