## Big Data y Machine Learning para Economia Aplicada
## Precio de la vivienda en Cali — preparacion de la base
## Autor: Elkin Mola
## Lee:     01_data/input/vivienda_cali.rds y diccionario_vivienda.csv (ya incluidos en el repositorio)
## Escribe: 01_data/output/vivienda_cali.csv (la leen todas las partes),
##          vivienda_cali.xlsx y diccionario_vivienda.csv (para consulta)

##==: 0. initial setup :==##

solo_entorno <- TRUE ; source("00_run.R")

##==: 1. load data :==##

## trust=T porque rio pide confirmar que el .rds viene de una fuente confiable (es el del repositorio)
data <- import("01_data/input/vivienda_cali.rds" , trust=T)
dictionary <- import("01_data/input/diccionario_vivienda.csv")

## validar: la base trae las variables del diccionario, un aviso por fila y la particion fija
stopifnot(nrow(data) > 0 ,
          all(c("id","tipo","area","banos","lon","lat","precio","vis","muestra") %in% names(data)) ,
          all(c("entrenamiento","prueba") %in% data$muestra) ,
          !anyDuplicated(data$id))
table(data$muestra)

##==: 2. export data :==##

export(data , "01_data/output/vivienda_cali.csv")
export(data , "01_data/output/vivienda_cali.xlsx")
export(dictionary , "01_data/output/diccionario_vivienda.csv")
