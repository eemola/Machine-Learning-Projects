## -------------------------------------------------------------------------------------------------#
## Big Data y Machine Learning para Economia Aplicada
## Clasificacion de vivienda de interes social (VIS) en Cali — Preparacion: variables de ubicacion (barrio, manzana, estrato y distancias)
## Autor: Elkin Mola
## Lee:     02_features/input/vivienda_cali.csv (copiada de 01_data/output/) y los archivos de 02_features/input/externos/
## Escribe: 02_features/output/vivienda_cali_variables.csv (la base con las variables nuevas)
## Ninguna variable de esta etapa usa el precio: se calculan igual para entrenamiento y prueba
## La imputacion de habitaciones NO se hace aqui: se aprende dentro de cada pliegue (partes 2 a 4)
## ------------------------------------------------------------------------------------------------#
##==: 0. initial setup :==##

solo_entorno <- TRUE ; source("00_run.R")
p_load(sf)

dir.create("02_features/output" , showWarnings=F , recursive=T)

##==: 1. load data :==## ----

viviendas = import("02_features/input/vivienda_cali.csv")

## 1.1. archivos externos (ya convertidos, ver 00_externos.R)
## los objetos espaciales se leen con readRDS para que conserven la clase sf
barrios = readRDS("02_features/input/externos/cali_barrios/barrios.rds")
manzanas = readRDS("02_features/input/externos/censo/manzanas.rds")
manzanas_censo = import("02_features/input/externos/censo/manzanas_censo.rds")

parques = readRDS("02_features/input/externos/distancias/parques.rds")
hospitales = readRDS("02_features/input/externos/distancias/salud.rds")
educacion = readRDS("02_features/input/externos/distancias/educacion.rds")
mio = readRDS("02_features/input/externos/distancias/mio.rds")
malls = readRDS("02_features/input/externos/distancias/malls.rds")
eco_parques = readRDS("02_features/input/externos/distancias/eco_parques.rds")

##==: 2. geometria de los avisos :==## ----

avisos_sf = st_as_sf(viviendas ,
                     coords = c("lon" , "lat") ,
                     crs = 4326 ,
                     remove = FALSE)

## 2.1. pasar a metros (MAGNA-SIRGAS, EPSG:3116)
avisos_sf = st_transform(avisos_sf , crs = 3116)

stopifnot(st_crs(manzanas) == st_crs(avisos_sf) , st_crs(barrios) == st_crs(avisos_sf))

##==: 3. barrio por poligono :==## ----

avisos_con_barrio = st_join(avisos_sf , barrios , join = st_within)

## 3.1. barrios NA al mas cercano (limite de 200 m)
barrios_buenos =  avisos_con_barrio %>%
                  filter(!is.na(barnombre)) %>%
                  mutate(sin_barrio_poligono = 0)

## filtramos los faltantes y quitamos las columnas del barrio
barrios_faltantes = avisos_con_barrio %>%
                    filter(is.na(barnombre)) %>%
                    select(-barnombre , -barcomuna)

## identificamos el barrio mas cercano y su distancia exacta en metros
indices_cercanos = st_nearest_feature(barrios_faltantes , barrios)
distancias = st_distance(barrios_faltantes , barrios[indices_cercanos , ] , by_element = TRUE)

## unimos, ponemos la marca y aplicamos la regla de 200 m
barrios_faltantes = barrios_faltantes %>%
                    st_join(barrios , join = st_nearest_feature) %>%
                    mutate(
                      sin_barrio_poligono = 1 ,
                      distancia_m = as.numeric(distancias) ,
                      barnombre = ifelse(distancia_m > 200 , "Sin barrio (> 200 m)" , barnombre)
                    ) %>%
                    select(-distancia_m)

## 3.2. consolidar
avisos_sf = rbind(barrios_buenos , barrios_faltantes)
rm(avisos_con_barrio , barrios_buenos , barrios_faltantes , indices_cercanos , distancias)

##==: 4. manzana del censo :==## ----

avisos_manzana = st_join(avisos_sf , manzanas , join = st_within)

## 4.1. manzanas NA a la mas cercana (limite de 200 m)
manzanas_buenas = avisos_manzana %>%
                  filter(!is.na(COD_DANE_A)) %>%
                  mutate(sin_manzana = 0)

manzanas_faltantes = avisos_manzana %>%
                     filter(is.na(COD_DANE_A)) %>%
                     select(-COD_DANE_A)

indices_cercanos = st_nearest_feature(manzanas_faltantes , manzanas)
dist_manzanas = st_distance(manzanas_faltantes , manzanas[indices_cercanos , ] , by_element = TRUE)

## unimos, ponemos la marca y borramos el codigo si supera los 200 m
manzanas_faltantes = manzanas_faltantes %>%
                     st_join(manzanas , join = st_nearest_feature) %>%
                     mutate(
                       sin_manzana = 1 ,
                       distancia_m = as.numeric(dist_manzanas) ,
                       COD_DANE_A = ifelse(distancia_m > 200 , NA , COD_DANE_A)) %>%
                     select(-distancia_m)

## 4.2. consolidar
avisos_sf = rbind(manzanas_buenas , manzanas_faltantes)
rm(avisos_manzana , manzanas_buenas , manzanas_faltantes , indices_cercanos , dist_manzanas)

##==: 5. estrato con el censo :==## ----

avisos_sf = avisos_sf %>%
            left_join(manzanas_censo , by = c("COD_DANE_A" , "tipo"))

## 5.1. respaldo: estrato mediano de la comuna (solo con avisos de entrenamiento)
respaldo_estrato = avisos_sf %>%
                   st_drop_geometry() %>%
                   filter(muestra == "entrenamiento" , !is.na(estrato_mediano)) %>%
                   group_by(barcomuna) %>%
                   summarise(estrato_comuna = round(median(estrato_mediano)))

## 5.2. consolidar con coalesce
avisos_sf = avisos_sf %>%
            left_join(respaldo_estrato , by = "barcomuna") %>%
            mutate(estrato = round(coalesce(estrato_mediano , estrato_comuna))) %>%
            select(-estrato_mediano , -estrato_comuna)

rm(respaldo_estrato)

##==: 6. habitaciones: solo se marca el faltante :==## ----

avisos_sf$habitaciones_na = ifelse(is.na(avisos_sf$habitaciones) , 1 , 0)

##==: 7. distancias :==## ----

## 7.1. parques
parques$area_m2 = as.numeric(st_area(parques))

## 7.1.1. parque mas cercano (cualquiera)
parques_todos = parques[parques$epepotele == "Parque" , ]

idx_parque = st_nearest_feature(avisos_sf , parques_todos)
dist_parque = st_distance(avisos_sf , parques_todos[idx_parque , ] , by_element = T)

avisos_sf$dist_parque_m = as.numeric(dist_parque)

## 7.1.2. parque grande mas cercano (>= 5.000 m2)
parques_grandes = parques_todos[parques_todos$area_m2 >= 5000 , ]

idx_grande = st_nearest_feature(avisos_sf , parques_grandes)
dist_grande = st_distance(avisos_sf , parques_grandes[idx_grande , ] , by_element = T)

avisos_sf$dist_parque_grande_m = as.numeric(dist_grande)

## 7.1.3. area verde a 500 m (parques + zonas verdes)
verdes = parques[parques$epepotele %in% c("Parque" , "Zona verde") , ]

cerca = st_is_within_distance(avisos_sf , verdes , dist = 500)
avisos_sf$area_verde_500m = sapply(cerca , function(i) sum(verdes$area_m2[i]))

rm(parques_todos , parques_grandes , verdes , idx_parque , idx_grande , dist_parque , dist_grande , cerca)

## 7.2. ecoparques
idx_eco = st_nearest_feature(avisos_sf , eco_parques)
dist_eco = st_distance(avisos_sf , eco_parques[idx_eco , ] , by_element = T)

avisos_sf$dist_ecoparque_m = as.numeric(dist_eco)

rm(idx_eco , dist_eco)

## 7.3. salud
## 7.3.1. cualquier centro de salud
idx_salud = st_nearest_feature(avisos_sf , hospitales)
dist_salud = st_distance(avisos_sf , hospitales[idx_salud , ] , by_element = T)

avisos_sf$dist_salud_m = as.numeric(dist_salud)

## 7.3.2. hospital o clinica con hospitalizacion
hosp = hospitales[grepl("Hospitales" , hospitales$categoria) , ]

idx_hosp = st_nearest_feature(avisos_sf , hosp)
dist_hosp = st_distance(avisos_sf , hosp[idx_hosp , ] , by_element = T)

avisos_sf$dist_hospital_m = as.numeric(dist_hosp)

## 7.3.3. hospital de alta complejidad (nivel 3)
hosp_alta = hosp[hosp$nivel == 3 , ]

idx_alta = st_nearest_feature(avisos_sf , hosp_alta)
dist_alta = st_distance(avisos_sf , hosp_alta[idx_alta , ] , by_element = T)

avisos_sf$dist_hospital_alta_m = as.numeric(dist_alta)

rm(hosp , hosp_alta , idx_salud , idx_hosp , idx_alta , dist_salud , dist_hosp , dist_alta)

## 7.4. educacion
## 7.4.1. colegios (basica y media)
colegios = educacion[grepl("BASICA|MEDIA|PRIMARIA|SECUNDARIA" , educacion$nivel) , ]

idx_colegio = st_nearest_feature(avisos_sf , colegios)
dist_colegio = st_distance(avisos_sf , colegios[idx_colegio , ] , by_element = T)

avisos_sf$dist_colegio_m = as.numeric(dist_colegio)

## 7.4.2. universidades (educacion superior)
univ = educacion[grepl("SUPERIOR" , educacion$nivel) , ]

idx_univ = st_nearest_feature(avisos_sf , univ)
dist_univ = st_distance(avisos_sf , univ[idx_univ , ] , by_element = T)

avisos_sf$dist_universidad_m = as.numeric(dist_univ)

rm(colegios , univ , idx_colegio , idx_univ , dist_colegio , dist_univ)

## 7.5. mio
mio = mio[mio$movpotele == "Infraestructura SITM" , ]

idx_mio = st_nearest_feature(avisos_sf , mio)
dist_mio = st_distance(avisos_sf , mio[idx_mio , ] , by_element = T)

avisos_sf$dist_mio_m = as.numeric(dist_mio)

## 7.6. centros comerciales
idx_mall = st_nearest_feature(avisos_sf , malls)
dist_mall = st_distance(avisos_sf , malls[idx_mall , ] , by_element = T)

avisos_sf$dist_mall_m = as.numeric(dist_mall)

rm(idx_mio , idx_mall , dist_mio , dist_mall)

##==: 8. marca de avisos fuera de Cali y orden :==## ----

avisos_sf$fuera_cali = ifelse(is.na(avisos_sf$COD_DANE_A) , 1 , 0)

avisos_sf = avisos_sf[order(avisos_sf$id) , ]

##==: 9. revisar :==## ----

stopifnot(nrow(avisos_sf) == nrow(viviendas) , !anyDuplicated(avisos_sf$id))

print(table(avisos_sf$muestra))

vars = c("barnombre" , "estrato" , "dist_parque_m" , "dist_parque_grande_m" , "area_verde_500m" , "dist_ecoparque_m" ,
         "dist_salud_m" , "dist_hospital_m" , "dist_hospital_alta_m" , "dist_colegio_m" , "dist_universidad_m" ,
         "dist_mio_m" , "dist_mall_m")
print(colSums(is.na(st_drop_geometry(avisos_sf)[ , vars])))

##==: 10. export :==## ----

## sin geometria (lon y lat siguen como columnas) y sin el codigo de la manzana,
## que es un identificador y no una variable del modelo
avisos = st_drop_geometry(avisos_sf)
avisos$COD_DANE_A = NULL

export(avisos , "02_features/output/vivienda_cali_variables.csv")

##==: 11. limpiar memoria :==## ----

## se borran los objetos que se crearon aqui (los espaciales pesan); el resultado ya quedo en el csv
rm(viviendas , barrios , manzanas , manzanas_censo , parques , hospitales , educacion , mio , malls ,
   eco_parques , avisos_sf , avisos , vars)
