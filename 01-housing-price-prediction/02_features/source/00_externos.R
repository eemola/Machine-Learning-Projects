## Big Data y Machine Learning para Economia Aplicada
## Precio de la vivienda en Cali — Datos externos: de los archivos pesados a archivos pequenos
## Autor: Elkin Mola
## NO hace parte de 00_run.R: se corrio una sola vez, con los datos pesados ya descargados.
## Necesita los datos pesados (censo, manzanas, barrios y capas de IDESC) en 02_features/input/,
## cada uno en su carpeta: cali_barrios/, censo/, distancias/ y valle microdatos/
## Escribe: 02_features/input/externos/ (cali_barrios/, censo/, distancias/), que si viajan en el zip
## Fuentes: IDESC (Alcaldia de Cali, CC BY-SA), DANE (MGN y Censo 2018), OpenStreetMap (ODbL)

##==: 0. initial setup :==##

solo_entorno <- TRUE ; source("00_run.R")
p_load(sf , data.table)

dir.create("02_features/input/externos/cali_barrios" , showWarnings=F , recursive=T)
dir.create("02_features/input/externos/censo" , showWarnings=F , recursive=T)
dir.create("02_features/input/externos/distancias" , showWarnings=F , recursive=T)

##==: 1. barrios :==##

barrios = st_read("02_features/input/cali_barrios/pdt_dpa_barrios_sectores.shp" , options = "ENCODING=LATIN1")
barrios = st_transform(barrios[ , c("barnombre" , "barcomuna")] , crs = 3116)

export(barrios , "02_features/input/externos/cali_barrios/barrios.rds")

##==: 2. censo: manzanas y estrato :==##

## 2.1. manzanas del DANE (solo Cali urbano)
caja = st_as_sfc(st_bbox(c(xmin = -76.65 , ymin = 3.25 , xmax = -76.40 , ymax = 3.62) , crs = 4326))

manzanas = st_read("02_features/input/censo/MGN_ANM_MANZANA.shp" , wkt_filter = st_as_text(caja))
manzanas = manzanas[manzanas$MPIO_CDPMP == "76001" & manzanas$CLAS_CCDGO == "1" , ]
manzanas = st_transform(manzanas[ , "COD_DANE_A"] , crs = 3116)

export(manzanas , "02_features/input/externos/censo/manzanas.rds")

## 2.2. estrato del censo por manzana y tipo
viv = import("02_features/input/valle microdatos/76_ValleDelCauca_CSV/CNPV2018_1VIV_A2_76.CSV" , setclass = "data.table")
mgn = import("02_features/input/valle microdatos/76_ValleDelCauca_CSV/CNPV2018_MGN_A2_76.CSV" , setclass = "data.table")

## filtrar Cali urbano y unir con la manzana
viv_cali = viv[U_MPIO %in% c("001" , 1) & UA_CLASE == 1]

censo = merge(viv_cali[ , .(COD_ENCUESTAS , V_TIPO_VIV , VA1_ESTRATO)] ,
              mgn[ , .(COD_ENCUESTAS , COD_DANE_ANM)] ,
              by = "COD_ENCUESTAS")

## limpiar variables
censo[ , tipo := fcase(V_TIPO_VIV == 1 , "Casa" ,
                       V_TIPO_VIV == 2 , "Apartamento" ,
                       default = "Otro")]

censo[ , estrato_limpio := fifelse(VA1_ESTRATO %in% 1:6 , VA1_ESTRATO , NA_real_)]

## estrato mediano por manzana
manzanas_censo = censo[tipo %in% c("Casa" , "Apartamento") ,
                       .(estrato_mediano = median(estrato_limpio , na.rm = TRUE)) ,
                       by = .(COD_DANE_A = COD_DANE_ANM , tipo)]

export(manzanas_censo , "02_features/input/externos/censo/manzanas_censo.rds")

##==: 3. capas de equipamientos (IDESC) :==##

## 3.1. parques y zonas verdes
parques = st_read("02_features/input/distancias/espacio publico/epou_epu_espacio_publico_efectivoPolygon.shp" , options = "ENCODING=LATIN1")
parques = st_transform(parques[ , "epepotele"] , crs = 3116)
export(parques , "02_features/input/externos/distancias/parques.rds")

## 3.2. salud
hospitales = st_read("02_features/input/distancias/salud/eqp_uco_salud.shp" , options = "ENCODING=LATIN1")
hospitales = st_transform(hospitales[ , c("categoria" , "nivel")] , crs = 3116)
export(hospitales , "02_features/input/externos/distancias/salud.rds")

## 3.3. educacion
educacion = st_read("02_features/input/distancias/educacion/eqp_uco_educacion.shp" , options = "ENCODING=LATIN1")
educacion = st_transform(educacion[ , "nivel"] , crs = 3116)
export(educacion , "02_features/input/externos/distancias/educacion.rds")

## 3.4. mio
mio = st_read("02_features/input/distancias/movilidad/pdt_pmep_movilidad_sitmPolygon.shp" , options = "ENCODING=LATIN1")
mio = st_transform(mio[ , "movpotele"] , crs = 3116)
export(mio , "02_features/input/externos/distancias/mio.rds")

## 3.5. ecoparques
eco_parques = st_read("02_features/input/distancias/eco_parques/amb_eep_aeie_ecoparques.shp" , options = "ENCODING=LATIN1")
eco_parques = st_transform(eco_parques[ , "nombre"] , crs = 3116)
export(eco_parques , "02_features/input/externos/distancias/eco_parques.rds")

## 3.6. centros comerciales (OpenStreetMap)
malls = import("02_features/input/distancias/centros_comerciales_osm.csv")
malls = malls[malls$incluir == 1 , ]
malls = st_as_sf(malls , coords = c("lon" , "lat") , crs = 4326)
malls = st_transform(malls[ , "nombre"] , crs = 3116)
export(malls , "02_features/input/externos/distancias/malls.rds")

##==: 4. revisar :==##

list.files("02_features/input/externos" , recursive = TRUE)

## se borran los objetos que se crearon aqui (los espaciales pesan)
rm(barrios , caja , manzanas , viv , mgn , viv_cali , censo , manzanas_censo ,
   parques , hospitales , educacion , mio , eco_parques , malls)
