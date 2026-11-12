## Proyecto de machine learning (Big Data y Machine Learning para Economia Aplicada, Icesi)
## Precio de la vivienda en Cali — 00_run.R: el entorno comun y la corrida del proyecto de principio a fin
## Autor: Elkin Mola
##
## Es el unico script fuera de las carpetas. Tiene dos partes:
##  - secciones 0 a 2: el entorno comun (paquetes y funciones). Cada script de las
##    carpetas lo carga con  solo_entorno <- TRUE ; source("00_run.R")  y se
##    detiene ahi, sin volver a correr el proyecto
##  - seccion 3: la corrida completa, solo cuando se lanza este archivo
##
## Uso (no hace falta abrir el .Rproj ni cambiar la carpeta de trabajo):
##  - RStudio: abrir este archivo y presionar Source
##  - Terminal: Rscript 00_run.R  (o con la ruta completa a este archivo)
## Deja:
##  - 06_report/output/informe_general.pdf   el informe
##  - 05_final_model/output/predicciones_precio.csv   las predicciones de la prueba
##  - 06_report/output/logs/                 el registro de cada etapa y sessionInfo()
## Que hace la corrida:
##  1. copia a cada etapa lo que lee en su carpeta input/ y corre los scripts que lo necesiten: si faltan sus resultados o si
##     su script, sus datos o este archivo cambiaron. Los demas se reutilizan
##     (no recalcula de nuevo lo que ya esta al dia)
##  2. valida que cada uno deje sus resultados, y predicciones.csv
##  3. arma el informe con 06_report/source/informe_general.Rmd, que lee los
##     resultados y las respuestas escritas en 01_baselines.Rmd ... 04_closing.Rmd
## Nunca escribe en los .Rmd: lo que se redacte ahi se conserva.

##==: 0. initial setup :==##

## clean environment (si lo cargo un script de las carpetas, conserva su marca)
rm(list = setdiff(ls() , "solo_entorno"))

## TRUE: vuelve a correr todos los analisis aunque esten al dia (para la prueba final)
forzar <- FALSE

## load/install packages: los de uso comun (rio para leer y escribir, dplyr para
## manipular, glmnet para Ridge, Lasso y Elastic Net) y ggplot2 + scales para
## las figuras del reporte (ggplot2 les da el formato del PDF)
## el repositorio de CRAN para instalar lo que falte: en un computador nuevo,
## Rscript no trae uno definido y install.packages() se detiene
repos <- getOption("repos")
if (is.null(repos) || !("CRAN" %in% names(repos)) || repos[["CRAN"]] %in% c("@CRAN@" , "")){
   options(repos = c(CRAN = "https://cloud.r-project.org"))
}
if (!require(pacman)){ install.packages("pacman") ; library(pacman) }
## yardstick solo para el r2 (rsq_trad_vec); se llama con yardstick:: y no se
## carga, porque su rmse() pide un data frame y taparia al de la seccion 1
p_load(rio , dplyr , glmnet , ggplot2 , scales , janitor)
if (!requireNamespace("yardstick" , quietly=T)) install.packages("yardstick")

## sin notacion cientifica en la consola
options(scipen = 999)

## cuando 00_run.R corre un script en la terminal, los graficos que imprima no se
## guardan en un Rplots.pdf suelto (en RStudio se siguen viendo como siempre)
if (!interactive()) pdf(NULL)

##==: 1. las funciones de las clases :==##

## las metricas, en millones de pesos (como el precio)
rmse <- function(y , yhat) sqrt(mean((y - yhat)^2))
mae <- function(y , yhat) mean(abs(y - yhat))

## el r2 de validacion, con yardstick: 1 - SSE/SST, con SST medido contra la media
## de la propia validacion. 1 es perfecto y puede ser negativo
r2 <- function(y , yhat) yardstick::rsq_trad_vec(truth = y , estimate = yhat)

## k-nn: el promedio de los k vecinos mas cercanos en el mapa;
## devuelve una columna por cada k de la grilla
knn_reg <- function(coord_train , y_train , coord_nueva , k){
           pred <- matrix(NA , nrow(coord_nueva) , length(k))
           for (j in 1:nrow(coord_nueva)){
                dist <- (coord_train[,1] - coord_nueva[j,1])^2 + (coord_train[,2] - coord_nueva[j,2])^2
                vecinos <- y_train[order(dist)[1:max(k)]]
                pred[j,] <- cumsum(vecinos)[k]/k
           }
           return(pred)
}

##==: 1b. la validacion cruzada de las partes 2 y 3 :==##

## transformaciones fijas (no se aprenden de los datos, asi que se pueden
## calcular antes de partir): log del area, coordenadas centradas en un punto
## fijo de Cali y en centesimas de grado (cerca de 1,1 km), el estrato como
## categoria con sus seis niveles y log(1 + distancia)
variables_modelo <- function(datos){
                    datos %>%
                    mutate(log_area = log(area) ,
                           cx = (lon - (-76.53))*100 ,
                           cy = (lat - 3.42)*100 ,
                           estrato = factor(estrato , levels=1:6) ,
                           across(c(starts_with("dist_") , area_verde_500m) , log1p , .names="log_{.col}"))
}

## imputar habitaciones dentro del pliegue: la mediana por tipo (y la global de
## respaldo) se calcula con los avisos de ajuste y se aplica a los dos lados;
## los de validacion (o los de prueba) nunca entran en la cuenta
imputar <- function(fit , val , grupo = "tipo"){
           # 1. Mediana global como respaldo
           med_global <- median(fit$habitaciones , na.rm=T)

           # 2. Mediana por grupo, calculada solo con fit
           medianas <- fit %>%
                       group_by(across(all_of(grupo))) %>%
                       summarise(med_grupo = median(habitaciones , na.rm=T) , .groups="drop")

           # 3. Rellenar: primero la mediana del grupo, si no existe la global
           rellenar <- function(df){
                       df %>%
                       left_join(medianas , by = grupo) %>%
                       mutate(habitaciones = coalesce(habitaciones , med_grupo , med_global)) %>%
                       select(-med_grupo)
           }

           list(fit = rellenar(fit) , val = rellenar(val))
}

## ajustar una regresion con fit y predecir nuevos avisos, en millones de pesos.
## log_y = TRUE: se ajusta log(precio) y se vuelve a millones con exp() por el
## factor de Duan, la media de exp(residuo) del ajuste (sin el, exp() predice la
## mediana y no la media del precio)
ajustar_predecir <- function(formula , fit , nuevos , log_y = FALSE){
                    if (!log_y) return(predict(lm(formula , data=fit) , nuevos))
                    modelo <- lm(update(formula , log(precio) ~ .) , data=fit)
                    exp(predict(modelo , nuevos))*mean(exp(residuals(modelo)))
}

## el cv a mano para una regresion: cada pliegue
## valida una vez; se imputa, se ajusta con los demas y se predice. Devuelve la
## prediccion fuera de pliegue de cada aviso del entrenamiento
cv_lm <- function(formula , datos , pliegue , log_y = FALSE){
         pred <- rep(NA , nrow(datos))
         for (j in sort(unique(pliegue))){
              partes <- imputar(datos[pliegue!=j,] , datos[pliegue==j,])
              pred[pliegue==j] <- ajustar_predecir(formula , partes$fit , partes$val , log_y)
         }
         pred
}

## el rmse de cada pliegue, a partir de las predicciones fuera de pliegue
rmse_pliegues <- function(y , pred , pliegue){
                 sapply(sort(unique(pliegue)) , function(j) rmse(y[pliegue==j] , pred[pliegue==j]))
}

## los 5 pliegues espaciales: celdas de 0,02 grados (cerca de
## 2,2 km) y cada celda entera a un pliegue al azar (semilla 12345)
pliegue_espacial <- function(datos , celda = 0.02){
                    zona <- paste(floor(datos$lon/celda) , floor(datos$lat/celda))
                    zonas <- unique(zona)
                    set.seed(12345)
                    grupo <- sample(rep(1:5 , length.out=length(zonas)))
                    grupo[match(zona , zonas)]
}

##==: 2. lo que cada parte le entrega al reporte :==##

## cada parte guarda una lista en 0X_*/output/results_part_K.rds; los .Rmd
## de 06_report/source la leen (no recalculan nada). la lista lleva:
##  - anexo: sus filas de la tabla de modelos, hechas con fila_anexo() (metricas de validacion)
##  - elementos: sus tablas y figuras, cada una con un nombre, hechas con
##    tabla_reporte() y figura_reporte(). En el .Rmd de la parte se muestran
##    por su nombre: mostrar(p1 , "descriptivas")
##  - cualquier otra cifra que quieran citar en sus respuestas (p. ej. rmse_m0)
## los numeros van sin redondear (el informe los escribe con coma decimal); las
## figuras van sin titulo ni tema: el titulo es el pie de figura y el informe les
## pone el mismo formato a todas

## una fila de la tabla de modelos: el nombre del modelo y sus metricas de validacion
## (ya calculadas). mae y r2 son opcionales: sirve para filas con solo el rmse,
## p. ej. un rmse de validacion cruzada
fila_anexo <- function(modelo , rmse , mae = NA , r2 = NA){
  data.frame(modelo = modelo , rmse = rmse , mae = mae , r2 = r2)
}

## la tabla de modelos de varias predicciones de una vez: calcula rmse(), mae() y r2() de cada
## prediccion y arma una fila por modelo con fila_anexo(). preds: lista con
## nombre, una prediccion por modelo; y: el observado de la validacion
tabla_anexo <- function(preds , y){
  filas <- lapply(names(preds) , function(m)
    fila_anexo(m , rmse(y , preds[[m]]) ,
               mae(y , preds[[m]]) ,
               r2(y , preds[[m]])))
  do.call(rbind , filas)
}

## una tabla del informe. columnas: nombres que se muestran; encabezado: titulos
## sobre grupos de columnas, p. ej. c(" " = 1 , "Entrenamiento" = 2); grupos:
## titulos sobre grupos de filas, p. ej. c("Muestra" = 2 , "Precio" = 5)
tabla_reporte <- function(datos , titulo , nota = NULL , columnas = names(datos) ,
                          encabezado = NULL , grupos = NULL , decimales = 2){
  list(tipo = "tabla" , datos = datos , titulo = titulo , nota = nota ,
       columnas = columnas , encabezado = encabezado ,
       grupos = grupos , decimales = decimales)
}

## una figura del informe: un grafico de ggplot, o una lista de dos que salen
## lado a lado como paneles A y B. alto: en pulgadas
figura_reporte <- function(grafico , titulo , nota = NULL , alto = 3.4){
  list(tipo = "figura" , grafico = grafico , titulo = titulo ,
       nota = nota , alto = alto)
}


##==: 3. correr el proyecto de principio a fin :==##

correr_proyecto <- function(){
   inicio <- Sys.time()

   ## A.1. ir a la carpeta del proyecto: la de este archivo, se lance desde donde se lance
   ## (Rscript la da en --file=; source() y el boton Source de RStudio, en ofile)
   carpeta_de_este_archivo <- function(){
                              args <- commandArgs(trailingOnly = FALSE)
                              archivo <- sub("^--file=" , "" , args[grep("^--file=" , args)])
                              if (length(archivo)==1) return(dirname(normalizePath(archivo)))
                              for (f in rev(sys.frames())) if (!is.null(f$ofile)) return(dirname(normalizePath(f$ofile)))
                              if (requireNamespace("rstudioapi" , quietly=T) && rstudioapi::isAvailable()){
                                 ruta <- rstudioapi::getSourceEditorContext()$path
                                 if (nzchar(ruta)) return(dirname(normalizePath(ruta)))
                              }
                              normalizePath(getwd())
   }
   raiz <- carpeta_de_este_archivo()
   if (!file.exists(file.path(raiz , "01_data"))){
      stop("No encuentro la carpeta 01_data junto a 00_run.R (" , raiz , "): 00_run.R debe estar en la carpeta del proyecto.")
   }
   setwd(raiz)
   cat("Carpeta del proyecto:" , raiz , "\n")

   ## A.2. lo que necesita el informe, ademas de los paquetes de la seccion 0:
   ## los paquetes de R (se instalan si faltan), pandoc y LaTeX
   pacman::p_load(tinytex)
   pacman::p_load(rmarkdown , knitr , kableExtra , ggplot2 , patchwork , scales)
   if (!rmarkdown::pandoc_available()){
      stop("Falta pandoc para armar el PDF: corran 00_run.R desde RStudio (lo trae incluido) o instalen pandoc.")
   }
   if (Sys.which("pdflatex")==""){
      cat("No hay LaTeX en este computador: se instala TinyTeX (una sola vez, toma unos minutos)\n")
      pacman::p_load(tinytex)
      tinytex::install_tinytex()
   }

   ## A.3. los paquetes de LaTeX que usa el informe (tablas, recuadros, titulos).
   ## Con TinyTeX, si falta alguno se instala desde un espejo fijo de CTAN: el espejo
   ## automatico a veces entrega archivos danados y el PDF no compila. Con MiKTeX o
   ## TeX Live completo no se hace nada (ellos instalan lo que falte por su cuenta).
   ## La letra Palatino (newpx) es opcional: si no esta, el informe usa la letra por
   ## defecto de LaTeX en vez de detenerse
   instalado <- function(sty){
                ruta <- tryCatch(suppressWarnings(system2("kpsewhich" , paste0(sty , ".sty") , stdout = T , stderr = F)) ,
                                 error = function(e) character(0))
                length(ruta) > 0 && nzchar(ruta[1])
   }
   if (tinytex::is_tinytex()){
      latex <- c(titlesec = "titlesec" , caption = "caption" , float = "float" , fancyhdr = "fancyhdr" ,
                 tcolorbox = "tcolorbox" , environ = "environ" , trimspaces = "trimspaces" , tikzfill = "tikzfill" ,
                 pdfcol = "pdfcol" , listingsutf8 = "listingsutf8" , booktabs = "booktabs" , multirow = "multirow" ,
                 wrapfig = "wrapfig" , colortbl = "colortbl" , pdflscape = "pdflscape" , tabu = "tabu" ,
                 threeparttable = "threeparttable" , threeparttablex = "threeparttablex" , ulem = "ulem" ,
                 makecell = "makecell" , newpxtext = "newpx")
      faltan <- unique(latex[!sapply(names(latex) , instalado)])
      if (length(faltan) > 0){
         cat("Faltan paquetes de LaTeX para el informe:" , paste(faltan , collapse = ", ") , "\n")
         espejos <- c("https://mirrors.mit.edu/CTAN/systems/texlive/tlnet" ,
                      "https://ctan.math.illinois.edu/systems/texlive/tlnet" ,
                      "https://ftp.math.utah.edu/pub/ctan/tex-archive/systems/texlive/tlnet")
         for (espejo in espejos){
              cat("   instalando desde" , espejo , "\n")
              try(tinytex::tlmgr_repo(espejo) , silent = T)
              try(tinytex::tlmgr_install(faltan) , silent = T)
              faltan <- unique(latex[!sapply(names(latex) , instalado)])
              if (length(faltan)==0) break
         }
         if (length(faltan) > 0){
            cat("   no se pudieron instalar:" , paste(faltan , collapse = ", ") ,
                "\n   si el PDF no compila, actualicen LaTeX una vez con tinytex::reinstall_tinytex()\n")
         }
      }
   }

   ## A.4. los registros de cada etapa
   logs <- "06_report/output/logs"
   dir.create(logs , showWarnings=F , recursive=T)
   rscript <- file.path(R.home("bin") , "Rscript")

   ## ---- B. las etapas de analisis, en orden ----

   ## cada etapa: el script que la corre, las salidas que debe dejar, los insumos de
   ## los que depende (si cambian, se vuelve a correr), las entradas que se copian a
   ## su carpeta input/ antes de correrla (nombre = destino, valor = origen) y si es obligatoria. Una
   ## etapa no obligatoria cuyo script aun no exporta queda PENDIENTE: el flujo sigue
   ## y el informe la marca. Cuando una etapa quede lista, poner obligatoria = TRUE.
   ## fechas = FALSE: basta con que las salidas existan
   etapa <- function(nombre , script , salidas , insumos = character(0) , obligatoria = TRUE , fechas = TRUE ,
                     entradas = character(0)){
            list(nombre = nombre , script = script , salidas = salidas ,
                 insumos = insumos , obligatoria = obligatoria , fechas = fechas , entradas = entradas)
   }

   datos_crudos <- c("01_data/input/vivienda_cali.rds" , "01_data/input/diccionario_vivienda.csv")
   base <- "01_data/output/vivienda_cali.csv"
   base_limpia <- "02_features/output/vivienda_cali_variables.csv"
   archivo_predicciones <- "05_final_model/output/predicciones_precio.csv"
   externos <- list.files("02_features/input/externos" , recursive=T , full.names=T)

   etapas <- list(
     etapa("Preparacion de la base" , "01_data/source/source.R" , base ,
           insumos = c("00_run.R" , datos_crudos)),
     etapa("Variables de ubicacion (barrio, manzana, estrato y distancias)" , "02_features/source/source.R" ,
           base_limpia , insumos = c("00_run.R" , base , externos) ,
           entradas = c("02_features/input/vivienda_cali.csv" = base)),
     etapa("Parte 1: referencia y regresion lineal" , "03_baselines/source/source.R" ,
           "03_baselines/output/results_part_1.rds" , insumos = c("00_run.R" , base) ,
           entradas = c("03_baselines/input/vivienda_cali.csv" = base)),
     etapa("Parte 2: validacion y regularizacion" , "04_validation/source/source.R" ,
           "04_validation/output/results_part_2.rds" ,
           insumos = c("00_run.R" , base_limpia , "03_baselines/output/results_part_1.rds") ,
           entradas = c("04_validation/input/vivienda_cali_variables.csv" = base_limpia ,
                        "04_validation/input/results_part_1.rds" = "03_baselines/output/results_part_1.rds")),
     etapa("Parte 3: modelo final y predicciones" , "05_final_model/source/source.R" ,
           c("05_final_model/output/results_part_3.rds" , archivo_predicciones) ,
           insumos = c("00_run.R" , base_limpia , "04_validation/output/results_part_2.rds") ,
           entradas = c("05_final_model/input/vivienda_cali_variables.csv" = base_limpia ,
                        "05_final_model/input/results_part_2.rds" = "04_validation/output/results_part_2.rds"))
   )

   ## ---- C. correr una etapa y validar que termino bien ----

   ## al dia: todas sus salidas existen y son mas nuevas que su script y sus insumos
   al_dia <- function(e){
             if (!all(file.exists(e$salidas))) return(FALSE)
             if (!e$fechas) return(TRUE)
             min(file.mtime(e$salidas)) >= max(file.mtime(c(e$script , e$insumos)) , na.rm=T)
   }

   correr <- function(e , i){
             cat(sprintf("\n[%d/%d] %s\n" , i , length(etapas) , e$nombre))

             ## el script no existe: error si es obligatoria; pendiente si no
             if (!file.exists(e$script)){
                if (e$obligatoria) stop("No existe el script de una etapa obligatoria: " , e$script)
                cat("      PENDIENTE: el script aun no existe.\n")
                return("pendiente")
             }

             ## al dia: se reutilizan sus resultados
             if (!forzar && al_dia(e)){
                cat("      al dia: se reutilizan sus resultados\n")
                return("al dia")
             }

             ## copiar a la carpeta input/ de la etapa lo que lee de las etapas anteriores:
             ## cada etapa lee solo de su input/ y escribe solo en su output/
             for (destino in names(e$entradas)){
                  dir.create(dirname(destino) , showWarnings=F , recursive=T)
                  file.copy(e$entradas[[destino]] , destino , overwrite=T)
             }

             ## correrlo en una sesion nueva; todo lo que imprime va al registro
             ## un registro por script, p. ej. logs/03_baselines_source.log
             registro <- file.path(logs , paste0(sub("\\.R$" , "" , gsub("/" , "_" , e$script)) , ".log"))
             t0 <- Sys.time()
             estado <- system2(rscript , shQuote(e$script) , stdout = registro , stderr = registro)

             ## validacion 1: el script termino sin error
             if (estado != 0){
                cat("      ERROR (codigo " , estado , "). Final del registro " , registro , ":\n\n" , sep = "")
                cat(paste0("      | " , tail(readLines(registro , warn = F) , 25)) , sep = "\n")
                stop("La etapa '" , e$nombre , "' fallo. Revise " , registro)
             }

             ## validacion 2: dejo sus salidas, escritas en esta corrida
             faltan <- e$salidas[!file.exists(e$salidas)]
             if (e$fechas) faltan <- e$salidas[!file.exists(e$salidas) | file.mtime(e$salidas) < t0]
             if (length(faltan) > 0){
                if (e$obligatoria) stop("La etapa '" , e$nombre , "' no dejo: " , paste(faltan , collapse = ", "))
                cat("      PENDIENTE: el script corrio pero aun no exporta" , paste(faltan , collapse = ", ") , "\n")
                return("pendiente")
             }

             cat(sprintf("      OK en %.0f s (registro: %s)\n" , as.numeric(difftime(Sys.time() , t0 , units = "secs")) , registro))
             return("ok")
   }

   ## ---- D. correr los analisis ----

   cat("== Precio de la vivienda en Cali ==\n")
   estados <- rep(NA , length(etapas))
   for (i in 1:length(etapas)){
        estados[i] <- correr(etapas[[i]] , i)
   }

   ## validar predicciones_precio.csv: una fila por aviso de prueba, las dos columnas, ninguna NA
   if (file.exists(archivo_predicciones)){
      pred <- read.csv(archivo_predicciones)
      db <- read.csv(base)
      test_ids <- db$id[db$muestra=="prueba"]
      problemas <- c(columnas = !identical(names(pred) , c("id" , "precio_predicho")) ,
                     filas = nrow(pred)!=length(test_ids) ,
                     ids = !setequal(pred$id , test_ids) | anyDuplicated(pred$id) > 0 ,
                     na = anyNA(pred) ,
                     precio_no_positivo = any(pred$precio_predicho <= 0 , na.rm = T))
      if (any(problemas)) stop("predicciones_precio.csv no cumple: " , paste(names(problemas)[problemas] , collapse = ", "))
      cat(sprintf("\npredicciones_precio.csv OK: %d filas, una por aviso de prueba, sin NA\n" , nrow(pred)))
   }

   ## ---- E. el informe en pdf ----

   ## el informe lee los resultados de cada parte desde su propia carpeta input/
   resultados_informe <- c("03_baselines/output/results_part_1.rds" , "04_validation/output/results_part_2.rds" , "05_final_model/output/results_part_3.rds")
   dir.create("06_report/input" , showWarnings=F , recursive=T)
   file.copy(resultados_informe , file.path("06_report/input" , basename(resultados_informe)) , overwrite=T)

   cat("\n[Informe] 06_report/source/informe_general.Rmd\n")
   registro <- file.path(logs , "informe_general.log")
   estado <- system2(rscript , c("-e" , shQuote("source('06_report/source/formato_informe.R'); generar_pdf()")) ,
                     stdout = registro , stderr = registro)
   if (estado != 0){
      cat(paste0("      | " , tail(readLines(registro , warn = F) , 25)) , sep = "\n")
      stop("No se pudo generar el PDF. Revise " , registro)
   }
   cat("      OK:" , file.path(raiz , "06_report/output/informe_general.pdf") , "\n")
   if (file.exists(archivo_predicciones)) cat("      predicciones_precio.csv:" , file.path(raiz , archivo_predicciones) , "\n")

   ## ---- F. resumen y sessionInfo() ----

   cat("\n== Resumen ==\n")
   print(data.frame(etapa = sapply(etapas , function(e) e$nombre) , estado = estados) , row.names = F)
   cat(sprintf("\nTiempo total: %.1f minutos\n\n" , as.numeric(difftime(Sys.time() , inicio , units = "mins"))))

   ## la sesion que produjo los resultados tambien en logs/
   sesion <- sessionInfo()
   writeLines(capture.output(sesion) , file.path(logs , "sessionInfo.txt"))
   print(sesion)
}

## los scripts de las carpetas cargan este archivo con solo_entorno <- TRUE: reciben
## los paquetes y las funciones de las secciones 0 a 2 y aqui se detiene. Lanzado
## directamente (Source o Rscript), corre el proyecto completo
if (exists("solo_entorno")){
   rm(solo_entorno)
} else {
   correr_proyecto()
}
