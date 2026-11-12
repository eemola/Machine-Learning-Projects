## Big Data y Machine Learning para Economia Aplicada
## Clasificacion de vivienda de interes social (VIS) en Cali — formato del informe (lo cargan los .Rmd de esta carpeta)
## Autor: Elkin Mola
## No hace falta editarlo. Da cinco funciones para trabajar en los .Rmd:
##   resultados(k)        lee los resultados que guardo el script de la parte k
##   ver(p1)              lista las tablas y figuras disponibles y sus nombres
##   mostrar(p1 , "x")    muestra la tabla o figura "x" (en RStudio y en el PDF)
##   fmt(x , d)           escribe un numero con coma decimal: fmt(1234.5 , 1) -> 1.234,5
##   generar_pdf()        arma 05_report/output/informe_general.pdf (lo usa 00_run.R)

##==: 0. paquetes y rutas :==##

suppressPackageStartupMessages({
  library(knitr) ; library(kableExtra) ; library(ggplot2) ; library(patchwork) ; library(scales)
})

## la raiz del proyecto (la carpeta de 00_run.R, o la del
## .Rproj), desde donde se haya cargado este archivo; las rutas de abajo son
## relativas a ella
es_raiz <- function(d) file.exists(file.path(d , "vis-classification.Rproj")) ||
                       file.exists(file.path(d , "00_run.R"))
buscar_raiz <- function(desde){
               d <- normalizePath(desde , mustWork = F)
               while (!es_raiz(d)){
                     if (dirname(d)==d) return(NA)
                     d <- dirname(d)
               }
               d
}
archivo_actual <- Filter(Negate(is.null) , lapply(sys.frames() , function(f) f$ofile))
raiz <- buscar_raiz(if (length(archivo_actual) > 0) dirname(tail(archivo_actual , 1)[[1]]) else getwd())
if (is.na(raiz)) raiz <- buscar_raiz(getwd())
if (is.na(raiz)) stop("No encuentro la carpeta del proyecto (la de 00_run.R): abran el proyecto en RStudio.")
dir_informe <- file.path(raiz , "05_report" , "source")
dir_pdf <- file.path(raiz , "05_report" , "output")

## donde guarda sus resultados el script de cada parte
archivos_resultados <- c("05_report/input/results_part_1.rds",
                         "05_report/input/results_part_2.rds")

##==: 1. leer y consultar resultados :==##

resultados <- function(k){
              archivo <- archivos_resultados[k]
              ruta <- file.path(raiz , archivo)
              if (!file.exists(ruta)) return(structure(list(.parte = k) , pendiente = archivo))
              r <- readRDS(ruta)
              r$.parte <- k
              r
}

## aviso en gris mientras el script de la parte no guarde sus resultados
estado <- function(r){
          if (is.null(attr(r , "pendiente"))) return(invisible())
          pendiente(tex(paste0("Pendiente: el script de esta parte aún no guarda " , attr(r , "pendiente") , ".")))
}

ver <- function(r){
       if (!is.null(attr(r , "pendiente"))) return(cat("Pendiente: aun no existe" , attr(r , "pendiente") , "\n"))
       el <- r$elementos
       print(data.frame(nombre = names(el) , tipo = sapply(el , function(e) e$tipo) ,
                        titulo = substr(sapply(el , function(e) e$titulo) , 1 , 70) , row.names = NULL))
       otros <- setdiff(names(r) , c("elementos" , ".parte"))
       cat("\nOtras cifras (p. ej. p" , r$.parte , "$" , otros[1] , "): " , paste(otros , collapse = ", ") , "\n" , sep = "")
}

##==: 2. numeros con coma decimal :==##

fmt <- function(x , d = 0){
       out <- formatC(x , format = "f" , digits = d , big.mark = "." , decimal.mark = ",")
       out[is.na(x)] <- "—"
       out
}

## una columna de enteros (conteos, k) va sin decimales; en una columna mixta
## solo los enteros desde 100 los pierden (15.277 junto a 391,0)
formato_numeros <- function(df , decimales){
                   df[] <- lapply(df , function(x){
                           if (!is.numeric(x)) return(x)
                           entero <- !is.na(x) & abs(x - round(x)) < 1e-9
                           if (all(entero | is.na(x))) return(fmt(x , 0))
                           ifelse(entero & abs(x) >= 100 , fmt(x , 0) , fmt(x , decimales))
                   })
                   df
}

## texto que va dentro de LaTeX (titulos y notas): escapar lo que lo romperia
tex <- function(x) gsub("([%&_#$])" , "\\\\\\1" , x)

##==: 3. mostrar una tabla o una figura :==##

tabla_latex <- function(e){
               alineacion <- ifelse(sapply(e$datos , is.numeric) , "r" , "l")
               datos <- formato_numeros(as.data.frame(e$datos) , e$decimales)
               ## tablas anchas (muchas columnas o textos largos): se reducen al ancho de
               ## la pagina para que no se corten; las demas quedan igual
               ancho <- sum(mapply(function(x , n) max(nchar(c(as.character(x) , n)) , na.rm = T) , datos , e$columnas)) + 2*ncol(datos)
               opciones <- if (ancho > 95) c("HOLD_position" , "scale_down") else "HOLD_position"
               out <- kbl(datos , format = "latex" ,
                          booktabs = T , caption = tex(e$titulo) , col.names = e$columnas ,
                          align = alineacion , linesep = "" , row.names = F) %>%
                      kable_styling(latex_options = opciones , font_size = 9.5 , position = "center")
               if (!is.null(e$encabezado)) out <- add_header_above(out , e$encabezado)
               if (!is.null(e$grupos)){
                  fin <- cumsum(e$grupos)
                  for (g in seq_along(e$grupos)) out <- pack_rows(out , names(e$grupos)[g] , fin[g] - e$grupos[g] + 1 , fin[g])
               }
               if (!is.null(e$nota)){
                  out <- footnote(out , general = e$nota , general_title = "Nota:" ,
                                  footnote_as_chunk = T , threeparttable = T)
               }
               out
}

grafico <- function(e){
           if (inherits(e$grafico , "ggplot")) return(e$grafico)
           wrap_plots(e$grafico) + plot_annotation(tag_levels = "A")
}

pie_figura <- function(e) tex(paste(c(e$titulo , e$nota) , collapse = " "))

pendiente <- function(texto) asis_output(paste0("\n\n\\textit{\\color{gray}" , texto , "}\n\n"))

## en RStudio devuelve la tabla (con coma decimal) o el grafico para verlos al
## correr el chunk; al armar el PDF escribe la tabla o la figura con su numero,
## su titulo y su etiqueta para citarla: \ref{tab:p1-descriptivas}, \ref{fig:p1-mapa_precios}
mostrar <- function(r , nombre){
           if (!is.null(attr(r , "pendiente"))){
              aviso <- paste0("Pendiente: aun no existe " , attr(r , "pendiente"))
              if (isTRUE(getOption("knitr.in.progress"))) return(pendiente(tex(aviso)))
              return(message(aviso))
           }
           e <- r$elementos[[nombre]]
           if (is.null(e)) stop("No hay un elemento '" , nombre , "'. Disponibles: " , paste(names(r$elementos) , collapse = ", "))
           if (!isTRUE(getOption("knitr.in.progress"))){
              if (e$tipo=="tabla"){
                 tabla <- formato_numeros(as.data.frame(e$datos) , e$decimales)
                 names(tabla) <- make.unique(e$columnas)
                 rownames(tabla) <- NULL
                 return(tabla)
              }
              return(grafico(e))
           }
           etiqueta <- paste0("p" , r$.parte , "-" , nombre)
           if (e$tipo=="tabla"){
              chunk <- sprintf("```{r %s}\ntabla_latex(.e)\n```" , etiqueta)
           } else {
              chunk <- sprintf("```{r %s, fig.height = %s, fig.cap = pie_figura(.e)}\ngrafico(.e)\n```" , etiqueta , e$alto)
           }
           entorno <- new.env(parent = knit_global())
           entorno$.e <- e
           asis_output(knit_child(text = chunk , envir = entorno , quiet = T))
}

##==: 5. el tema de todas las figuras :==##

## sin cairo (p. ej. un Mac sin XQuartz) las figuras salen con el dispositivo pdf,
## que no conoce "Palatino Linotype" pero si "Palatino"
fuente <- if (capabilities("cairo")) "Palatino Linotype" else "Palatino"
theme_set(theme_minimal(base_size = 9 , base_family = fuente) +
          theme(panel.grid.minor = element_blank() ,
                panel.grid.major = element_line(color = "grey90" , linewidth = 0.3) ,
                axis.title = element_text(color = "grey15") ,
                axis.text = element_text(color = "grey35") ,
                legend.title = element_text(size = 8 , color = "grey15") ,
                legend.text = element_text(size = 7.5 , color = "grey25") ,
                legend.key.size = unit(0.4 , "cm") ,
                plot.tag = element_text(face = "bold" , size = 10 , color = "#1F497D") ,
                plot.margin = margin(4 , 6 , 4 , 4)))
update_geom_defaults("text" , list(family = fuente))

##==: 6. el informe general :==##

## una seccion dentro del informe: el .Rmd sin su encabezado YAML
incluir <- function(archivo){
           lineas <- readLines(file.path(dir_informe , archivo) , encoding = "UTF-8" , warn = F)
           if (length(lineas) > 0 && lineas[1]=="---") lineas <- lineas[-(1:which(lineas=="---")[2])]
           asis_output(knit_child(text = lineas , envir = knit_global() , quiet = T))
}

## titulo del informe
titulo <- function(){
          meses <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto",
                     "septiembre","octubre","noviembre","diciembre")
          hoy <- Sys.Date()
          fecha <- paste(as.integer(format(hoy , "%d")) , "de" , meses[as.integer(format(hoy , "%m"))] , "de" , format(hoy , "%Y"))
          asis_output(paste0("```{=latex}\n\\thispagestyle{plain}\n\\begin{center}\n" ,
                 "{\\color{azul}\\LARGE\\bfseries ¿Qué avisos son vivienda de interés social (VIS) en Cali?\\par}\n\\vspace{0.5em}\n" ,
                 "{\\large Proyecto de machine learning: clasificación de una clase rara\\par}\n\\vspace{0.25em}\n" ,
                 "Universidad Icesi --- Maestría en Economía \\textperiodcentered{} Big Data y Machine Learning para Economía Aplicada\\par\n\\vspace{0.9em}\n" ,
                 "{\\large\\bfseries Elkin Mola\\par}\n\\vspace{0.2em}\n" ,
                 "{\\small\\color{gray}" , fecha , "\\par}\n\\end{center}\n" ,
                 "\\vspace{-0.4em}{\\color{azul}\\rule{\\textwidth}{0.6pt}}\n\\vspace{-1.2em}\n```\n"))
}

## arma el PDF con informe_general.Rmd y lo deja en 05_report/output/.
## solo = "01_classification.Rmd" arma una vista previa con esa seccion (boton Knit).
## Se arma junto al .Rmd y luego se mueve: rmarkdown con output_dir convierte la
## ruta de las figuras en una ruta corta de Windows con "\" que LaTeX no entiende
generar_pdf <- function(solo = ""){
               nombre <- if (solo=="") "informe_general.pdf" else paste0("vista_previa_" , sub("\\.Rmd$" , ".pdf" , solo))
               pdf <- rmarkdown::render(file.path(dir_informe , "informe_general.Rmd") , output_file = nombre ,
                                        params = list(solo = solo) , envir = new.env() , quiet = T)
               dir.create(dir_pdf , showWarnings = F)
               destino <- file.path(dir_pdf , nombre)
               file.copy(pdf , destino , overwrite = T)
               file.remove(pdf)
               base_nombre <- sub("\\.pdf$" , "" , nombre)
               unlink(file.path(dir_informe , paste0(base_nombre , "_files")) , recursive = T , force = T)
               unlink(file.path(dir_informe , paste0(base_nombre , c(".log" , ".tex"))) , force = T)
               message("PDF listo: " , destino)
               invisible(destino)
}
