##############################################################################
##  Big Data y Machine Learning para Economia Aplicada                      ##
##  Precio de la vivienda en Cali — Parte 1: la referencia y la regresion lineal ##
##  Autor: Elkin Mola                                                       ##
##############################################################################


##============================================================================##
##  0. CONFIGURACION INICIAL                                                  ##
##============================================================================##

## paquetes y funciones: rmse(), mae(), r2() (ver 00_run.R)
solo_entorno <- TRUE ; source("00_run.R")

## colores de las figuras (el azul de los titulos del reporte)
azul <- "#1F497D"
naranja <- "#D9822B"
rojo <- "#B2182B"


##============================================================================##
##  1. PREPARAR LOS DATOS                                                     ##
##============================================================================##

## ---- 1.1. cargar los avisos preparados en 01_data --------------------------
db <- import("03_baselines/input/vivienda_cali.csv")

## ---- 1.2. separar entrenamiento y prueba ----------------------------------
## la particion fija viene en la variable muestra
## (en la prueba el precio esta oculto: no se mira)
train <- db %>% subset(muestra=="entrenamiento")
test <- db %>% subset(muestra=="prueba")
c(train = nrow(train) , test = nrow(test))


##============================================================================##
##  2. DESCRIBIR EL TARGET ANTES DE MODELAR                                   ##
##============================================================================##

## ---- 2.1. el precio en millones: media, desviacion y rango ----------------
price_stats <- train %>%
               summarise(mean_price = mean(precio) , sd_price = sd(precio) ,
                         median_price = median(precio) , min_price = min(precio) ,
                         max_price = max(precio))
price_stats

## ---- 2.2. composicion por tipo de inmueble --------------------------------
type_composition <- train %>%
                    count(tipo) %>%
                    mutate(percentage = n/sum(n)*100)
type_composition

## ---- 2.3. faltantes por variable en las dos muestras ----------------------
## un texto vacio ("") tambien es faltante (barrio viene vacio, no NA);
## precio se deja por fuera porque en la prueba esta oculto; vis tambien, porque se calcula a partir del precio
faltante <- function(x) sum(is.na(x) | (is.character(x) & x==""))
variables <- setdiff(names(db) , c("precio","vis","muestra"))
missing_values <- data.frame(variable = variables ,
                             na_train = sapply(train[,variables] , faltante) ,
                             na_test = sapply(test[,variables] , faltante)) %>%
                  mutate(pct_train = na_train/nrow(train)*100 ,
                         pct_test = na_test/nrow(test)*100) %>%
                  subset(na_train > 0 | na_test > 0) %>%
                  arrange(desc(na_train))
missing_values

## ---- 2.4. figura: el mapa de precios --------------------------------------
## lon contra lat, coloreado por el precio. el color va en escala logaritmica
## porque el precio tiene cola larga a la derecha; los mas caros se dibujan
## encima (en clase: plot(train$lon , train$lat , col=...))
plot_spatial <- ggplot(train %>% arrange(precio) , aes(x=lon , y=lat , color=precio)) +
                geom_point(alpha=0.8 , size=0.45 , stroke=0) +
                scale_color_viridis_c(option="magma" , end=0.92 , direction=-1 , trans="log10" ,
                                      breaks=c(100,200,500,1000,2000) ,
                                      labels=label_number(big.mark="." , decimal.mark=",") ,
                                      name="Precio\n(millones COP)") +
                coord_quickmap() +
                labs(x="Longitud" , y="Latitud")

## ---- 2.5. figura: la distribucion del precio por tipo ---------------------
## (en clase: hist(train$precio))
plot_price_hist <- ggplot(train , aes(x=precio , fill=tipo)) +
                   geom_histogram(bins=45 , position="identity" , alpha=0.65 , color=NA) +
                   scale_x_log10(breaks=c(50,100,200,500,1000,2000) ,
                                 labels=label_number(big.mark="." , decimal.mark=",")) +
                   scale_y_continuous(labels=label_number(big.mark="." , decimal.mark=",") ,
                                      expand=expansion(mult=c(0,0.05))) +
                   scale_fill_manual(values=c(Apartamento=azul , Casa=naranja) , name=NULL) +
                   labs(x="Precio (millones COP, escala logarítmica)" , y="Número de avisos") +
                   theme(legend.position="inside" , legend.position.inside=c(0.82,0.88))


##============================================================================##
##  3. LA REFERENCIA Y LA REGRESION                                           ##
##============================================================================##

## ---- 3.1. la validacion propia: 80 % ajuste, 20 % validacion --------------
## semilla fija; la prueba sigue bajo llave (regla de oro #2)
set.seed(12345)
ajuste <- sample(1:nrow(train) , size=0.8*nrow(train))
fit <- train[ajuste,]
val <- train[-ajuste,]
c(ajuste = nrow(fit) , validacion = nrow(val))

## ---- 3.2. M0, la referencia: la media del ajuste para todos ---------------
media_ajuste <- mean(fit$precio)
pred_m0 <- rep(media_ajuste , nrow(val))

## ---- 3.3. M1, la recta con el area ----------------------------------------
m1 <- lm(precio ~ area , data=fit)
pred_m1 <- predict(m1 , val)

## ---- 3.4. M2: area, banos, tipo y coordenadas -----------------------------
m2 <- lm(precio ~ area + banos + tipo + lon + lat , data=fit)
pred_m2 <- predict(m2 , val)

## ---- 3.5. primeras filas de la tabla de modelos: rmse, mae y r2 de validacion -----------
## (en millones)
annex <- tabla_anexo(list("M0: media del entrenamiento"         = pred_m0 ,
                          "M1: recta con el área"               = pred_m1 ,
                          "M2: área, baños, tipo y coordenadas" = pred_m2) ,
                     y = val$precio)
annex


##============================================================================##
##  4. DESPUES DE PREDECIR: QUE PREDICE M2                                    ##
##============================================================================##

## ---- 4.1. predicho, residuo y quintil de cada aviso de validacion ---------
val <- val %>%
       mutate(pred_m2 = pred_m2 ,
              residuo = precio - pred_m2 ,
              quintil = ntile(precio , 5))

## ---- 4.2. predicho vs. observado ------------------------------------------
## la pendiente de la recta entre los dos
recta_po <- coef(lm(pred_m2 ~ precio , data=val))
slope_pred_obs <- unname(recta_po[2])
slope_pred_obs

## figura: la que nunca falta (en clase: plot(observado , predicho) + abline(a=0 , b=1))
lim <- range(c(val$precio , val$pred_m2))
plot_pred_obs <- ggplot(val , aes(x=precio , y=pred_m2)) +
                 geom_point(alpha=0.3 , size=0.8 , stroke=0 , color=azul) +
                 geom_abline(intercept=0 , slope=1 , linetype="dashed" , color=rojo , linewidth=0.6) +
                 geom_smooth(method="lm" , formula=y ~ x , se=F , color=naranja , linewidth=0.8) +
                 ## leyenda a mano en la esquina vacia (arriba a la izquierda)
                 annotate("text" , x=lim[1] + 0.03*diff(lim) , y=lim[2] ,
                          label="- - 45°: predicción perfecta" ,
                          hjust=0 , size=2.8 , color=rojo) +
                 annotate("text" , x=lim[1] + 0.03*diff(lim) , y=lim[2] - 0.07*diff(lim) ,
                          label=paste0("— recta ajustada: pendiente ",
                                       number(slope_pred_obs , accuracy=0.01 , decimal.mark=",")) ,
                          hjust=0 , size=2.8 , color=naranja) +
                 scale_x_continuous(labels=label_number(big.mark="." , decimal.mark=",")) +
                 scale_y_continuous(labels=label_number(big.mark="." , decimal.mark=",")) +
                 coord_equal(xlim=lim , ylim=lim) +
                 labs(x="Precio observado (millones COP)" , y="Precio predicho por M2 (millones COP)")

## ---- 4.3. el error por quintil del precio observado -----------------------
## con el precio medio observado y predicho de cada quintil
quintile_diagnostics <- val %>%
                        group_by(quintil) %>%
                        summarise(n = n() , min_price = min(precio) , max_price = max(precio) ,
                                  mean_price = mean(precio) , mean_pred = mean(pred_m2) ,
                                  quintile_rmse = rmse(precio , pred_m2) ,
                                  quintile_mae = mae(precio , pred_m2)) %>%
                        rename(price_quintile = quintil)
quintile_diagnostics

## figura: rmse por quintil (en clase: barplot(rmse por quintil))
plot_quintiles <- quintile_diagnostics %>%
                  mutate(etiqueta = paste0("Q" , price_quintile , "\n(" ,
                                           number(mean_price , accuracy=1 , big.mark="." , decimal.mark=",") , ")")) %>%
                  ggplot(aes(x=etiqueta , y=quintile_rmse)) +
                  geom_col(fill=azul , width=0.65) +
                  geom_text(aes(label=number(quintile_rmse , accuracy=1 , big.mark="." , decimal.mark=",")) ,
                            vjust=-0.5 , size=2.8 , color="grey20") +
                  scale_y_continuous(labels=label_number(big.mark="." , decimal.mark=",") ,
                                     expand=expansion(mult=c(0,0.12))) +
                  labs(x="Quintil del precio observado (precio medio)" , y="RMSE de M2 (millones COP)")

## ---- 4.4. los residuos en el mapa -----------------------------------------
## el color es el signo y el tamano, el error (como el mapa de residuos de la
## sesion 2)
val <- val %>% mutate(signo = ifelse(residuo > 0 , "Vale más de lo predicho" , "Vale menos de lo predicho"))
plot_errors <- ggplot(val %>% arrange(abs(residuo)) , aes(x=lon , y=lat , color=signo , size=abs(residuo))) +
               geom_point(alpha=0.55 , stroke=0) +
               scale_color_manual(values=c("Vale más de lo predicho"=rojo , "Vale menos de lo predicho"=azul) ,
                                  name=NULL) +
               scale_size_area(max_size=3.5 , breaks=c(250,500,1000) ,
                               labels=label_number(big.mark="." , decimal.mark=",") ,
                               name="Error absoluto\n(millones COP)") +
               coord_quickmap() +
               guides(color=guide_legend(override.aes=list(size=2.5 , alpha=1))) +
               labs(x="Longitud" , y="Latitud")

## ---- 4.5. el error por zona -----------------------------------------------
## una grilla de 0,01 grados (cerca de 1,1 km) y el error absoluto medio de
## cada celda con al menos cinco avisos de validacion
celda <- 0.01
error_grid <- val %>%
              mutate(cell_lon = (floor(lon/celda) + 0.5)*celda ,
                     cell_lat = (floor(lat/celda) + 0.5)*celda) %>%
              group_by(cell_lon , cell_lat) %>%
              summarise(n = n() , mae = mean(abs(residuo)) , .groups="drop") %>%
              subset(n >= 5)

plot_error_zones <- ggplot() +
                    geom_point(data=val , aes(x=lon , y=lat) , color="grey80" , size=0.3 , stroke=0) +
                    geom_tile(data=error_grid , aes(x=cell_lon , y=cell_lat , fill=mae) ,
                              width=celda , height=celda , alpha=0.9) +
                    scale_fill_viridis_c(option="rocket" , direction=-1 , end=0.95 ,
                                         labels=label_number(big.mark="." , decimal.mark=",") ,
                                         name="Error absoluto\nmedio (M COP)") +
                    coord_quickmap() +
                    labs(x="Longitud" , y="Latitud")


##============================================================================##
##  5. CUANTO REDUCE EL ERROR CADA MODELO                                     ##
##============================================================================##

## ---- 5.1. reduccion del rmse: frente a M0 y de M2 frente a M1, en % -------
rmse_m0 <- annex$rmse[1]
rmse_m1 <- annex$rmse[2]
rmse_m2 <- annex$rmse[3]
reduccion <- c(m1_vs_m0 = (1 - rmse_m1/rmse_m0)*100 ,
               m2_vs_m0 = (1 - rmse_m2/rmse_m0)*100 ,
               m2_vs_m1 = (1 - rmse_m2/rmse_m1)*100)
round(reduccion , 1)


##============================================================================##
##  6. LO QUE SALE EN EL REPORTE (TABLAS Y FIGURAS)                           ##
##============================================================================##
## cada tabla o figura lleva un nombre: en 06_report/source/01_baselines.Rmd
## se muestran con mostrar(p1 , "nombre") (funciones de 00_run.R,
## seccion 2). Se arma una lista por tema y al final se juntan.

entero <- function(x) formatC(round(x) , format="d" , big.mark=".")

## ---- 6.1. descriptivas, faltantes y mapa de precios ----------
descriptivas <- data.frame(indicador = c("Avisos de entrenamiento" , "Avisos de prueba" ,
                                         "Media" , "Desviación estándar" , "Mediana" , "Mínimo" , "Máximo" ,
                                         type_composition$tipo) ,
                           valor = c(nrow(train) , nrow(test) ,
                                     price_stats$mean_price , price_stats$sd_price , price_stats$median_price ,
                                     price_stats$min_price , price_stats$max_price ,
                                     type_composition$percentage))
faltantes <- missing_values %>% select(variable , na_train , pct_train , na_test , pct_test)

elementos_1a <- list(
  descriptivas = tabla_reporte(descriptivas , "Descripción de la muestra" ,
                columnas = c("" , "Valor") , decimales = 1 ,
                grupos = c("Muestra" = 2 , "Precio en el entrenamiento (millones COP)" = 5 ,
                           "Tipo de inmueble (% del entrenamiento)" = nrow(type_composition)) ,
                nota = "Precios en millones de pesos corrientes."),
  faltantes = tabla_reporte(faltantes , "Valores faltantes por variable y muestra" ,
                columnas = c("Variable" , "Avisos" , "%" , "Avisos" , "%") , decimales = 1 ,
                encabezado = c(" " = 1 , "Entrenamiento" = 2 , "Prueba" = 2) ,
                nota = "Nota: Los valores faltantes de precio en la prueba corresponden a la muestra oculta: ese precio no se usa para ajustar ni para elegir modelos."),
  mapa_precios = figura_reporte(list(plot_spatial , plot_price_hist) , alto = 3.7 ,
                 titulo = "Precios de los avisos de entrenamiento. Panel A: ubicación de cada aviso, con el color según su precio (escala logarítmica). Panel B: distribución del precio por tipo de inmueble (escala logarítmica). Fuente: vivienda_cali.rds.")
)

## ---- 6.2. metricas de M0, M1 y M2 ----------------------------
elementos_1b <- list(
  metricas = tabla_reporte(annex , "Métricas de validación de M0, M1 y M2 (primeras filas de la tabla de modelos)" ,
                columnas = c("Modelo" , "RMSE de validación" , "MAE de validación" , "R² de validación") ,
                nota = paste0("RMSE y MAE en millones de pesos. Ajuste: " , entero(nrow(fit)) ,
                              " avisos; validación: " , entero(nrow(val)) ,
                              " avisos (20 % del entrenamiento, semilla 12345). R² contra la media de la validación (yardstick::rsq_trad_vec)."))
)

## ---- 6.3. diagnosticos, quintiles y mapas de error -----------
elementos_1c <- list(
  diagnosticos = figura_reporte(list(plot_pred_obs , plot_quintiles) , alto = 3.5 ,
                 titulo = "Diagnósticos de M2 en la validación. Panel A: precio predicho contra observado; la línea roja punteada es la predicción perfecta (45 grados) y la naranja, la recta ajustada entre los dos. Panel B: RMSE por quintil del precio observado; entre paréntesis, el precio medio de cada quintil."),
  quintiles = tabla_reporte(quintile_diagnostics %>%
                       mutate(rango = paste(entero(min_price) , "–" , entero(max_price)) ,
                              relativo = quintile_rmse/mean_price*100 ,
                              price_quintile = paste0("Q" , price_quintile)) %>%
                       select(price_quintile , rango , n , mean_price , mean_pred ,
                              quintile_rmse , quintile_mae , relativo) ,
                "Rendimiento de M2 por quintil del precio observado" , decimales = 1 ,
                columnas = c("Quintil" , "Rango de precio" , "Avisos" , "Observado" , "Predicho" ,
                             "RMSE" , "MAE" , "RMSE / precio (%)") ,
                encabezado = c(" " = 3 , "Precio medio" = 2 , "Error" = 3) ,
                nota = "Valores en millones de pesos, sobre los avisos de validación. La última columna divide el RMSE del quintil por su precio observado medio."),
  mapas_error = figura_reporte(list(plot_errors , plot_error_zones) , alto = 3.3 ,
                 titulo = "Errores de M2 en el mapa (avisos de validación). Panel A: residuo de cada aviso (observado menos predicho); el color indica el signo y el tamaño, el error absoluto. Panel B: error absoluto medio por celda de 0,01 grados (cerca de 1,1 km), en las celdas con al menos cinco avisos; en gris, los avisos de validación.")
)

## ---- 6.4. reduccion del error --------------------------------
elementos_1d <- list(
  reduccion = tabla_reporte(data.frame(modelo = annex$modelo , rmse = annex$rmse ,
                                 vs_m0 = c(NA , reduccion[["m1_vs_m0"]] , reduccion[["m2_vs_m0"]]) ,
                                 vs_anterior = c(NA , reduccion[["m1_vs_m0"]] , reduccion[["m2_vs_m1"]])) ,
                "Reducción del error de validación" , decimales = 1 ,
                columnas = c("Modelo" , "RMSE de validación" , "Frente a M0 (%)" , "Frente al modelo anterior (%)") ,
                encabezado = c(" " = 2 , "Reducción del RMSE" = 2) ,
                nota = "RMSE en millones de pesos. La reducción es 1 menos el cociente de los RMSE, en porcentaje.")
)

## ---- 6.5. todos los elementos juntos, en el orden del informe --------
elementos <- c(elementos_1a , elementos_1b , elementos_1c , elementos_1d)


##============================================================================##
##  7. EXPORTAR                                                               ##
##============================================================================##
## la lista que leen los .Rmd de 06_report (ver 00_run.R, seccion 2):
## el anexo, los elementos y algunas cifras para citar en las respuestas (p1$...)

part_one <- list(anexo = annex , elementos = elementos ,
                 n_train = nrow(train) , n_test = nrow(test) ,
                 n_fit = nrow(fit) , n_val = nrow(val) ,
                 media_ajuste = media_ajuste , slope_pred_obs = slope_pred_obs ,
                 rmse_m0 = rmse_m0 , rmse_m1 = rmse_m1 , rmse_m2 = rmse_m2 ,
                 reduccion = reduccion , quintiles = quintile_diagnostics)
export(part_one , "03_baselines/output/results_part_1.rds")
