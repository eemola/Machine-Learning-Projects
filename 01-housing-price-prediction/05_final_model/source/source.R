##############################################################################
##  Big Data y Machine Learning para Economia Aplicada                      ##
##  Precio de la vivienda en Cali — Parte 3: el modelo final y las predicciones ##
##  Autor: Elkin Mola                                                       ##
##############################################################################
## lee:     input/vivienda_cali_variables.csv (copiada de 02_features/output/)
## escribe: 05_final_model/output/results_part_3.rds y 05_final_model/output/predicciones_precio.csv
## la prueba se usa una sola vez, al final, para predecir


##============================================================================##
##  0. CONFIGURACION INICIAL                                                  ##
##============================================================================##

## paquetes y funciones: rmse(), mae(), r2(), cv_lm() (ver 00_run.R)
solo_entorno <- TRUE ; source("00_run.R")


##============================================================================##
##  1. DATOS                                                                  ##
##============================================================================##

## base con las variables de ubicacion
vivienda <- import("05_final_model/input/vivienda_cali_variables.csv")

## entrenamiento para escoger y ajustar; prueba solo para predecir al final
entrenamiento <- subset(vivienda , muestra == "entrenamiento")
prueba <- subset(vivienda , muestra == "prueba")
c(entrenamiento = nrow(entrenamiento) , prueba = nrow(prueba))


##============================================================================##
##  2. MODELO FINAL DEL PRECIO                                                ##
##============================================================================##

## el de menor rmse de validacion, reajustado con todo el entrenamiento; al final
## queda el vector precio_predicho (una prediccion por aviso de prueba)

## colores de las figuras (los mismos de las partes 1 y 2)
azul <- "#1F497D"
naranja <- "#D9822B"
rojo <- "#B2182B"
coma <- label_number(big.mark="." , decimal.mark=",")

## ---- 2.1. lo que dejo la parte 2 --------------------------------------------
## todos sus modelos se validaron con los mismos 5 pliegues espaciales; el de
## menor rmse fue la especificacion lineal E4
p2 <- readRDS("05_final_model/input/results_part_2.rds")
p2$todos %>% arrange(rmse)
f_e4 <- p2$especificaciones[[p2$mejor_spec]]
f_e4

## ---- 2.2. los mismos datos y los mismos pliegues de la parte 2 ------------
## variables_modelo(), pliegue_espacial(), cv_lm() e imputar() estan en
## 00_run.R (seccion 1b). Se guardan aparte (train_precio, test_precio)
## para no cambiar los datos que usa la seccion 3
train_precio <- variables_modelo(entrenamiento)
test_precio <- variables_modelo(prueba)
pliegue_precio <- pliegue_espacial(train_precio)
table(pliegue_precio)

## comprobacion: con estos pliegues, E4 repite el rmse de validacion de la parte 2
rmse_e4_parte2 <- p2$todos$rmse[p2$todos$modelo==p2$mejor_spec]
pred_e4 <- cv_lm(f_e4 , train_precio , pliegue_precio)
c(parte_4 = rmse(train_precio$precio , pred_e4) , parte_2 = rmse_e4_parte2)
stopifnot(abs(rmse(train_precio$precio , pred_e4) - rmse_e4_parte2) < 1e-6)

## ---- 2.3. dos mejoras, con la misma validacion ----------------------------
## (i) el precio en logaritmo: el precio tiene una cola larga a la derecha (parte 1)
##     y el logaritmo la acorta. Las metricas se miden igual, en millones:
##     la prediccion vuelve con exp() por el factor de Duan (ajustar_predecir())
## (ii) estrato x area: el metro cuadrado puede valer distinto segun el estrato
##     (E4 ya deja que valga distinto segun el tipo)
f_e5 <- update(f_e4 , . ~ . + estrato:area + estrato:log_area)
candidatos <- list("E4 (Parte 2)" = list(formula = f_e4 , log_y = FALSE) ,
                   "E4 con log(precio)" = list(formula = f_e4 , log_y = TRUE) ,
                   "E5: E4 + estrato × área" = list(formula = f_e5 , log_y = FALSE) ,
                   "E5 con log(precio)" = list(formula = f_e5 , log_y = TRUE))
pred_candidatos <- lapply(candidatos , function(cand) cv_lm(cand$formula , train_precio , pliegue_precio ,
                                                              log_y = cand$log_y))

## ---- 2.4. los candidatos: los modelos de la parte 2 y las mejoras ----------
mejoras <- tabla_anexo(pred_candidatos[-1] , train_precio$precio)
tabla_4a <- rbind(p2$todos , mejoras)
tabla_4a

## el rmse de cada pliegue de los cuatro candidatos
tabla_4a_pliegues <- data.frame(modelo = names(candidatos) ,
                                t(sapply(pred_candidatos , function(p) rmse_pliegues(train_precio$precio , p , pliegue_precio))) ,
                                row.names = NULL)
names(tabla_4a_pliegues)[-1] <- paste0("pliegue_" , 1:5)
tabla_4a_pliegues

## ---- 2.5. el modelo final: el menor rmse de validacion --------------------
rmse_candidatos <- sapply(pred_candidatos , function(p) rmse(train_precio$precio , p))
final <- names(which.min(rmse_candidatos))
final

## el elegido tiene el menor rmse de todos los modelos, tambien los de la parte 2
stopifnot(min(rmse_candidatos) <= min(p2$todos$rmse))
rmse_final <- unname(rmse_candidatos[final])
pred_final_cv <- pred_candidatos[[final]]

## lo que se escribe en la primera linea del informe
descripcion <- c("E4 (Parte 2)" = "regresión lineal del precio con la especificación E4 de la Parte 2" ,
                 "E4 con log(precio)" = "regresión lineal de log(precio) con la especificación E4 de la Parte 2" ,
                 "E5: E4 + estrato × área" = "regresión lineal del precio con la especificación E4 de la Parte 2 más estrato × área" ,
                 "E5 con log(precio)" = "regresión lineal de log(precio) con la especificación E4 de la Parte 2 más estrato × área")
modelo_final <- paste0(final , ", " , descripcion[[final]] ,
                       " (interacciones con el tipo, polinomios del área y de las coordenadas, habitaciones, estrato, año y distancias), reajustada con todo el entrenamiento")
modelo_final

## ---- 2.6. reajustar con todo el entrenamiento y predecir la prueba ---------
## la mediana de habitaciones sale de todo el entrenamiento y se aplica a la prueba
partes <- imputar(train_precio , test_precio)
precio_predicho <- ajustar_predecir(candidatos[[final]]$formula , partes$fit , partes$val ,
                                    log_y = candidatos[[final]]$log_y)

## una prediccion por aviso de prueba, ninguna NA ni negativa
stopifnot(length(precio_predicho)==nrow(prueba) , !anyNA(precio_predicho) , all(precio_predicho > 0))
summary(precio_predicho)

## ---- 2.7. figuras: el modelo final en la validacion y en la prueba ---------
## panel A: predicho (fuera de pliegue) contra observado en el entrenamiento
lim <- range(c(train_precio$precio , pred_final_cv))
plot_final_po <- ggplot(data.frame(observado = train_precio$precio , predicho = pred_final_cv) ,
                        aes(x=observado , y=predicho)) +
                 geom_point(alpha=0.25 , size=0.6 , stroke=0 , color=azul) +
                 geom_abline(intercept=0 , slope=1 , linetype="dashed" , color=rojo , linewidth=0.6) +
                 scale_x_continuous(labels=coma) +
                 scale_y_continuous(labels=coma) +
                 coord_equal(xlim=lim , ylim=lim) +
                 labs(x="Precio observado (millones COP)" , y="Precio predicho (millones COP)")

## panel B: el precio observado en el entrenamiento y el predicho en la prueba
precios <- rbind(data.frame(muestra = "Observado (entrenamiento)" , precio = train_precio$precio) ,
                 data.frame(muestra = "Predicho (prueba)" , precio = precio_predicho))
plot_final_dist <- ggplot(precios , aes(x=precio , color=muestra)) +
                   geom_density(linewidth=0.8 , key_glyph="path") +
                   scale_x_log10(breaks=c(50,100,200,500,1000,2000) , labels=coma) +
                   scale_color_manual(values=c("Observado (entrenamiento)"=azul , "Predicho (prueba)"=naranja) , name=NULL) +
                   labs(x="Precio (millones COP, escala logarítmica)" , y="Densidad") +
                   theme(legend.position="inside" , legend.position.inside=c(0.75,0.88))

## ---- 2.8. lo que va al reporte ---------------------------------------------
## el resumen de los precios: observado, predicho en la validacion y en la prueba
resumen <- function(x) c(min(x) , quantile(x , 0.25) , median(x) , mean(x) , quantile(x , 0.75) , max(x))
tabla_precios <- data.frame(estadistico = c("Mínimo" , "Percentil 25" , "Mediana" , "Media" , "Percentil 75" , "Máximo") ,
                            observado = resumen(train_precio$precio) ,
                            validacion = resumen(pred_final_cv) ,
                            prueba = resumen(precio_predicho))

elementos_4a <- list(
  candidatos = tabla_reporte(tabla_4a , "Candidatos al modelo final del precio" ,
               columnas = c("Modelo" , "RMSE de validación" , "MAE de validación" , "R² de validación") ,
               grupos = c("Modelos de la Parte 2" = nrow(p2$todos) , "Mejoras de E4" = nrow(mejoras)) ,
               decimales = 2 ,
               nota = paste0("Métricas en millones de pesos, con las predicciones fuera de pliegue de los " ,
                             formatC(nrow(train_precio) , format="d" , big.mark="." , decimal.mark=",") ,
                             " avisos de entrenamiento y los mismos 5 pliegues espaciales de la Parte 2. Con log(precio), la predicción vuelve a millones con exp() y el factor de Duan. Modelo elegido (menor RMSE): " ,
                             final , ".")),
  candidatos_pliegues = tabla_reporte(tabla_4a_pliegues , "RMSE de los candidatos en cada pliegue espacial" ,
                        columnas = c("Modelo" , paste("Pliegue" , 1:5)) , decimales = 1 ,
                        nota = "RMSE de validación en millones de pesos."),
  final_figura = figura_reporte(list(plot_final_po , plot_final_dist) , alto = 3.4 ,
                 titulo = paste0("El modelo final (" , final , "). Panel A: precio predicho fuera de pliegue contra el observado, en los avisos de entrenamiento; la línea punteada es la predicción perfecta. Panel B: densidad del precio observado en el entrenamiento y del precio predicho en los " ,
                                 formatC(nrow(prueba) , format="d" , big.mark="." , decimal.mark=",") , " avisos de prueba (escala logarítmica).")),
  final_precios = tabla_reporte(tabla_precios , "Precios observados y predichos por el modelo final" ,
                  columnas = c("" , "Observado (entrenamiento)" , "Predicho fuera de pliegue" , "Predicho (prueba)") ,
                  decimales = 1 ,
                  nota = "Millones de pesos. La prueba se usó una sola vez, para predecir con el modelo reajustado con todo el entrenamiento.")
)

## la fila final de la tabla de modelos: el modelo final, con sus metricas de validacion
anexo_4 <- tabla_anexo(setNames(list(pred_final_cv) , paste0("Modelo final: " , final)) ,
                       train_precio$precio)
anexo_4


##============================================================================##
##  3. PREDICCIONES                                                           ##
##============================================================================##

## una fila por aviso de prueba, sin NA
dir.create("05_final_model/output" , showWarnings=F , recursive=T)
predicciones <- data.frame(id = prueba$id , precio_predicho = precio_predicho)
stopifnot(nrow(predicciones) == nrow(prueba) , !anyNA(predicciones))
export(predicciones , "05_final_model/output/predicciones_precio.csv")


##============================================================================##
##  4. EXPORT                                                                 ##
##============================================================================##

## modelo_final y rmse_final (la primera linea del informe), la fila final de la
## tabla de modelos y las cifras de los candidatos
## ojo: ningun otro nombre puede empezar por "anexo": r$anexo lo tomaria por la tabla de modelos
part_three <- list(anexo = anexo_4 , modelo_final = modelo_final , rmse_final = rmse_final ,
                   final = final , candidatos = tabla_4a , precio_predicho = precio_predicho ,
                   elementos = elementos_4a)
export(part_three , "05_final_model/output/results_part_3.rds")
