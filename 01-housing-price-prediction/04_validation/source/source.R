##############################################################################
##  Big Data y Machine Learning para Economia Aplicada                      ##
##  Precio de la vivienda en Cali — Parte 2: validacion y regularizacion         ##
##  Autor: Elkin Mola                                                       ##
##############################################################################


##============================================================================##
##  0. CONFIGURACION INICIAL                                                  ##
##============================================================================##

## paquetes y funciones: rmse(), mae(), r2(), knn_reg(), glmnet
## y las funciones del reporte (ver 00_run.R)
solo_entorno <- TRUE ; source("00_run.R")

## colores de las figuras (los mismos de la parte 1)
azul <- "#1F497D"
naranja <- "#D9822B"
rojo <- "#B2182B"
colores_pliegue <- c("1"=azul , "2"=naranja , "3"=rojo , "4"="#4E9A62" , "5"="#8E6BB0")

## numeros con coma decimal en los ejes
coma <- label_number(big.mark="." , decimal.mark=",")

## enteros con punto de miles, para las notas de las tablas (15.277)
entero <- function(x) formatC(round(x) , format="d" , big.mark="." , decimal.mark=",")


##============================================================================##
##  1. DATOS Y FUNCIONES DE LA VALIDACION CRUZADA                             ##
##============================================================================##

## ---- 1.1. cargar los avisos con las variables de ubicacion -----------------
db <- import("04_validation/input/vivienda_cali_variables.csv")

## ---- 1.2. el entrenamiento: en esta parte la prueba no se toca ------------
train <- db %>% subset(muestra=="entrenamiento")
nrow(train)

## ---- 1.3. transformaciones fijas (no se aprenden de los datos) -------------
## log del area, coordenadas centradas, estrato como categoria y log de las
## distancias: variables_modelo() de 00_run.R (seccion 1b)
train <- variables_modelo(train)

## ---- 1.4. funciones de la validacion cruzada --------------------------------
## estan en 00_run.R (seccion 1b), porque la parte 3 usa las mismas:
##  - imputar(fit , val): habitaciones con la mediana por tipo de los avisos de
##    ajuste, aplicada a los dos lados (los de validacion nunca entran en la cuenta)
##  - cv_lm(formula , datos , pliegue): el cv a mano;
##    devuelve la prediccion fuera de pliegue de cada aviso del entrenamiento
##  - rmse_pliegues(y , pred , pliegue): el rmse de cada pliegue


##============================================================================##
##  2. CINCO PLIEGUES ALEATORIOS                                              ##
##============================================================================##

## ---- 2.1. los 5 pliegues: cada aviso a uno al azar --------------------------
set.seed(12345)
pliegue_azar <- sample(rep(1:5 , length.out=nrow(train)))
table(pliegue_azar)

## ---- 2.2. M1 y M2, las mismas formulas de la parte 1 -----------------------
f_m1 <- precio ~ area
f_m2 <- precio ~ area + banos + tipo + lon + lat
pred_m1_azar <- cv_lm(f_m1 , train , pliegue_azar)
pred_m2_azar <- cv_lm(f_m2 , train , pliegue_azar)

## ---- 2.3. rmse por pliegue, su promedio y su desviacion ---------------------
rmse_azar <- data.frame(pliegue = 1:5 ,
                        m1 = rmse_pliegues(train$precio , pred_m1_azar , pliegue_azar) ,
                        m2 = rmse_pliegues(train$precio , pred_m2_azar , pliegue_azar))
rmse_azar

## ---- 2.4. la validacion de la parte 1 (80/20, semilla 12345) ---------------
p1 <- readRDS("04_validation/input/results_part_1.rds")
c(m1 = p1$rmse_m1 , m2 = p1$rmse_m2)

## ---- 2.5. lo que va al reporte ---------------------------------------------
tabla_2a <- data.frame(fila = c(paste("Pliegue" , 1:5) , "Promedio" , "Desviación estándar" ,
                                "Validación de la Parte 1 (80/20)") ,
                       avisos = c(as.vector(table(pliegue_azar)) , NA , NA , p1$n_val) ,
                       m1 = c(rmse_azar$m1 , mean(rmse_azar$m1) , sd(rmse_azar$m1) , p1$rmse_m1) ,
                       m2 = c(rmse_azar$m2 , mean(rmse_azar$m2) , sd(rmse_azar$m2) , p1$rmse_m2))

elementos_2a <- list(
  cv_aleatoria = tabla_reporte(tabla_2a , "RMSE de M1 y M2 con cinco pliegues aleatorios" ,
                 columnas = c("" , "Avisos validados" , "M1" , "M2") , decimales = 1 ,
                 grupos = c("Validación cruzada, 5 pliegues aleatorios" = 5 , "Resumen entre pliegues" = 2 ,
                            "Parte 1" = 1) ,
                 nota = paste0("RMSE en millones de pesos. Cada pliegue se valida con los modelos ajustados en los otros cuatro (" ,
                               entero(nrow(train)) , " avisos de entrenamiento, semilla 12345)."))
)


##============================================================================##
##  3. PLIEGUES ESPACIALES                                                    ##
##============================================================================##

## ---- 3.1. la grilla: celdas de 0,02 grados (cerca de 2,2 km de lado) -------
celda <- 0.02
train <- train %>% mutate(zona = paste(floor(lon/celda) , floor(lat/celda)))
zonas <- unique(train$zona)
length(zonas)

## ---- 3.2. cada zona entera a un pliegue al azar ----------------------------
set.seed(12345)
grupo <- sample(rep(1:5 , length.out=length(zonas)))
pliegue_zona <- grupo[match(train$zona , zonas)]
table(pliegue_zona)

## ---- 3.3. M1 y M2 con los pliegues espaciales ------------------------------
pred_m1_zona <- cv_lm(f_m1 , train , pliegue_zona)
pred_m2_zona <- cv_lm(f_m2 , train , pliegue_zona)
rmse_zona <- data.frame(pliegue = 1:5 ,
                        m1 = rmse_pliegues(train$precio , pred_m1_zona , pliegue_zona) ,
                        m2 = rmse_pliegues(train$precio , pred_m2_zona , pliegue_zona))
rmse_zona

## ---- 3.4. los dos esquemas lado a lado -------------------------------------
tabla_2b <- data.frame(modelo = c("M1: recta con el área" , "M2: área, baños, tipo y coordenadas") ,
                       azar_media = c(mean(rmse_azar$m1) , mean(rmse_azar$m2)) ,
                       azar_sd = c(sd(rmse_azar$m1) , sd(rmse_azar$m2)) ,
                       zona_media = c(mean(rmse_zona$m1) , mean(rmse_zona$m2)) ,
                       zona_sd = c(sd(rmse_zona$m1) , sd(rmse_zona$m2)))
tabla_2b

## el rmse de cada pliegue en los dos esquemas (el detalle detras del resumen)
tabla_2b_pliegues <- data.frame(pliegue = paste("Pliegue" , 1:5) ,
                                azar_m1 = rmse_azar$m1 , azar_m2 = rmse_azar$m2 ,
                                azar_dif = rmse_azar$m1 - rmse_azar$m2 ,
                                zona_m1 = rmse_zona$m1 , zona_m2 = rmse_zona$m2 ,
                                zona_dif = rmse_zona$m1 - rmse_zona$m2)
tabla_2b_pliegues

## los avisos validados por pliegue en cada esquema
avisos_pliegue <- data.frame(pliegue = paste("Pliegue" , 1:5) ,
                             azar = as.vector(table(pliegue_azar)) ,
                             espacial = as.vector(table(pliegue_zona)) ,
                             zonas = as.vector(table(grupo)))

## ---- 3.5. figura: los pliegues en el mapa ----------------------------------
mapa_pliegues <- function(pliegue){
                 ggplot(train %>% mutate(pliegue = factor(pliegue)) , aes(x=lon , y=lat , color=pliegue)) +
                 geom_point(size=0.35 , alpha=0.7 , stroke=0) +
                 scale_color_manual(values=colores_pliegue , name="Pliegue") +
                 guides(color=guide_legend(override.aes=list(size=2.5 , alpha=1))) +
                 coord_quickmap() +
                 labs(x="Longitud" , y="Latitud")
}
plot_pliegues_azar <- mapa_pliegues(pliegue_azar)
plot_pliegues_zona <- mapa_pliegues(pliegue_zona)

## ---- 3.6. el esquema que se usa en (c), (d) y (e) --------------------------
## cambiar aqui si deciden el otro ("azar" o "espacial"); todo lo que sigue usa
## estos pliegues
esquema <- "espacial"
pliegue <- if (esquema=="espacial") pliegue_zona else pliegue_azar
nombre_esquema <- if (esquema=="espacial") "pliegues espaciales" else "pliegues aleatorios"

## ---- 3.7. lo que va al reporte ---------------------------------------------
elementos_2b <- list(
  mapa_pliegues = figura_reporte(list(plot_pliegues_azar , plot_pliegues_zona) , alto = 3.6 ,
                  titulo = paste0("Avisos de entrenamiento según el pliegue en que se validan. Panel A: cinco pliegues aleatorios. Panel B: cinco pliegues espaciales; la ciudad se divide en celdas de 0,02 grados (cerca de 2,2 km) y cada celda entera va a un pliegue (" ,
                                  length(zonas) , " celdas con avisos).")),
  pliegues = tabla_reporte(avisos_pliegue , "Avisos validados en cada pliegue" ,
             columnas = c("" , "Aleatorios" , "Espaciales" , "Celdas (espaciales)") ,
             encabezado = c(" " = 1 , "Avisos" = 2 , " " = 1)),
  rmse_cada_pliegue = tabla_reporte(tabla_2b_pliegues , "RMSE de M1 y M2 en cada pliegue, aleatorio y espacial" ,
             columnas = c("" , "M1" , "M2" , "Diferencia" , "M1" , "M2" , "Diferencia") ,
             encabezado = c(" " = 1 , "Pliegues aleatorios" = 3 , "Pliegues espaciales" = 3) , decimales = 1 ,
             nota = "RMSE de validación en millones de pesos. Cada pliegue se valida con los modelos ajustados en los otros cuatro. Diferencia = RMSE de M1 menos RMSE de M2 en ese pliegue."),
  esquemas = tabla_reporte(tabla_2b , "RMSE de M1 y M2 con pliegues aleatorios y espaciales" ,
             columnas = c("Modelo" , "Promedio" , "Desv. estándar" , "Promedio" , "Desv. estándar") ,
             encabezado = c(" " = 1 , "Pliegues aleatorios" = 2 , "Pliegues espaciales" = 2) , decimales = 1 ,
             nota = "RMSE de validación en millones de pesos: promedio y desviación estándar de los cinco pliegues.")
)


##============================================================================##
##  4. TRES (Y UNA) ESPECIFICACIONES                                          ##
##============================================================================##

## ---- 4.1. las especificaciones ----------------------------------------------
## E1. M2, la de la parte 1
## E2. interacciones con el tipo: el metro cuadrado y el bano adicional pueden
##     valer distinto en una casa que en un apartamento (area x tipo, banos x tipo)
## E3. polinomios y logaritmos: log del area y su cuadrado, banos y su cuadrado,
##     y un polinomio de grado 3 en las coordenadas (la superficie de precios)
## E4. la mas rica: E2 y E3 juntas, mas habitaciones (imputada en el pliegue) y
##     su indicador de faltante, el estrato (indicadoras), estrato x tipo, el
##     anio y las distancias en logaritmo. Es la matriz ampliada de (d)
f_inter <- precio ~ area*tipo + banos*tipo + lon + lat
f_poli <- precio ~ log_area + I(log_area^2) + banos + I(banos^2) + tipo +
                   cx + cy + I(cx^2) + I(cy^2) + I(cx*cy) + I(cx^3) + I(cy^3)
distancias <- grep("^log_(dist|area_verde)" , names(train) , value=T)
f_rica <- as.formula(paste("precio ~ (area + log_area + I(log_area^2) + banos + I(banos^2) +" ,
                           "habitaciones + habitaciones_na + estrato)*tipo +" ,
                           "cx + cy + I(cx^2) + I(cy^2) + I(cx*cy) + I(cx^3) + I(cy^3) + anio +" ,
                           paste(distancias , collapse=" + ")))
especificaciones <- list("E1. M2: área, baños, tipo y coordenadas" = f_m2 ,
                         "E2. Interacciones con el tipo" = f_inter ,
                         "E3. Polinomios y logaritmos" = f_poli ,
                         "E4. Rica: E2 + E3, habitaciones, estrato y distancias" = f_rica)

## ---- 4.2. las cuatro con los mismos pliegues --------------------------------
## (el preprocesamiento que se aprende, la mediana de habitaciones, se hace
## dentro de cada pliegue en cv_lm(); las indicadoras salen de niveles fijos)
pred_spec <- lapply(especificaciones , cv_lm , datos=train , pliegue=pliegue)

## ---- 4.3. metricas de validacion y coeficientes de cada una ----------------
coeficientes <- sapply(especificaciones , function(f) ncol(model.matrix(f , imputar(train , train)$fit)))
tabla_2c <- tabla_anexo(pred_spec , train$precio) %>%
            mutate(coeficientes = unname(coeficientes) ,
                   media_pliegues = sapply(pred_spec , function(p) mean(rmse_pliegues(train$precio , p , pliegue))) ,
                   sd_pliegues = sapply(pred_spec , function(p) sd(rmse_pliegues(train$precio , p , pliegue)))) %>%
            select(modelo , coeficientes , rmse , mae , r2 , media_pliegues , sd_pliegues)
tabla_2c

## el rmse de cada pliegue para cada especificacion (el detalle detras del resumen)
rmse_spec <- sapply(pred_spec , function(p) rmse_pliegues(train$precio , p , pliegue))
colnames(rmse_spec) <- paste0("e" , 1:4)
tabla_2c_pliegues <- data.frame(pliegue = paste("Pliegue" , 1:5) ,
                                avisos = as.vector(table(pliegue)) ,
                                rmse_spec)
tabla_2c_pliegues

## la mejor especificacion lineal: el menor rmse de validacion
mejor_spec <- tabla_2c$modelo[which.min(tabla_2c$rmse)]
mejor_spec

## ---- 4.4. lo que va al reporte ---------------------------------------------
elementos_2c <- list(
  especificaciones_pliegues = tabla_reporte(tabla_2c_pliegues ,
                              paste0("RMSE de cada especificación en cada pliegue (" , nombre_esquema , ")") ,
                              columnas = c("" , "Avisos validados" , "E1" , "E2" , "E3" , "E4") , decimales = 1 ,
                              nota = "RMSE de validación en millones de pesos. E1: M2 (área, baños, tipo y coordenadas); E2: interacciones con el tipo; E3: polinomios y logaritmos; E4: rica. Cada pliegue se valida con los modelos ajustados en los otros cuatro."),
  especificaciones = tabla_reporte(tabla_2c , paste0("Especificaciones lineales con " , nombre_esquema) ,
                     columnas = c("Especificación" , "Coef." , "RMSE" , "MAE" , "R²" , "Promedio" , "Desv. est.") ,
                     encabezado = c(" " = 2 , "Validación (fuera de pliegue)" = 3 , "RMSE entre pliegues" = 2) ,
                     decimales = 2 ,
                     nota = paste0("RMSE y MAE en millones de pesos, con las predicciones fuera de pliegue de los " ,
                                   entero(nrow(train)) , " avisos de entrenamiento. Coef.: columnas de la matriz, intercepto incluido. La mediana de habitaciones (E4) se calcula dentro de cada pliegue."))
)


##============================================================================##
##  5. RIDGE, LASSO Y ELASTIC NET                                             ##
##============================================================================##

## ---- 5.1. la matriz ampliada: la especificacion E4 --------------------------

x_rica <- model.matrix(f_rica , imputar(train , train)$fit)[,-1]
dim(x_rica)
matriz_ampliada <- c(n = nrow(x_rica) , p = ncol(x_rica))

## ---- 5.2. el juez a mano para glmnet ---------------------------------------

cv_glmnet_pliegues <- function(formula , datos , pliegue , alpha , lambdas){
                      pred <- matrix(NA , nrow(datos) , length(lambdas))
                      for (j in sort(unique(pliegue))){
                           partes <- imputar(datos[pliegue!=j,] , datos[pliegue==j,])
                           x_fit <- model.matrix(formula , partes$fit)[,-1]
                           x_val <- model.matrix(formula , partes$val)[,-1]
                           modelo_j <- glmnet(x_fit , partes$fit$precio , alpha=alpha , lambda=lambdas)
                           pred[pliegue==j,] <- predict(modelo_j , newx=x_val , s=lambdas)
                      }
                      error2 <- (datos$precio - pred)^2
                      mse_pliegue <- sapply(sort(unique(pliegue)) , function(j) colMeans(error2[pliegue==j , , drop=F]))
                      list(alpha = alpha , lambda = lambdas , pred = pred ,
                           cvm = colMeans(error2) ,
                           cvsd = apply(mse_pliegue , 1 , sd)/sqrt(ncol(mse_pliegue)))
}

## ---- 5.3. la grilla de lambda de cada alfa ---------------------------------
alphas <- c("Ridge" = 0 , "Elastic Net (α = 0,25)" = 0.25 , "Elastic Net (α = 0,5)" = 0.5 ,
            "Elastic Net (α = 0,75)" = 0.75 , "Lasso" = 1)
grilla <- function(alpha){
          glmnet(x_rica , train$precio , alpha=alpha , lambda.min.ratio=1e-7 , nlambda=150)$lambda
}

## ---- 5.4. el juez para cada alfa, con los mismos pliegues ------------------
caminos <- lapply(alphas , function(a) cv_glmnet_pliegues(f_rica , train , pliegue , a , grilla(a)))

## ---- 5.5. lambda_min y lambda_1se de cada alfa -----------------------------

resumir_camino <- function(camino){
                  i_min <- which.min(camino$cvm)
                  techo <- camino$cvm[i_min] + camino$cvsd[i_min]
                  i_1se <- which(camino$lambda==max(camino$lambda[camino$cvm <= techo]))
                  completo <- glmnet(x_rica , train$precio , alpha=camino$alpha , lambda=camino$lambda)
                  data.frame(alpha = camino$alpha ,
                             lambda_min = camino$lambda[i_min] , rmse_min = sqrt(camino$cvm[i_min]) ,
                             coef_min = sum(coef(completo , s=camino$lambda[i_min])[-1]!=0) ,
                             lambda_1se = camino$lambda[i_1se] , rmse_1se = sqrt(camino$cvm[i_1se]) ,
                             coef_1se = sum(coef(completo , s=camino$lambda[i_1se])[-1]!=0) ,
                             i_min = i_min , i_1se = i_1se ,
                             borde = i_min==length(camino$lambda))
}
lambdas <- do.call(rbind , lapply(caminos , resumir_camino))
lambdas <- data.frame(metodo = names(alphas) , lambdas , row.names = NULL)
lambdas

## si algun lambda_min cae en el borde de la grilla, el minimo verdadero esta afuera
lambdas$metodo[lambdas$borde]

## el mejor Elastic Net (el alfa con menor rmse en su lambda_min) y el mejor de los tres
en <- lambdas %>% subset(alpha > 0 & alpha < 1)
mejor_en <- en$metodo[which.min(en$rmse_min)]
penalizados <- lambdas %>% subset(metodo %in% c("Ridge" , "Lasso" , mejor_en))
mejor_pen <- penalizados$metodo[which.min(penalizados$rmse_min)]
c(mejor_en = mejor_en , mejor_penalizado = mejor_pen)

## el rmse de cada pliegue de los penalizados y de E4 sin penalizar (el detalle
## detras del resumen); E4 es la cuarta especificacion, la de la matriz ampliada
pred_pen <- list(e4 = pred_spec[[4]] ,
                 ridge = caminos$Ridge$pred[ , lambdas$i_min[lambdas$metodo=="Ridge"]] ,
                 lasso = caminos$Lasso$pred[ , lambdas$i_min[lambdas$metodo=="Lasso"]] ,
                 en = caminos[[mejor_en]]$pred[ , lambdas$i_min[lambdas$metodo==mejor_en]] ,
                 lasso_1se = caminos$Lasso$pred[ , lambdas$i_1se[lambdas$metodo=="Lasso"]])
tabla_2d_pliegues <- data.frame(pliegue = paste("Pliegue" , 1:5) ,
                                avisos = as.vector(table(pliegue)) ,
                                sapply(pred_pen , function(p) rmse_pliegues(train$precio , p , pliegue)))
tabla_2d_pliegues

## ---- 5.6. figura: rmse de validacion contra log lambda ---------------------
## la banda es +/- un error estandar; las lineas, lambda_min (roja) y lambda_1se (naranja)
curvas <- do.call(rbind , lapply(c("Ridge" , "Lasso" , mejor_en) , function(m)
                  data.frame(metodo = m , log_lambda = log(caminos[[m]]$lambda) ,
                             rmse = sqrt(caminos[[m]]$cvm) ,
                             bajo = sqrt(caminos[[m]]$cvm - caminos[[m]]$cvsd) ,
                             alto = sqrt(caminos[[m]]$cvm + caminos[[m]]$cvsd))))
curvas$metodo <- factor(curvas$metodo , levels=c("Ridge" , "Lasso" , mejor_en))
marcas <- lambdas %>%
          subset(metodo %in% levels(curvas$metodo)) %>%
          mutate(metodo = factor(metodo , levels=levels(curvas$metodo)))
plot_camino <- ggplot(curvas , aes(x=log_lambda , y=rmse)) +
               geom_ribbon(aes(ymin=bajo , ymax=alto) , fill=azul , alpha=0.18) +
               geom_line(color=azul , linewidth=0.7) +
               geom_vline(data=marcas , aes(xintercept=log(lambda_min)) , color=rojo , linetype="dashed" , linewidth=0.5) +
               geom_vline(data=marcas , aes(xintercept=log(lambda_1se)) , color=naranja , linetype="dashed" , linewidth=0.5) +
               facet_wrap(~ metodo , scales="free_x") +
               scale_y_continuous(labels=coma) +
               labs(x="log λ (a la derecha, más penalización)" , y="RMSE de validación (millones COP)")

## ---- 5.7. lo que va al reporte ---------------------------------------------
## la composicion de la matriz: cuantas columnas aporta cada bloque de E4
columnas <- colnames(x_rica)
interaccion <- grepl(":" , columnas)
bloque <- ifelse(interaccion , "Interacciones de los anteriores con el tipo" ,
          ifelse(columnas=="tipoCasa" , "Tipo (Casa)" ,
          ifelse(grepl("^estrato" , columnas) , "Estrato (indicadoras de 2 a 6)" ,
          ifelse(grepl("c[xy]" , columnas) , "Coordenadas: polinomio de grado 3" ,
          ifelse(columnas %in% distancias , "Distancias y área verde, en logaritmo" ,
          ifelse(columnas=="anio" , "Año del aviso" ,
                 "Área, log(área), log(área)², baños, baños², habitaciones y su faltante"))))))
orden_bloques <- c("Tipo (Casa)" , "Área, log(área), log(área)², baños, baños², habitaciones y su faltante" ,
                   "Estrato (indicadoras de 2 a 6)" , "Interacciones de los anteriores con el tipo" ,
                   "Coordenadas: polinomio de grado 3" , "Distancias y área verde, en logaritmo" , "Año del aviso")
tabla_matriz <- data.frame(bloque = c(orden_bloques , "Total de columnas (p)" , "Avisos de entrenamiento (n)") ,
                           columnas = c(as.vector(table(factor(bloque , levels=orden_bloques))) ,
                                        ncol(x_rica) , nrow(x_rica)))
tabla_matriz
## los lambda con tres cifras significativas (van de milesimas a cientos)
cifras3 <- function(x) formatC(signif(x , 3) , format="fg" , digits=3 , big.mark="." , decimal.mark=",")
tabla_2d <- lambdas %>%
            mutate(lambda_min = cifras3(lambda_min) , lambda_1se = cifras3(lambda_1se)) %>%
            select(metodo , lambda_min , rmse_min , coef_min , lambda_1se , rmse_1se , coef_1se)
en_borde <- lambdas$metodo[lambdas$borde]
nota_borde <- if (length(en_borde) > 0) paste0(" λ mín es el menor λ de la grilla en: " , paste(en_borde , collapse=", ") , ".") else ""

elementos_2d <- list(
  matriz = tabla_reporte(tabla_matriz , "La matriz ampliada (especificación E4): n y p" ,
           columnas = c("Bloque" , "Columnas") , grupos = c("Columnas por bloque" = 7 , "Tamaño" = 2) ,
           nota = "Sin el intercepto, que glmnet no penaliza."),
  camino_cv = figura_reporte(plot_camino , alto = 2.9 ,
              titulo = paste0("RMSE de validación contra log λ (" , nombre_esquema , ", especificación E4). La banda es ± un error estándar entre pliegues; la línea roja marca λ mín y la naranja, λ 1se.")),
  regularizacion_pliegues = tabla_reporte(tabla_2d_pliegues ,
                            paste0("RMSE de E4 sin penalizar y de los modelos penalizados en cada pliegue (" , nombre_esquema , ")") ,
                            columnas = c("" , "Avisos validados" , "E4 sin penalizar" , "Ridge" , "Lasso" ,
                                         sub("Elastic Net" , "EN" , mejor_en) , "Lasso") ,
                            encabezado = c(" " = 3 , "λ mín" = 3 , "λ 1se" = 1) , decimales = 1 ,
                            nota = paste0("RMSE de validación en millones de pesos. Todos usan la matriz de E4 y los mismos pliegues; cada pliegue se valida con el modelo ajustado en los otros cuatro. EN: el Elastic Net con menor RMSE en su λ mín (" ,
                                          mejor_en , ").")),
  regularizacion = tabla_reporte(tabla_2d , "Ridge, Lasso y Elastic Net: λ elegido por la validación" ,
                   columnas = c("Método" , "λ" , "RMSE" , "Coef. ≠ 0" , "λ" , "RMSE" , "Coef. ≠ 0") ,
                   encabezado = c(" " = 1 , "λ mín" = 3 , "λ 1se" = 3) , decimales = 2 ,
                   nota = paste0("RMSE de validación en millones de pesos (" , nombre_esquema ,
                                 "). λ en la escala de glmnet. Coef. ≠ 0: coeficientes distintos de cero, sin el intercepto, con el ajuste en todo el entrenamiento." ,
                                 nota_borde))
)


##============================================================================##
##  6. k-NN EN EL MAPA, TODOS LOS MODELOS Y EL LASSO EN 1SE                    ##
##============================================================================##

## ---- 6.1. k-NN sobre lon y lat estandarizadas ------------------------------

ks <- c(1 , 5 , 10 , 25 , 50 , 100)
pred_knn <- matrix(NA , nrow(train) , length(ks))
for (j in sort(unique(pliegue))){
     coord_fit <- as.matrix(train[pliegue!=j , c("lon","lat")])
     coord_val <- as.matrix(train[pliegue==j , c("lon","lat")])
     centro <- colMeans(coord_fit)
     escala <- apply(coord_fit , 2 , sd)
     pred_knn[pliegue==j,] <- knn_reg(scale(coord_fit , center=centro , scale=escala) , train$precio[pliegue!=j] ,
                                      scale(coord_val , center=centro , scale=escala) , ks)
}

## ---- 6.2. el k que elige la validacion -------------------------------------
rmse_knn <- data.frame(k = ks , rmse = sqrt(colMeans((train$precio - pred_knn)^2)))
rmse_knn
k_mejor <- rmse_knn$k[which.min(rmse_knn$rmse)]
k_mejor

## el rmse de cada pliegue para cada k (el detalle detras del resumen)
rmse_knn_pliegues <- sapply(seq_along(ks) , function(i) rmse_pliegues(train$precio , pred_knn[ , i] , pliegue))
colnames(rmse_knn_pliegues) <- paste0("k" , ks)
tabla_knn_pliegues <- data.frame(pliegue = paste("Pliegue" , 1:5) ,
                                 avisos = as.vector(table(pliegue)) ,
                                 rmse_knn_pliegues)
tabla_knn_pliegues

plot_knn <- ggplot(rmse_knn , aes(x=k , y=rmse)) +
            geom_line(color=azul , linewidth=0.7) +
            geom_point(color=azul , size=1.8) +
            geom_point(data=rmse_knn %>% subset(k==k_mejor) , color=rojo , size=2.8) +
            scale_x_log10(breaks=ks) +
            scale_y_continuous(labels=coma) +
            labs(x="k: vecinos en el mapa (escala logarítmica)" , y="RMSE de validación (millones COP)")

## ---- 6.3. todos los modelos hasta aqui, con la misma validacion ------------
## M0 es la media de los avisos de ajuste de cada pliegue (una regresion sin x)
pred_m0 <- cv_lm(precio ~ 1 , train , pliegue)
pred_m1 <- cv_lm(f_m1 , train , pliegue)
i_lasso_1se <- lambdas$i_1se[lambdas$metodo=="Lasso"]
preds_todos <- c(list("M0: media del entrenamiento" = pred_m0 ,
                      "M1: recta con el área" = pred_m1) ,
                 pred_spec ,
                 list("Ridge (λ mín)" = caminos$Ridge$pred[ , lambdas$i_min[lambdas$metodo=="Ridge"]] ,
                      "Lasso (λ mín)" = caminos$Lasso$pred[ , lambdas$i_min[lambdas$metodo=="Lasso"]] ,
                      "Lasso (λ 1se)" = caminos$Lasso$pred[ , i_lasso_1se]) ,
                 setNames(list(caminos[[mejor_en]]$pred[ , lambdas$i_min[lambdas$metodo==mejor_en]]) ,
                          sub(")" , "; λ mín)" , mejor_en , fixed=T)) ,
                 setNames(list(pred_knn[ , ks==k_mejor]) , paste0("k-NN en el mapa (k = " , k_mejor , ")")))
tabla_todos <- tabla_anexo(preds_todos , train$precio)
tabla_todos

## ---- 6.4. las filas de la parte 2 en la tabla de modelos -----------------------------
## la mejor especificacion lineal, el mejor penalizado (en su lambda_min) y el k-NN
nombre_pen <- paste0(mejor_pen , ", λ mín")
anexo_2 <- tabla_anexo(setNames(list(pred_spec[[mejor_spec]] ,
                                     caminos[[mejor_pen]]$pred[ , lambdas$i_min[lambdas$metodo==mejor_pen]] ,
                                     pred_knn[ , ks==k_mejor]) ,
                                c(paste0("Mejor especificación lineal (Parte 2): " , sub("^E[0-9]\\. " , "" , mejor_spec)) ,
                                  paste0("Ridge / Lasso / Elastic Net (mejor): " , nombre_pen) ,
                                  paste0("k-NN en el mapa (k = " , k_mejor , ")"))) ,
                       train$precio)
anexo_2

## ---- 6.5. las variables que conserva el Lasso en lambda_1se ----------------
lasso_completo <- glmnet(x_rica , train$precio , alpha=1 , lambda=caminos$Lasso$lambda)
coef_1se <- coef(lasso_completo , s=caminos$Lasso$lambda[i_lasso_1se])[,1]
lasso_1se <- data.frame(variable = names(coef_1se) , coeficiente = unname(coef_1se)) %>%
             subset(coeficiente!=0 & variable!="(Intercept)")

## el nombre de cada columna de la matriz, en palabras: una tabla de nombres
## para las columnas de la vivienda y las coordenadas, y una regla para las
## distancias; las interacciones terminan en ":tipoCasa"
nombres <- c("tipoCasa" = "Casa (frente a apartamento)" ,
             "area" = "área (m²)" , "log_area" = "log(área)" , "I(log_area^2)" = "log(área)²" ,
             "banos" = "baños" , "I(banos^2)" = "baños²" ,
             "habitaciones" = "habitaciones (imputadas en el pliegue)" ,
             "habitaciones_na" = "habitaciones faltantes" ,
             "estrato2" = "estrato 2" , "estrato3" = "estrato 3" , "estrato4" = "estrato 4" ,
             "estrato5" = "estrato 5" , "estrato6" = "estrato 6" ,
             "cx" = "longitud" , "cy" = "latitud" , "I(cx^2)" = "longitud²" , "I(cy^2)" = "latitud²" ,
             "I(cx * cy)" = "longitud × latitud" , "I(cx^3)" = "longitud³" , "I(cy^3)" = "latitud³" ,
             "anio" = "año del aviso" , "log_area_verde_500m" = "log(área verde a 500 m)")
describir <- function(v){
             interaccion <- grepl(":tipoCasa" , v , fixed=T)
             base <- gsub(":tipoCasa" , "" , v , fixed=T)
             d <- ifelse(base %in% names(nombres) , nombres[base] , base)
             es_distancia <- startsWith(base , "log_dist_")
             lugar <- gsub("_" , " " , sub("_m$" , "" , sub("log_dist_" , "" , base)))
             d[es_distancia] <- paste0("log(distancia a: " , lugar[es_distancia] , ")")
             ifelse(interaccion , paste(d , "× Casa") , d)
}
lasso_1se <- lasso_1se %>% mutate(descripcion = describir(variable)) %>% select(variable , descripcion , coeficiente)
nrow(lasso_1se)
lasso_1se

## ---- 6.6. lo que va al reporte ---------------------------------------------
elementos_2e <- list(
  knn = figura_reporte(plot_knn , alto = 2.6 ,
        titulo = paste0("k-NN en el mapa: RMSE de validación según el número de vecinos (" , nombre_esquema ,
                        "; lon y lat estandarizadas dentro de cada pliegue). En rojo, el k elegido.")),
  knn_pliegues = tabla_reporte(tabla_knn_pliegues , paste0("RMSE del k-NN en el mapa en cada pliegue (" , nombre_esquema , ")") ,
                 columnas = c("" , "Avisos validados" , paste("k =" , ks)) ,
                 encabezado = c(" " = 2 , "Número de vecinos" = length(ks)) , decimales = 1 ,
                 nota = "RMSE de validación en millones de pesos. Cada pliegue se valida con los vecinos de los otros cuatro; lon y lat se estandarizan con la media y la desviación de los avisos de ajuste."),
  tabla_knn = tabla_reporte(rmse_knn , "RMSE de validación del k-NN en el mapa" ,
              columnas = c("k: vecinos" , "RMSE de validación (millones COP)") , decimales = 1),
  todos = tabla_reporte(tabla_todos , paste0("Todos los modelos hasta aquí, con " , nombre_esquema) ,
          columnas = c("Modelo" , "RMSE de validación" , "MAE de validación" , "R² de validación") , decimales = 2 ,
          nota = paste0("Métricas en millones de pesos con las predicciones fuera de pliegue de los " , entero(nrow(train)) ,
                        " avisos de entrenamiento; la misma validación para todos los modelos.")),
  lasso_1se = tabla_reporte(lasso_1se , paste0("Variables que conserva el Lasso en λ 1se (" , nrow(lasso_1se) , " de " ,
                                               ncol(x_rica) , ")") ,
              columnas = c("Columna de la matriz" , "Descripción" , "Coeficiente") , decimales = 3 ,
              nota = "Coeficientes en las unidades originales de cada variable (glmnet estandariza por dentro y los devuelve en la escala original), con el ajuste en todo el entrenamiento. Longitud y latitud: coordenadas centradas en un punto fijo de Cali, en centésimas de grado; las distancias, en metros.")
)


##============================================================================##
##  7. EXPORTAR                                                               ##
##============================================================================##
## la lista que lee 06_report/source/02_validation.Rmd: las filas de la tabla de modelos,
## las tablas y figuras de cada literal y algunas cifras para citar (p2$...)

part_two <- list(anexo = anexo_2 ,
                 elementos = c(elementos_2a , elementos_2b , elementos_2c , elementos_2d , elementos_2e) ,
                 esquema = esquema , n_zonas = length(zonas) , celda = celda ,
                 rmse_azar = rmse_azar , rmse_zona = rmse_zona ,
                 mejor_spec = mejor_spec , n_matriz = nrow(x_rica) , p_matriz = ncol(x_rica) ,
                 lambdas = lambdas %>% select(-i_min , -i_1se) , mejor_en = mejor_en , mejor_pen = mejor_pen ,
                 k_mejor = k_mejor , rmse_knn = rmse_knn ,
                 todos = tabla_todos , lasso_1se = lasso_1se ,
                 especificaciones = especificaciones)
export(part_two , "04_validation/output/results_part_2.rds")
