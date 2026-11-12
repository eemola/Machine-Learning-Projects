##############################################################################
##  Big Data y Machine Learning para Economia Aplicada                      ##
##  VIS en Cali — Parte 2: el modelo final y las predicciones             ##
##  Autor: Elkin Mola                                                       ##
##############################################################################
## lee:     input/vivienda_cali_variables.csv (copiada de 02_features/output/)
## escribe: 04_final_model/output/results_part_2.rds y 04_final_model/output/predicciones_vis.csv
## la prueba se usa una sola vez, al final, para predecir


##============================================================================##
##  0. CONFIGURACION INICIAL                                                  ##
##============================================================================##

## paquetes y funciones: auc(), matriz() (ver 00_run.R)
solo_entorno <- TRUE ; source("00_run.R")


##============================================================================##
##  1. DATOS                                                                  ##
##============================================================================##

## base con las variables de ubicacion
vivienda <- import("04_final_model/input/vivienda_cali_variables.csv")

## entrenamiento para escoger y ajustar; prueba solo para predecir al final
entrenamiento <- subset(vivienda , muestra == "entrenamiento")
prueba <- subset(vivienda , muestra == "prueba")
c(entrenamiento = nrow(entrenamiento) , prueba = nrow(prueba))


##============================================================================##
##  3. MODELO FINAL DE VIS                                                    ##
##============================================================================##

## ---- 2.1. pliegues aleatorios: 5, estratificados por vis ------------------
## asi cada pliegue tiene sus vis
set.seed(12345)
entrenamiento$pliegue_azar <- NA
entrenamiento$pliegue_azar[entrenamiento$vis == 1] <- sample(rep(1:5 , length.out = sum(entrenamiento$vis == 1)))
entrenamiento$pliegue_azar[entrenamiento$vis == 0] <- sample(rep(1:5 , length.out = sum(entrenamiento$vis == 0)))
table(entrenamiento$pliegue_azar , entrenamiento$vis)

## ---- 2.2. pliegues espaciales: una grilla de celdas --------------
## celdas de 0,02 grados (cerca de 2,2 km); cada celda entera va a un pliegue
celda <- 0.02
entrenamiento <- entrenamiento %>% mutate(zona = paste(floor(lon/celda) , floor(lat/celda)))
zonas <- unique(entrenamiento$zona)
set.seed(12345)
grupo <- sample(rep(1:5 , length.out = length(zonas)))
entrenamiento$pliegue_espacial <- grupo[match(entrenamiento$zona , zonas)]
table(entrenamiento$pliegue_espacial , entrenamiento$vis)

## ---- 2.3. los predictores de cada modelo (ninguno usa el precio) ----------
distancias <- "dist_parque_m + dist_parque_grande_m + area_verde_500m + dist_ecoparque_m + dist_salud_m + dist_hospital_m + dist_hospital_alta_m + dist_colegio_m + dist_universidad_m + dist_mio_m + dist_mall_m"

## el estrato como factor, igual que en la parte 1
x_basica <- "tipo + area + banos + factor(estrato , levels = 1:6)"
x_ubicacion <- paste(x_basica , "+ lon + lat + anio")
x_completa <- paste(x_ubicacion , "+ fuera_cali +" , distancias)
x_curvas <- paste(x_completa , "+ I(area^2) + I(banos^2) + tipo:area")

## las tres logisticas
formulas <- list(logit_basica = as.formula(paste("vis ~" , x_basica)) ,
                 logit_ubicacion = as.formula(paste("vis ~" , x_ubicacion)) ,
                 logit_completa = as.formula(paste("vis ~" , x_completa)))

## lasso, ridge y elastic net: todos sobre la especificacion con curvas
alphas <- c(lasso = 1 , ridge = 0 , elastic_net = 0.5)

## matriz de predictores para glmnet (sin la y, para poder usarla en la prueba)
x_entrenamiento <- model.matrix(as.formula(paste("~" , x_curvas)) , entrenamiento)[ , -1]

## ---- 2.4. funciones de validacion cruzada ---------------------------------

## logistica: cada pliegue se predice con el modelo ajustado en los otros cuatro
cv_logit <- function(formula , pliegue){
            p_fuera <- rep(NA , nrow(entrenamiento))
            for (j in 1:5){
                 modelo_j <- glm(formula , family = binomial , data = entrenamiento[pliegue != j , ])
                 p_fuera[pliegue == j] <- predict(modelo_j , entrenamiento[pliegue == j , ] , type = "response")
            }
            p_fuera
}

## penalizados: cv.glmnet escoge lambda (regla de 1 error estandar) con los
## mismos pliegues; luego cada pliegue se predice con ese lambda
cv_penalizado <- function(alpha , pliegue){
                 ajuste_cv <- cv.glmnet(x_entrenamiento , entrenamiento$vis , family = "binomial" , alpha = alpha ,
                                        foldid = pliegue , type.measure = "auc")
                 p_fuera <- rep(NA , nrow(entrenamiento))
                 for (j in 1:5){
                      modelo_j <- glmnet(x_entrenamiento[pliegue != j , ] , entrenamiento$vis[pliegue != j] ,
                                         family = "binomial" , alpha = alpha , lambda = ajuste_cv$lambda.1se)
                      p_fuera[pliegue == j] <- as.numeric(predict(modelo_j , x_entrenamiento[pliegue == j , ] , type = "response"))
                 }
                 list(modelo = ajuste_cv , p_fuera = p_fuera)
}

## auc, log-loss y brier (el log-loss no admite 0 ni 1, por eso se recorta)
metricas <- function(p){
            y <- entrenamiento$vis
            p_recortada <- pmin(pmax(p , 1e-6) , 1 - 1e-6)
            c(auc = auc(y , p) ,
              logloss = -mean(y*log(p_recortada) + (1 - y)*log(1 - p_recortada)) ,
              brier = mean((p - y)^2))
}

## ---- 2.5. la tabla de comparacion -----------------------------------------
tabla_vis <- data.frame(modelo = c(names(formulas) , names(alphas)) ,
                        auc_azar = NA , auc_espacial = NA , logloss_espacial = NA , brier_espacial = NA)

## las tres logisticas
for (m in names(formulas)){
     p_azar <- cv_logit(formulas[[m]] , entrenamiento$pliegue_azar)
     p_espacial <- cv_logit(formulas[[m]] , entrenamiento$pliegue_espacial)
     tabla_vis[tabla_vis$modelo == m , -1] <- c(auc(entrenamiento$vis , p_azar) , metricas(p_espacial))
}

## lasso, ridge y elastic net (se guarda el de pliegues espaciales para el final)
penalizados <- list()
for (m in names(alphas)){
     azar <- cv_penalizado(alphas[[m]] , entrenamiento$pliegue_azar)
     espacial <- cv_penalizado(alphas[[m]] , entrenamiento$pliegue_espacial)
     penalizados[[m]] <- espacial$modelo
     tabla_vis[tabla_vis$modelo == m , -1] <- c(auc(entrenamiento$vis , azar$p_fuera) , metricas(espacial$p_fuera))
}

## sin modelo: predecir siempre la prevalencia
prevalencia <- mean(entrenamiento$vis)
referencia <- c(logloss = -mean(entrenamiento$vis*log(prevalencia) + (1 - entrenamiento$vis)*log(1 - prevalencia)) ,
                brier = mean((prevalencia - entrenamiento$vis)^2))

tabla_vis
referencia

## ---- 2.6. el ganador: mayor auc con pliegues espaciales -------------------
ganador <- tabla_vis$modelo[which.max(tabla_vis$auc_espacial)]
ganador

## ---- 2.7. reajustar con todo el entrenamiento y predecir la prueba --------
## las logisticas se reajustan con glm; en los penalizados cv.glmnet ya dejo el
## modelo ajustado con todo el entrenamiento (con lambda 1se)
if (ganador %in% names(formulas)){
   modelo_vis <- glm(formulas[[ganador]] , family = binomial , data = entrenamiento)
   prob_vis <- as.numeric(predict(modelo_vis , newdata = prueba , type = "response"))
} else {
   x_prueba <- model.matrix(as.formula(paste("~" , x_curvas)) , prueba)[ , -1]
   prob_vis <- as.numeric(predict(penalizados[[ganador]] , x_prueba , s = "lambda.1se" , type = "response"))
}

## una probabilidad por aviso de prueba, ninguna NA
stopifnot(length(prob_vis) == nrow(prueba) , !anyNA(prob_vis))
summary(prob_vis)


##============================================================================##
##  3. PREDICCIONES                                                           ##
##============================================================================##

## una fila por aviso de prueba, sin NA (prob_vis: la probabilidad de ser VIS)
dir.create("04_final_model/output" , showWarnings=F , recursive=T)
predicciones <- data.frame(id = prueba$id , prob_vis = prob_vis)
stopifnot(nrow(predicciones) == nrow(prueba) , !anyNA(predicciones))
export(predicciones , "04_final_model/output/predicciones_vis.csv")


##============================================================================##
##  4. LO QUE VA AL REPORTE                                                   ##
##============================================================================##

## una tabla como el anexo a: una fila por modelo, las metricas de validacion
## (pliegues espaciales) y al final el modelo escogido
etiquetas <- c(logit_basica = "V1: logit con tipo, área, baños y estrato" ,
               logit_ubicacion = "V2: V1 + coordenadas y año" ,
               logit_completa = "V3: V2 + distancias (logit de la Parte 1)" ,
               lasso = "V4: Lasso (V3 + curvas)" ,
               ridge = "V5: Ridge (V3 + curvas)" ,
               elastic_net = "V6: Elastic Net, α = 0,5 (V3 + curvas)")

anexo_vis <- rbind(data.frame(modelo = "V0: siempre la prevalencia" , auc = 0.5 ,
                              logloss = referencia[["logloss"]] , brier = referencia[["brier"]]) ,
                   data.frame(modelo = etiquetas[tabla_vis$modelo] , auc = tabla_vis$auc_espacial ,
                              logloss = tabla_vis$logloss_espacial , brier = tabla_vis$brier_espacial) ,
                   data.frame(modelo = paste0("Modelo final de VIS: " , sub(":.*" , "" , etiquetas[[ganador]]) , " (reajustado con todo el entrenamiento)") ,
                              auc = tabla_vis$auc_espacial[tabla_vis$modelo == ganador] ,
                              logloss = tabla_vis$logloss_espacial[tabla_vis$modelo == ganador] ,
                              brier = tabla_vis$brier_espacial[tabla_vis$modelo == ganador]))
rownames(anexo_vis) <- NULL
anexo_vis

elementos <- list(
  anexo_vis = tabla_reporte(anexo_vis , "Métricas de validación de los modelos de VIS" ,
              columnas = c("Modelo" , "AUC de validación" , "Log-loss de validación" , "Brier de validación") ,
              decimales = 4 ,
              nota = paste0("Validación cruzada con 5 pliegues espaciales (celdas de 0,02 grados), sobre los " , nrow(entrenamiento) ,
                            " avisos de entrenamiento. AUC: más alto es mejor; log-loss y Brier: más bajo es mejor. ",
                            "V0 no usa predictores. La última fila es el modelo escogido (mayor AUC), reajustado con todo el entrenamiento.")) ,
  esquemas_vis = tabla_reporte(tabla_vis %>% mutate(modelo = etiquetas[modelo]) %>% select(modelo , auc_azar , auc_espacial) ,
                 "AUC de validación con pliegues aleatorios y espaciales" ,
                 columnas = c("Modelo" , "AUC, pliegues aleatorios" , "AUC, pliegues espaciales") ,
                 decimales = 4 ,
                 nota = "Pliegues aleatorios estratificados por VIS. Con pliegues espaciales el modelo predice zonas que no vio al ajustarse.")
)


##============================================================================##
##  5. EXPORT                                                                 ##
##============================================================================##

## el modelo escogido, las tablas de comparacion y las probabilidades de la prueba
part_two <- list(elementos = elementos ,
                 tabla_vis = tabla_vis , referencia_vis = referencia ,
                 tabla_anexo_vis = anexo_vis , modelo_vis = etiquetas[[ganador]] , prob_vis = prob_vis)
export(part_two , "04_final_model/output/results_part_2.rds")
