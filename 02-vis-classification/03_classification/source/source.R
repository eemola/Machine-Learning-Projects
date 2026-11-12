##############################################################################
##  Big Data y Machine Learning para Economia Aplicada                      ##
##  VIS en Cali — Parte 1: clasificacion, ¿es vivienda de interes social? ##
##  Autor: Elkin Mola                                                       ##
##############################################################################
## lee:     input/vivienda_cali_variables.csv (copiada de 02_features/output/)
## escribe: 03_classification/output/results_part_1.rds


##============================================================================##
##  0. CONFIGURACION INICIAL                                                  ##
##============================================================================##

## paquetes y funciones: auc(), matriz() (ver 00_run.R)
solo_entorno <- TRUE ; source("00_run.R")

## colores de las figuras
azul <- "#1F497D"
naranja <- "#D9822B"
rojo <- "#B2182B"


##============================================================================##
##  1. DATOS                                                                  ##
##============================================================================##

## base con las variables de ubicacion
vivienda <- import("03_classification/input/vivienda_cali_variables.csv")

## solo el entrenamiento tiene vis; la prueba no se mira
entrenamiento <- subset(vivienda, muestra == "entrenamiento")


##============================================================================##
##  2. LA REFERENCIA                                                          ##
##============================================================================##

## el clasificador trivial dice "no vis" para todos: acierta 1 - prevalencia
prevalencia <- entrenamiento %>%
  summarise(avisos = n() ,
            n_vis = sum(vis == 1) ,
            n_no_vis = sum(vis == 0) ,
            prevalencia = mean(vis) ,
            accuracy_trivial = 1 - mean(vis))
prevalencia


##============================================================================##
##  3. AJUSTE Y VALIDACION (80 / 20)                                          ##
##============================================================================##

## estratificado por vis: sorteo dentro de las vis y dentro de las no vis,
## para que las dos partes queden con el mismo 5 %
set.seed(12345)
ajuste <- entrenamiento %>% group_by(vis) %>% slice_sample(prop = 0.8) %>% ungroup()
validacion <- entrenamiento %>% subset(!(id %in% ajuste$id))

c(ajuste = nrow(ajuste) , validacion = nrow(validacion))
c(vis_ajuste = mean(ajuste$vis) , vis_validacion = mean(validacion$vis))


##============================================================================##
##  4. LA LOGISTICA, ROC Y MATRIZ CON 0,5                                     ##
##============================================================================##

## logistica con los predictores del aviso, sin el precio
## para estimar mejor se suman las distancias que calculamos en
## 02_features (parques, salud, colegios, universidades, mio y centros comerciales)
## el estrato entra como factor (para que entre sin imponer un orden): la vis se concentra en
## los estratos 1 a 3 y cae de golpe en el 4, no baja en linea recta
modelo_logit <- glm(vis ~ tipo + area + banos + lon + lat + anio + factor(estrato , levels = 1:6) +
                      dist_parque_m + dist_parque_grande_m + area_verde_500m +
                      dist_ecoparque_m + dist_salud_m + dist_hospital_m +
                      dist_hospital_alta_m + dist_colegio_m + dist_universidad_m +
                      dist_mio_m + dist_mall_m + fuera_cali ,
                    family = binomial , data = ajuste)

## probabilidad de vis en la validacion
validacion$prob <- predict(modelo_logit , newdata = validacion , type = "response")

## auc
auc_logit <- auc(validacion$vis , validacion$prob)
auc_logit

## matriz de confusion con 0,5
matriz_05 <- matriz(validacion$vis , validacion$prob , 0.5)
matriz_05

## curva roc: cada umbral es un punto
umbrales <- sort(unique(c(0 , 1 , validacion$prob)) , decreasing = T)
roc <- data.frame(tau = umbrales ,
                  tpr = sapply(umbrales , function(u) mean(validacion$prob[validacion$vis == 1] >= u)) ,
                  fpr = sapply(umbrales , function(u) mean(validacion$prob[validacion$vis == 0] >= u)))


##============================================================================##
##  5. COSTOS Y UMBRAL                                                        ##
##============================================================================##

## costos: dejar pasar una vis (fn) cuesta 10 veces revisar un aviso de mas (fp)
## es un supuesto; solo importa el cociente
c_fp <- 1
c_fn <- 10
tau_estrella <- c_fp/(c_fp + c_fn)
tau_estrella

## matriz con 0,5 y con tau*
matrices <- data.frame(umbral = c("0,5" , "τ*") , tau = c(0.5 , tau_estrella) ,
                       rbind(matriz(validacion$vis , validacion$prob , 0.5) ,
                             matriz(validacion$vis , validacion$prob , tau_estrella))) %>%
  mutate(a_revisar = vp + fp ,
         porcentaje_revisar = a_revisar/nrow(validacion)*100 ,
         costo = fp*c_fp + fn*c_fn)
matrices

## curva precision - recall: un punto por umbral
taus <- seq(0 , 1 , by = 0.01)
curva <- data.frame(tau = taus , t(sapply(taus , function(u) matriz(validacion$vis , validacion$prob , u)))) %>%
  subset(!is.nan(precision))

## y si el cociente de costos fuera otro
cocientes <- c(2 , 5 , 10 , 20 , 50)
resultados <- t(sapply(1/(1 + cocientes) , function(u) matriz(validacion$vis , validacion$prob , u)))
sensibilidad <- data.frame(cociente = cocientes , tau = 1/(1 + cocientes) ,
                           resultados[ , c("vp" , "fp" , "fn" , "sensibilidad" , "precision")]) %>%
  mutate(a_revisar = vp + fp ,
         porcentaje_revisar = a_revisar/nrow(validacion)*100)
sensibilidad


##============================================================================##
##  6. FIGURAS                                                                ##
##============================================================================##

## los dos umbrales, para marcarlos en las curvas
puntos <- matrices %>%
  mutate(fpr = fp/(fp + vn) , tpr = sensibilidad , etiqueta = c("τ = 0,5" , "τ*"))

## roc (en clase: plot(roc$fpr , roc$tpr , type="l"))
plot_roc <- ggplot(roc , aes(x = fpr , y = tpr)) +
  geom_line(color = azul , linewidth = 0.8) +
  geom_abline(intercept = 0 , slope = 1 , linetype = "dashed" , color = "grey50") +
  geom_point(data = puntos , aes(color = etiqueta) , size = 2.8) +
  scale_color_manual(values = c("τ = 0,5" = rojo , "τ*" = naranja) , name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  coord_equal() +
  labs(x = "Falsos positivos (1 − especificidad)" , y = "Sensibilidad (recall)")


## precision contra recall; la linea punteada es la prevalencia
plot_pr <- ggplot(curva , aes(x = sensibilidad , y = precision)) +
  geom_line(color = azul , linewidth = 0.8) +
  geom_hline(yintercept = mean(validacion$vis) , linetype = "dashed" , color = "grey50") +
  geom_point(data = puntos , aes(color = etiqueta) , size = 2.8) +
  scale_color_manual(values = c("τ = 0,5" = rojo , "τ*" = naranja) , name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",") , limits = c(0 , 1)) +
  scale_y_continuous(labels = label_number(decimal.mark = ",") , limits = c(0 , 1)) +
  labs(x = "Recall" , y = "Precision")

##============================================================================##
##  7. LO QUE VA AL REPORTE                                                   ##
##============================================================================##

elementos <- list(
  referencia = tabla_reporte(data.frame(indicador = c("Avisos de entrenamiento" , "Avisos VIS" , "Avisos no VIS" ,
                                                      "Prevalencia de VIS (%)" , "Accuracy del clasificador trivial (%)") ,
                                        valor = c(prevalencia$avisos , prevalencia$n_vis , prevalencia$n_no_vis ,
                                                  prevalencia$prevalencia*100 , prevalencia$accuracy_trivial*100)) ,
                             "Prevalencia de VIS y clasificador trivial" , decimales = 1 ,
                             columnas = c("" , "Valor") ,
                             nota = "El clasificador trivial dice «no VIS» para todos los avisos; su accuracy es 1 menos la prevalencia.") ,
  matrices = tabla_reporte(matrices %>% select(umbral , tau , vp , fn , fp , vn , sensibilidad , precision , exactitud , a_revisar , porcentaje_revisar , costo) ,
                           "Matriz de confusión y métricas con τ = 0,5 y con τ*" , decimales = 3 ,
                           columnas = c("Umbral" , "τ" , "VP" , "FN" , "FP" , "VN" , "Recall" , "Precision" , "Accuracy" , "Avisos a revisar" , "% a revisar" , "Costo") ,
                           nota = paste0("Sobre la validación (" , nrow(validacion) , " avisos, " , sum(validacion$vis) , " VIS). Costos supuestos: c_FP = " , c_fp , " y c_FN = " , c_fn , ".")) ,
  sensibilidad = tabla_reporte(sensibilidad , "¿Qué pasa si cambia el cociente de costos?" , decimales = 3 ,
                               columnas = c("c_FN / c_FP" , "τ*" , "VP" , "FP" , "FN" , "Recall" , "Precision" , "Avisos a revisar" , "% a revisar") ,
                               nota = "Con c_FP = 1. Cada fila usa su propio τ* = 1/(1 + cociente).") ,
  roc = figura_reporte(plot_roc , alto = 3.4 ,
                       titulo = paste0("Curva ROC de la logística en la validación (AUC " , format(round(auc_logit , 3) , decimal.mark = ",") , "). La diagonal es el azar; los puntos son los umbrales τ = 0,5 y τ*.")) ,
  precision_recall = figura_reporte(plot_pr , alto = 3.4 ,
                                    titulo = "Curva precision–recall de la logística en la validación. La línea punteada es la prevalencia; los puntos son los umbrales τ = 0,5 y τ*.")
)


##============================================================================##
##  8. EXPORT                                                                 ##
##============================================================================##

part_one <- list(elementos = elementos ,
                   prevalencia = prevalencia , auc_logit = auc_logit ,
                   n_ajuste = nrow(ajuste) , n_validacion = nrow(validacion) , vis_validacion = sum(validacion$vis) ,
                   c_fp = c_fp , c_fn = c_fn , tau_estrella = tau_estrella ,
                   matrices = matrices , sensibilidad = sensibilidad)

export(part_one , "03_classification/output/results_part_1.rds")

## limpiar: lo que sirve ya quedo en results_part_1.rds
rm(azul , naranja , rojo ,
   vivienda , entrenamiento , prevalencia , ajuste , validacion ,
   modelo_logit , auc_logit , matriz_05 , umbrales , roc ,
   c_fp , c_fn , tau_estrella , matrices , taus , curva ,
   cocientes , resultados , sensibilidad , puntos ,
   plot_roc , plot_pr , elementos , part_one)
