library(tidyverse)
library(dplyr)
library(caret)
library(gam)
library(klaR)
library(MASS)
library(MXM)
library(dummies)


setwd("D:/TFM/Seleccion de variables")

library(readr)
datos <- read_csv("datos_mw_conv_final_ohe.csv")
df <- as.data.frame(datos)

tabla <- table(datos$TARGET_occurrence_M3.0)
prop.table(tabla)


#Suavizar con logaritmo la energia 
df$energia_acumulada_diaria <- log(df$energia_acumulada_diaria+1)
df$energia_lag1 <- log(df$energia_lag1+1)
df$roll7_energia <- log(df$roll7_energia+1)
df$roll30_energia <- log(df$roll30_energia+1)
df$adj_roll30_energia <- log(df$adj_roll30_energia+1)

#Estandarizacion de las variables numéricas

listconti <- c("eq_count", "energia_acumulada_diaria", 
"main_mag", "main_depth", "main_lat", "main_lon", "magError", 
"depthError", "horizontalError", "nst", "gap", "dmin", "rms", 
"magNst", "is_converted_mag", "lat_bin", "lon_bin",
"b_value_daily", "lag1_mag_max", "lag1_eq_count", "energia_lag1", 
"roll7_eq_count", "roll30_eq_count", "roll7_mag_max", "roll30_mag_max", 
"roll7_energia", "roll30_energia", "days_since_last_eq", "TARGET_mag_max_next_7d", 
"lat", "lon", 
"adj_num_eqs_x", "adj_max_mag_x", "adj_num_eqs_y", "adj_max_mag_y", 
"adj_roll7_eq_count", "adj_roll7_mag_max", "adj_roll30_energia", 
"adj_roll30_eq_count", "adj_roll30_mag_max")


df[, listconti] <- scale(df[, listconti])


df <- df %>%
  mutate(across(where(is.logical), as.numeric))

dput(names(df))

str(df$date)

#División en train y test por corte de tiempo

train <- df[df$date < as.Date("2019-01-01"), ]
test  <- df[df$date >= as.Date("2019-01-01"), ]


#Target encoding con ventana móvil

library(slider)
library(lubridate)
library(dplyr)

#Para el target de clasificacion de 3.0
train_enc_cla <- train %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_cla_enc_30d = slide_index_dbl(
      .x = TARGET_occurrence_M3.0,               # Variable a promediar
      .i = date,                # Índice temporal
      .f = mean,                 # Función a aplicar (media)
      .before = days(30),        # Mirar 30 días hacia atrás
      .after = -1,               # CRÍTICO: -1 excluye el día actual para evitar fuga de datos
      na_rm = TRUE
    )
  ) %>%
  ungroup() %>%
  
  # 4. Opcional: Rellenar los NAs de los primeros días (cuando no hay historial)
  # usando la media global acumulativa (como en el ejemplo anterior) o un valor por defecto.
  mutate(
    target_cla_enc_30d = ifelse(is.na(target_cla_enc_30d), mean(TARGET_occurrence_M3.0, na.rm = TRUE), target_cla_enc_30d)
  )
#Para el target de clasificacion de 4.5
train_enc_cla2 <- train %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_cla_enc_30d2 = slide_index_dbl(
      .x = TARGET_occurrence_M4.5,               # Variable a promediar
      .i = date,                # Índice temporal
      .f = mean,                 # Función a aplicar (media)
      .before = days(30),        # Mirar 30 días hacia atrás
      .after = -1,               # CRÍTICO: -1 excluye el día actual para evitar fuga de datos
      na_rm = TRUE
    )
  ) %>%
  ungroup() %>%
  
  # 4. Opcional: Rellenar los NAs de los primeros días (cuando no hay historial)
  # usando la media global acumulativa (como en el ejemplo anterior) o un valor por defecto.
  mutate(
    target_cla_enc_30d2 = ifelse(is.na(target_cla_enc_30d2), mean(TARGET_occurrence_M4.5, na.rm = TRUE), target_cla_enc_30d2)
  )

#Para el target de clasificacion de 3.0, test
test_enc_cla <- test %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_cla_enc_30d = slide_index_dbl(
      .x = TARGET_occurrence_M3.0,               # Variable a promediar
      .i = date,                # Índice temporal
      .f = mean,                 # Función a aplicar (media)
      .before = days(30),        # Mirar 30 días hacia atrás
      .after = -1,               # CRÍTICO: -1 excluye el día actual para evitar fuga de datos
      na_rm = TRUE
    )
  ) %>%
  ungroup() %>%
  
  # 4. Opcional: Rellenar los NAs de los primeros días (cuando no hay historial)
  # usando la media global acumulativa (como en el ejemplo anterior) o un valor por defecto.
  mutate(
    target_cla_enc_30d = ifelse(is.na(target_cla_enc_30d), mean(TARGET_occurrence_M3.0, na.rm = TRUE), target_cla_enc_30d)
  )
#Para el target de clasificacion de 4.5
test_enc_cla2 <- test %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_cla_enc_30d2 = slide_index_dbl(
      .x = TARGET_occurrence_M4.5,               # Variable a promediar
      .i = date,                # Índice temporal
      .f = mean,                 # Función a aplicar (media)
      .before = days(30),        # Mirar 30 días hacia atrás
      .after = -1,               # CRÍTICO: -1 excluye el día actual para evitar fuga de datos
      na_rm = TRUE
    )
  ) %>%
  ungroup() %>%
  
  # 4. Opcional: Rellenar los NAs de los primeros días (cuando no hay historial)
  # usando la media global acumulativa (como en el ejemplo anterior) o un valor por defecto.
  mutate(
    target_cla_enc_30d2 = ifelse(is.na(target_cla_enc_30d2), mean(TARGET_occurrence_M4.5, na.rm = TRUE), target_cla_enc_30d2)
  )


#Selección de variables

vars_exlu <- c("TARGET_occurrence_M4.5", 
               "date","cell_id", "TARGET_mag_max_next_7d")

archivo1 <- train_enc_cla[train_enc_cla$date < as.Date("2002-01-01"), ]
archivo1<- archivo1[,-which(names(archivo1) %in% vars_exlu)]

archivo2 <- train_enc_cla[train_enc_cla$date < as.Date("2000-06-01"), ]
archivo2<- archivo2[,-which(names(archivo1) %in% vars_exlu)]


# -----------------------------------------------------
# STEPAIC
# -----------------------------------------------------
full<-glm(TARGET_occurrence_M3.0~.,data=archivo1,family = binomial(link = "logit"))
null<-glm(TARGET_occurrence_M3.0~1,data=archivo1,family = binomial(link = "logit"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = binomial(link = "logit"),trace=FALSE)

vec<-(names(selec1[[1]]))

length(vec)

dput(vec)

listconti1 <- setdiff(names(selec1[[1]]), "(Intercept)")

#c( "target_cla_enc_30d", "days_since_last_eq", 
#  "adj_roll30_energia", "roll30_energia", "roll30_mag_max", "lon", 
#  "eq_count", "lat", "lag1_eq_count", "roll30_eq_count", "main_depth", 
#  "adj_roll7_mag_max", "locationSource_ci", "magNst", "main_lat", 
#  "energia_acumulada_diaria", "roll7_eq_count", "is_converted_mag", 
#  "locationSource_us", "net_us", "adj_max_mag_x")

# STEPBIC

k_BIC = log(nrow(archivo1))

selec2 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = k_BIC)
listconti2 <- setdiff(names(selec2[[1]]), "(Intercept)")

dput(listconti2)

#c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
#  "roll30_energia", "roll30_mag_max", "lon", "eq_count", "lat", 
#  "lag1_eq_count")

source("funcion steprepetido binaria.R")
source("cruzadas avnnet y log binaria.R")

sinicio=12345
sfinal=12365
num_vars_rfe=2
lista <- c("eq_count", "energia_acumulada_diaria", "main_mag", "main_depth", 
           "main_lat", "main_lon", "magError", "depthError", "horizontalError", 
           "nst", "gap", "dmin", "rms", "magNst", "is_converted_mag", "lat_bin", 
           "lon_bin", "b_value_daily", "lag1_mag_max", "lag1_eq_count", 
           "energia_lag1", "roll7_eq_count", "roll30_eq_count", "roll7_mag_max", 
           "roll30_mag_max", "roll7_energia", "roll30_energia", "days_since_last_eq", 
            "lat", "lon", "adj_num_eqs_x", "adj_max_mag_x", 
           "adj_num_eqs_y", "adj_max_mag_y", "adj_roll7_eq_count", "adj_roll7_mag_max", 
           "adj_roll30_energia", "adj_roll30_eq_count", "adj_roll30_mag_max", 
           "locationSource_ci", "locationSource_ecx", "locationSource_empty", 
           "locationSource_nc", "locationSource_nn", "locationSource_pas", 
           "locationSource_ren", "locationSource_slc", "locationSource_us", 
           "locationSource_uu", "locationSource_uw", "magSource_ci", "magSource_ecx", 
           "magSource_empty", "magSource_nc", "magSource_nn", "magSource_pas", 
           "magSource_ren", "magSource_slc", "magSource_slm", "magSource_us", 
           "magSource_uu", "magSource_uw", "net_empty", "net_nc", "net_nn", 
           "net_us", "net_uu", "net_uw", "status_empty", "status_reviewed", 
           "target_cla_enc_30d")
# STEPWISE con k general
selec3 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = 15)
listconti3 <- setdiff(names(selec3[[1]]), "(Intercept)")

dput(listconti3)

#c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
#  "roll30_energia", "roll30_mag_max", "lon", "eq_count", "lat")

cat("Ejecutando STEPWISE repetido AIC...\n")

lis<-steprepetidobinaria(data=archivo1,vardep="TARGET_occurrence_M3.0",
                         listconti=lista,
                         sinicio=sinicio,sfinal=sfinal,porcen=0.8,criterio="AIC")
listconti4<-dput(lis[[2]][[1]])
dput(listconti4)

#c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
#  "roll30_eq_count", "eq_count", "roll30_energia", "roll30_mag_max", 
#  "lon", "lag1_eq_count", "lat", "main_depth", "locationSource_ci", 
#  "magNst", "adj_max_mag_x", "locationSource_us", "net_us", "adj_roll7_mag_max"
#)

lis<-steprepetidobinaria(data=archivo1,vardep="TARGET_occurrence_M3.0",
                         listconti=lista,
                         sinicio=sinicio,sfinal=sfinal,porcen=0.8,criterio="BIC")
listconti5<-dput(lis[[2]][[1]])
dput(listconti5)

#c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
#  "roll30_energia", "roll30_mag_max", "eq_count", "lon", "lat")

listconti6<-dput(lis[[2]][[2]])
dput(listconti6)

#c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
#  "roll30_energia", "roll30_mag_max", "lon", "eq_count", "lat", 
#  "lag1_eq_count")



cat("Ejecutando RFE...\n")
# RFE

x <- archivo2 %>% select(-c("TARGET_occurrence_M3.0"))
y <- archivo2$TARGET_occurrence_M3.0
y <- as.factor(y)
levels(y) <- c("No", "Yes")

num_vars_rfe <- ncol(x)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 4)
results <- rfe(x, y, sizes = c(1:num_vars_rfe), rfeControl = control)

cosa<-as.data.frame(results$results) 

listconti7 <- results$optVariables[1:6]
dput(listconti7)

#c("target_cla_enc_30d", "adj_roll30_eq_count", "lon", "roll30_eq_count", 
#  "lat", "adj_roll7_mag_max")


cosa<-as.data.frame(results$results)

# Resultados en gráfico
ggplot(cosa,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+ 
  scale_y_continuous(breaks = cosa$Accuracy) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")

cat("Ejecutando MMPC...\n")
# MMPC

t1 <- 0.05   # Filtro estándar (Permisivo)
t2 <- 0.01   # Filtro exigente (Estricto)
t3 <- 0.001  # Filtro de alta confianza (Ultra estricto)

max_k_mmpc <- 3

mmpc2 <- MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t1, test = "testIndLogistic")
listconti8 <- names(x[, c(mmpc2$selectedVars)])
dput(listconti8)

#c("target_cla_enc_30d", "days_since_last_eq", "lat", "eq_count")

mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t2, test = "testIndLogistic")
listconti9 <- names(x[, c(mmpc2$selectedVars)])
dput(listconti9)

#c("target_cla_enc_30d", "days_since_last_eq", "lat")

mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t3, test = "testIndLogistic")
listconti10 <- names(x[, c(mmpc2$selectedVars)])
cat("Fin MMPC...\n")  

dput(listconti10)

#c("target_cla_enc_30d", "days_since_last_eq", "lat")


#Boruta

library(Boruta)

set.seed(1234) # Fijamos semilla porque Random Forest tiene aleatoriedad
resultado_boruta <- Boruta(x, y, doTrace = 2, maxRuns = 100)

# Ver el resumen de lo que ha decidido
print(resultado_boruta)

boruta_final <- TentativeRoughFix(resultado_boruta)
print(boruta_final)

listconti11 <- getSelectedAttributes(boruta_final, withTentative = FALSE)
dput(listconti11)

#c("eq_count", "energia_acumulada_diaria", "main_mag", "main_depth", 
#  "main_lat", "main_lon", "magError", "depthError", "horizontalError", 
#  "nst", "gap", "dmin", "rms", "magNst", "lat_bin", "lon_bin", 
#  "lag1_mag_max", "energia_lag1", "roll7_eq_count", "roll30_eq_count", 
#  "roll7_mag_max", "roll30_mag_max", "roll7_energia", "roll30_energia", 
#  "days_since_last_eq", "lat", "lon", "adj_max_mag_x", "adj_max_mag_y", 
#  "adj_roll7_eq_count", "adj_roll7_mag_max", "adj_roll30_energia", 
#  "adj_roll30_eq_count", "adj_roll30_mag_max", "locationSource_ci", 
#  "magSource_ci", "target_cla_enc_30d")


#Mutual Information

library(infotheo)

x_discretizada <- discretize(x, disc = "equalwidth", nbins = 10)

valores_mi <- sapply(x_discretizada, function(columna) {
  mutinformation(columna, y)
})

# Convertimos el resultado en un dataframe ordenado de mayor a menor importancia
ranking_mi <- data.frame(
  Variable = names(valores_mi),
  Informacion_Mutua = valores_mi
) %>% 
  arrange(desc(Informacion_Mutua))

# Ver las variables con más información mutua
print(head(ranking_mi, 20))

# Filtrar variables que aporten un mínimo de información
listconti12 <- ranking_mi %>% 
  filter(Informacion_Mutua > 0.01) %>% 
  pull(Variable)

dput(listconti12)

#c("target_cla_enc_30d", "roll30_eq_count", "roll30_energia", 
#  "roll30_mag_max", "roll7_energia", "roll7_mag_max")


#Lasso

library(glmnet)

x_matrix <- model.matrix(~ . - 1, data = x)

cv_lasso <- cv.glmnet(x_matrix, y, family = "binomial", alpha = 1, nfolds = 4)

# Ver el gráfico de cómo varía el error según el número de variables vivas
plot(cv_lasso)

coeficientes_optimos <- coef(cv_lasso, s = cv_lasso$lambda.1se)

# Convertir a una matriz limpia para poder visualizarla
matriz_coef <- as.matrix(coeficientes_optimos)

# Filtrar las variables que sobrevivieron (coeficiente diferente de cero)
# Excluimos el "(Intercept)" de nuestra lista de variables predictoras
variables_lasso <- rownames(matriz_coef)[matriz_coef[, 1] != 0]
variables_lasso <- setdiff(variables_lasso, "(Intercept)")

# Ver tu lista final de variables seleccionadas
listconti13  <- variables_lasso
dput(listconti13)

#c("eq_count", "main_depth", "main_lat", "magError", "depthError", 
#  "rms", "magNst", "lat_bin", "lag1_eq_count", "roll30_eq_count", 
#  "roll7_mag_max", "roll30_mag_max", "roll30_energia", "days_since_last_eq", 
#  "lat", "lon", "adj_roll7_eq_count", "adj_roll7_mag_max", "adj_roll30_eq_count", 
#  "adj_roll30_mag_max", "locationSource_ci", "locationSource_nn", 
#  "magSource_ci", "magSource_nn", "net_us", "target_cla_enc_30d"
#)


#Random forest

library(randomForest)

modelo_rf <- randomForest(
  x = x,
  y = y,
  ntree = 200,        # Número de árboles
  importance = TRUE,  # << CRUCIAL: Calcula la importancia de las variables
  keep.forest = FALSE # Ahorra memoria RAM ya que solo queremos la importancia
)

# Extraer la matriz de importancia
importancia_matriz <- as.data.frame(importance(modelo_rf))

# Creamos un dataframe limpio y ordenando de mayor a menor importancia
ranking_rf <- data.frame(
  Variable = rownames(importancia_matriz),
  Importancia = importancia_matriz$MeanDecreaseAccuracy
) %>% 
  arrange(desc(Importancia))

# Ver las 20 variables más importantes
print(head(ranking_rf, 20))

# Graficar el ranking de forma visual (Muestra las variables más críticas arriba)
varImpPlot(modelo_rf, type = 1, scale = FALSE, main = "Importancia de Variables (Random Forest)")


variables_rf_final <- ranking_rf$Variable[1:15]

listconti14 <- variables_rf_final
dput(listconti14)

#c("target_cla_enc_30d", "roll30_mag_max", "lat", "adj_roll30_eq_count", 
#  "lon", "adj_roll7_eq_count", "roll30_energia", "roll30_eq_count", 
#  "days_since_last_eq", "adj_roll30_energia", "adj_roll7_mag_max", 
#  "roll7_mag_max", "adj_roll30_mag_max", "roll7_energia", "roll7_eq_count"
#)


#XGBoost

library(xgboost)

y_numeric <- as.numeric(as.factor(archivo2$TARGET_occurrence_M3.0)) - 1

modelo_xgb <- xgboost(
  data = x_matrix,
  label = y_numeric,
  nrounds = 100,                    # Número de iteraciones/árboles
  objective = "binary:logistic",    # Clasificación binaria
  verbose = 0                       # Para que no llene la consola de logs matemáticos
)

# 1. Calcular la matriz de importancia
importancia_xgb <- xgb.importance(feature_names = colnames(x_matrix), model = modelo_xgb)

# 2. Convertir a dataframe ordenado por Ganancia (Gain)
ranking_xgb <- as.data.frame(importancia_xgb) %>% 
  arrange(desc(Gain))

# Ver las 20 variables más importantes
print(head(ranking_xgb, 20))

# 3. Graficar el ranking de forma nativa y visual
xgb.plot.importance(importance_matrix = importancia_xgb, top_n = 20, main = "Top 20 Variables - XGBoost")

variables_xgb_95 <- ranking_xgb %>% 
  mutate(Gain_Acumulado = cumsum(Gain)) %>% 
  filter(Gain_Acumulado <= 0.95) %>% 
  pull(Feature)

listconti15 <- variables_xgb_95
dput(listconti15)

#c("target_cla_enc_30d", "roll30_mag_max", "roll30_eq_count", 
#  "roll30_energia", "days_since_last_eq", "energia_acumulada_diaria", 
#  "adj_roll30_energia", "adj_roll30_eq_count", "adj_roll7_mag_max", 
#  "adj_roll30_mag_max", "adj_roll7_eq_count", "roll7_energia", 
#  "lat", "main_lat")




# -----------------------------------------------------
# COMPARACION VIA CV REPETIDA Y BOXPLOT
# -----------------------------------------------------

library(doParallel)

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

source("cruzadas avnnet y log binaria.R")

data<-archivo1

data$TARGET_occurrence_M3.0 <- as.factor(data$TARGET_occurrence_M3.0)
levels(data$TARGET_occurrence_M3.0) <- c("No", "Yes")

#Es necesario añadir penalización por coste para obtener mejor sensibilidad
conteo <- table(data$TARGET_occurrence_M3.0)
peso_multiplicador <- conteo["No"] / conteo["Yes"]
pesos_vector <- ifelse(data$TARGET_occurrence_M3.0 == "Yes", peso_multiplicador, 1)

medias1<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti1,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias1$modelo="STEPAIC"

medias2<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti2,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias2$modelo="STEPBIC"


medias3<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti3,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo="STEPk15"


medias4<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti4,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias4$modelo="STEPrepAIC1"

medias5<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti5,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="STEPrepBIC1"

medias6<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti6,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias6$modelo="STEPrepBIC2"

medias7<-cruzadalogistica(data=data,
                    vardep="TARGET_occurrence_M3.0",listconti=listconti7,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias7$modelo="RFE"



medias8<-cruzadalogistica(data=data,
                          vardep="TARGET_occurrence_M3.0",listconti=listconti8,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias8$modelo="MMPC-0.05"


medias9<-cruzadalogistica(data=data,
                          vardep="TARGET_occurrence_M3.0",listconti=listconti9,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias9$modelo="MMPC-0.01"

medias10<-cruzadalogistica(data=data,
                          vardep="TARGET_occurrence_M3.0",listconti=listconti10,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias10$modelo="MMPC-0.001"

medias11<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti11,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias11$modelo="Boruta"

medias12<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti12,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias12$modelo="MI"

medias13<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti13,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias13$modelo="Lasso"

medias14<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti14,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias14$modelo="RF"


medias15<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti15,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias15$modelo="XGB"

stopCluster(cl)
registerDoSEQ()


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9,medias10,
              medias11,medias12,medias13,medias14,medias15)


orden <- c("STEPAIC", "STEPBIC","STEPk15", "STEPrepAIC1", "STEPrepBIC1", 
           "STEPrepBIC2", "RFE", "MMPC-0.05", "MMPC-0.01", "MMPC-0.001","Boruta"
           , "MI","Lasso","RF", "XGB")
listas_conti <- list(listconti1, listconti2, listconti3, listconti4, 
                     listconti5, listconti6, listconti7, listconti8, listconti9,listconti10
                     ,listconti11, listconti12,listconti13,listconti14,listconti15)
n_vars <- sapply(listas_conti, length)


union1$modelo <- factor(union1$modelo, levels = orden)  


par(cex.axis=0.8, las=2)

boxplot(data=union1,col="pink",auc~modelo,main="AUC")
text(x = 1:length(n_vars),
     y = aggregate(auc ~ modelo, union1, max)$auc ,labels = n_vars,
     col = "red",  font = 2, cex =1  )


boxplot(data=union1,col="pink",sensi~modelo,main="SENSITIVIDAD")
text(x = 1:length(n_vars),
     y = aggregate(sensi ~ modelo, union1, max)$sensi ,labels = n_vars,
     col = "red",  font = 2, cex =1  )


boxplot(data=union1,col="pink",tasa~modelo,main="TASA DE FALLOS")
text(x = 1:length(n_vars),
     y = aggregate(tasa ~ modelo, union1, max)$tasa ,labels = n_vars,
     col = "red",  font = 2, cex =1  )

source("funcion crear_tabla_modelos_binaria.R")
source("funcion crear_tabla_modelos presencia.R")


modelos <- list(
  "STEPAIC"     = listconti1,
  "STEPBIC"     = listconti2,
  "STEPk15"     = listconti3,
  "STEPrepAIC1" = listconti4,
  "STEPrepBIC1" = listconti5,
  "STEPrepBIC2" = listconti6,
  "RFE"         = listconti7,
  "MMPC-0.05"   = listconti8,
  "MMPC-0.01"   = listconti9,
  "MMPC-0.001"  = listconti10,
  "Boruta"      = listconti11,
  "MI"          = listconti12,
  "Lasso"       = listconti13,
  "RF"          = listconti14,
  "XGB"         = listconti15
)

crear_tabla_modelos_binaria_auc(union1, modelos)
crear_tabla_modelos_binaria_tasa(union1, modelos)
tabla <- crear_tabla_presencia(union1, modelos)
htmltools::save_html(tabla, "tabla_presencia_variables.html")



media <- mean(union1[union1$modelo== "STEPk15",]$sensi, na.rm = TRUE)  
desviacion <- sd(union1[union1$modelo== "STEPk15",]$sensi, na.rm = TRUE)

# Ver resultados
media
desviacion

listconti16 <- c("target_cla_enc_30d", "days_since_last_eq", "lat",
                 "roll30_energia", "roll30_mag_max")

listconti17 <- c("target_cla_enc_30d", "days_since_last_eq", "lat",
                 "roll30_energia", "roll30_mag_max", "adj_roll30_energia",
                 "lon")

listconti18 <- c("target_cla_enc_30d", "days_since_last_eq", "lat",
                 "roll30_energia", "roll30_mag_max", "adj_roll30_energia",
                 "lon", "eq_count", "roll30_eq_count")



medias16<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti16,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias16$modelo="First5"

medias17<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti17,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias17$modelo="First7"


medias18<-cruzadalogistica(data=data,
                           vardep="TARGET_occurrence_M3.0",listconti=listconti18,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias18$modelo="Mas_mitad"


union2<-rbind(medias2,medias3,medias16,medias17,medias18)


orden <- c( "STEPBIC","STEPk15", "First5","First7","Mas_mitad")
listas_conti <- list( listconti2, listconti3,
                     listconti16,listconti17,listconti18)
n_vars <- sapply(listas_conti, length)


union2$modelo <- factor(union2$modelo, levels = orden)  

boxplot(data=union2,col="pink",auc~modelo,main="AUC")
text(x = 1:length(n_vars),
     y = aggregate(auc ~ modelo, union2, max)$auc ,labels = n_vars,
     col = "red",  font = 2, cex =1  )



boxplot(data=union2,col="pink",sensi~modelo,main="SENSITIVIDAD")
text(x = 1:length(n_vars),
     y = aggregate(sensi ~ modelo, union2, max)$sensi ,labels = n_vars,
     col = "red",  font = 2, cex =1  )


boxplot(data=union2,col="pink",tasa~modelo,main="TASA DE FALLOS")
text(x = 1:length(n_vars),
     y = aggregate(tasa ~ modelo, union2, max)$tasa ,labels = n_vars,
     col = "red",  font = 2, cex =1  )



#Los conjuntos seleccionados
listconti2 <- c("target_cla_enc_30d", "days_since_last_eq", "adj_roll30_energia", 
                 "roll30_energia", "roll30_mag_max", "lon", "eq_count", "lat", 
                 "lag1_eq_count")


listconti17 <- c("target_cla_enc_30d", "days_since_last_eq", "lat",
                 "roll30_energia", "roll30_mag_max", "adj_roll30_energia",
                 "lon")






#Tuneo sobre redes neuronales

library(lubridate)
library(tidymodels)
library(rsample)


set.seed(123)

datos_tuneo <- train_enc_cla %>%
  filter(date >= as.Date("2018-01-01") & date < as.Date("2019-01-01"))

datos_tuneo$TARGET_occurrence_M3.0 <- as.factor(datos_tuneo$TARGET_occurrence_M3.0)
levels(datos_tuneo$TARGET_occurrence_M3.0) <- c("No", "Yes")

datos_tuneo1 <- datos_tuneo[, c("TARGET_occurrence_M3.0", "date", listconti2)]
datos_tuneo2 <- datos_tuneo[, c("TARGET_occurrence_M3.0", "date", listconti17)]
library(hardhat)
library(dplyr)


conteo <- table(datos_tuneo1$TARGET_occurrence_M3.0)
peso_no <- 1
peso_yes <- conteo["No"] / conteo["Yes"]

datos_tuneo1 <- datos_tuneo1 %>%
  mutate(
    # Asignamos el peso según la clase
    peso_obs = ifelse(TARGET_occurrence_M3.0 == "Yes", peso_yes, peso_no),
    # Lo transformamos al formato que exige tidymodels
    peso_obs = importance_weights(peso_obs) 
  )

datos_tuneo2 <- datos_tuneo2 %>%
  mutate(
    # Asignamos el peso según la clase
    peso_obs = ifelse(TARGET_occurrence_M3.0 == "Yes", peso_yes, peso_no),
    # Lo transformamos al formato que exige tidymodels
    peso_obs = importance_weights(peso_obs) 
  )


N_obs <- nrow(datos_tuneo)
N_vars <- length(listconti2)

max_size_teorico <- floor(((40000 / 20) - 1) / (7 + 2))

cat("Observaciones en tuneo:", N_obs, "\n")
cat("Máximo de neuronas ocultas permitidas (regla 20 obs):", max_size_teorico, "\n")


grid_nodos <- c(1,3,5,7,10, 30, 60, 100) # Ej: 1, 3, 5, 7...
grid_decay <- c(0.0001, 0.001, 0.01, 0.1)
grid_nnet <- expand.grid(size = grid_nodos, decay = grid_decay)

grid_maxit <- c(100, 300, 500, 2000) 

library(caret)
library(dplyr)
library(nnet)



ctrl_temporal <- trainControl(
  method = "timeslice",
  initialWindow = 25000, 
  horizon = 5000,       
  skip = 5000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = twoClassSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  allowParallel = TRUE
)

library(parallel)
library(doParallel)

# 1. Detectar cuántos núcleos tiene tu computadora
num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")

datos_sin_metadatos <- datos_tuneo1[, !names(datos_tuneo1) %in% c("date", "peso_obs")]

resultados_tuneo <- list()

for(m_it in grid_maxit) {
  cat("\nEntrenando modelos para maxit =", m_it, "...\n")
  
  tiempo_bucle_inicio <- Sys.time()
  
  set.seed(123)
  modelo <- train(
    TARGET_occurrence_M3.0~ ., 
    data = datos_sin_metadatos,
    method = "nnet",
    trControl = ctrl_temporal,
    tuneGrid = grid_nnet,
    weights = datos_tuneo1$peso_obs, # Usar los pesos que calculamos
    metric = "ROC",                  # Optimizar en base a la curva ROC (AUC)
    maxit = m_it,                    # Aplicar aquí el maxit del bucle
    trace = FALSE,                   # Para que no imprima iteraciones infinitas por consola
    MaxNWts = 2000                  # Prevenir error interno de limitación de pesos
  )
  
  resultados_tuneo[[paste0("maxit_", m_it)]] <- modelo
  
  tiempo_bucle_fin <- Sys.time()
  
  # 3. Calcular la diferencia de tiempo para este bloque
  tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")
  
  cat("\n✅ Terminado maxit =", m_it, "\n")
  cat("Tiempo empleado para este bloque:", round(tiempo_diferencia, 2), "minutos\n")
}

stopCluster(cl)
registerDoSEQ()


num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")

datos_sin_metadatos2 <- datos_tuneo2[, !names(datos_tuneo2) %in% c("date", "peso_obs")]

resultados_tuneo2 <- list()

for(m_it in grid_maxit) {
  cat("\nEntrenando modelos para maxit =", m_it, "...\n")
  
  tiempo_bucle_inicio <- Sys.time()
  
  set.seed(123)
  modelo <- train(
    TARGET_occurrence_M3.0~ ., 
    data = datos_sin_metadatos2,
    method = "nnet",
    trControl = ctrl_temporal,
    tuneGrid = grid_nnet,
    weights = datos_tuneo2$peso_obs, # Usar los pesos que calculamos
    metric = "ROC",                  # Optimizar en base a la curva ROC (AUC)
    maxit = m_it,                    # Aplicar aquí el maxit del bucle
    trace = FALSE,                   # Para que no imprima iteraciones infinitas por consola
    MaxNWts = 2000                  # Prevenir error interno de limitación de pesos
  )
  
  resultados_tuneo2[[paste0("maxit_", m_it)]] <- modelo
  
  tiempo_bucle_fin <- Sys.time()
  
  # 3. Calcular la diferencia de tiempo para este bloque
  tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")
  
  cat("\n✅ Terminado maxit =", m_it, "\n")
  cat("Tiempo empleado para este bloque:", round(tiempo_diferencia, 2), "minutos\n")
}

stopCluster(cl)
registerDoSEQ()

library(dplyr)
library(purrr)
library(ggplot2)


df_grafico <- bind_rows(
  resultados_tuneo$maxit_100$results %>% mutate(itera = 100),
  resultados_tuneo$maxit_300$results %>% mutate(itera = 300),
  resultados_tuneo$maxit_500$results %>% mutate(itera = 500),
  resultados_tuneo$maxit_2000$results %>% mutate(itera = 2000)
)

df_grafico$Accuracy <- (0.03 * df_grafico$Sens) + 
  ((0.97) * df_grafico$Spec)
ggplot(df_grafico, aes(x = factor(itera), y = ROC, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "AUC", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()

ggplot(df_grafico, aes(x = factor(itera), y = Sens, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "SENSITIVIDAD", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()


ggplot(df_grafico, aes(x = factor(itera), y = Accuracy, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "Accuracy", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()




#Size 3,5 y decay 0.1 y 100 itera, size 3, decay 0.01 y 300 itera
df_grafico

df_grafico_ordenado <- df_grafico %>%
  arrange(desc(ROC))
df_grafico_ordenado

df_grafico_ordenado2 <- df_grafico %>%
  arrange(desc(Sens))

df_grafico_ordenado

df_grafico_ordenado[c(1,2,6),]

df_grafico3 <- bind_rows(
  resultados_tuneo2$maxit_100$results %>% mutate(itera = 100),
  resultados_tuneo2$maxit_300$results %>% mutate(itera = 300),
  resultados_tuneo2$maxit_500$results %>% mutate(itera = 500),
  resultados_tuneo2$maxit_2000$results %>% mutate(itera = 2000)
)

df_grafico3$Accuracy <- (0.03 * df_grafico3$Sens) + 
  ((0.97) * df_grafico3$Spec)

ggplot(df_grafico3, aes(x = factor(itera), y = ROC, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "AUC", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()

ggplot(df_grafico3, aes(x = factor(itera), y = Sens, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "SENSITIVIDAD", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()

ggplot(df_grafico3, aes(x = factor(itera), y = Accuracy, color = factor(decay), size = factor(size))) +
  # Usamos position_dodge para que ni los colores ni los tamaños se encimen
  geom_point(position = position_dodge(width = 0.6)) +
  
  # Forzamos que los puntos mantengan un rango de tamaños visible y estético
  scale_size_manual(values = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  
  labs(
    x = "factor(itera)",
    y = "SENSITIVIDAD", # O 'RMSE' según tu métrica
    color = "factor(decay)",
    size = "factor(size)"
  ) +
  theme_grey()


df_grafico3

df_grafico_ordenado3 <- df_grafico3 %>%
  arrange(desc(ROC))

df_grafico_ordenado3[c(4,9,38),]

df_grafico_ordenado3 <- df_grafico3 %>%
  arrange(desc(Sens))

df_grafico_ordenado3


#Size 3,5 y decay 0.1 y 100 itera, size 3, decay 0.01 y 300 itera


library(dplyr)
train_cla <- train_enc_cla 

train_cla$TARGET_occurrence_M3.0 <- as.factor(train_cla$TARGET_occurrence_M3.0)
levels(train_cla$TARGET_occurrence_M3.0) <- c("No", "Yes")
train_cla$TARGET_occurrence_M3.0 <- factor(train_cla$TARGET_occurrence_M3.0, levels = c("Yes", "No"))
train_cla <- train_cla[, c("TARGET_occurrence_M3.0", "date", listconti2)]

conteo <- table(train_cla$TARGET_occurrence_M3.0)
peso_no <- 1
peso_yes <- conteo["No"] / conteo["Yes"]

train_cla <- train_cla %>%
  mutate(
    # Asignamos el peso según la clase
    peso_obs = ifelse(TARGET_occurrence_M3.0 == "Yes", peso_yes, peso_no),
    # Lo transformamos al formato que exige tidymodels
    peso_obs = importance_weights(peso_obs) 
  )

train_sin_metadatos <- train_cla[, !names(train_cla) %in% c("date", "peso_obs")]


ctrl_temporal <- trainControl(
  method = "timeslice",
  initialWindow = 500000, 
  horizon = 50000,       
  skip = 50000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = twoClassSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  returnResamp = "all",
  allowParallel = TRUE
)



# Creamos una lista para almacenar los 3 modelos
modelos_finales <- list()

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)

cat("\nEntrenando el modelo 1")
# --- Modelo 1: Size 3, decay 0.1, 100 iteraciones ---
set.seed(123) # Fijar semilla para comparabilidad
modelos_finales[["Mod1_s3_d0.1_100it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla$peso_obs,
  tuneGrid = expand.grid(size = 3, decay = 0.1), # Grid específico
  metric = "ROC", maxit = 100, trace = FALSE, MaxNWts = 2000
)

cat("\nEntrenando el modelo 2")
# --- Modelo 2: Size 5, decay 0.1, 100 iteraciones ---
set.seed(123)
modelos_finales[["Mod2_s5_d0.1_100it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla$peso_obs,
  tuneGrid = expand.grid(size = 5, decay = 0.1),
  metric = "ROC", maxit = 100, trace = FALSE, MaxNWts = 2000
)
cat("\nEntrenando el modelo 3")
# --- Modelo 3: Size 3, decay 0.01, 300 iteraciones ---
set.seed(123)
modelos_finales[["Mod3_s3_d0.01_300it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla$peso_obs,
  tuneGrid = expand.grid(size = 3, decay = 0.01),
  metric = "ROC", maxit = 300, trace = FALSE, MaxNWts = 2000
)

stopCluster(cl)
registerDoSEQ()

modelos_finales$Mod1_s3_d0.1_100it
modelos_finales$Mod2_s5_d0.1_100it
modelos_finales$Mod3_s3_d0.01_300it

resultados_resamples <- resamples(modelos_finales)

# 2. Resumen estadístico (opcional, para ver valores)
summary(resultados_resamples)

df_resultados <- as.data.frame(resultados_resamples$values)

# Transformar a formato largo
df_long <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "ROC") # Filtramos solo la métrica ROC

df_long2 <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "Sens") # Filtramos solo la métrica ROC

df_long3 <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "Spec") # Filtramos solo la métrica ROC

df_long4 <- df_long2
df_long4$value1 <- df_long3$value
df_long4$Accuracy <- (0.03 * df_long4$value) + 
  ((0.97) * df_long4$value1)
# Crear gráfico
ggplot(df_long, aes(x = Modelo, y = value, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución del AUC",
       y = "AUC",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_long2, aes(x = Modelo, y = value, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Sensitividad",
       y = "Sens",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_long4, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Accuracy",
       y = "Accuracy",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


modelos_finales$Mod3_s3_d0.01_300it$results
modelos_finales$Mod2_s5_d0.1_100it$results

#Para listconti17, size 3 y 5 decay 0.1 y 100 iteraciones, y size 3 decay 0.001, 300 iter

train_cla2 <- train_enc_cla 

train_cla2$TARGET_occurrence_M3.0 <- as.factor(train_cla2$TARGET_occurrence_M3.0)
levels(train_cla2$TARGET_occurrence_M3.0) <- c("No", "Yes")
train_cla2$TARGET_occurrence_M3.0 <- factor(train_cla2$TARGET_occurrence_M3.0, levels = c("Yes", "No"))
train_cla2 <- train_cla2[, c("TARGET_occurrence_M3.0", "date", listconti17)]

conteo <- table(train_cla2$TARGET_occurrence_M3.0)
peso_no <- 1
peso_yes <- conteo["No"] / conteo["Yes"]

train_cla2 <- train_cla2 %>%
  mutate(
    # Asignamos el peso según la clase
    peso_obs = ifelse(TARGET_occurrence_M3.0 == "Yes", peso_yes, peso_no),
    # Lo transformamos al formato que exige tidymodels
    peso_obs = importance_weights(peso_obs) 
  )

train_sin_metadatos2 <- train_cla2[, !names(train_cla2) %in% c("date", "peso_obs")]


ctrl_temporal <- trainControl(
  method = "timeslice",
  initialWindow = 500000, 
  horizon = 50000,       
  skip = 50000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = twoClassSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  returnResamp = "all",
  allowParallel = TRUE
)

ctrl_temporal1 <- trainControl(
  method = "timeslice",
  initialWindow = 500000, 
  horizon = 50000,       
  skip = 50000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = prSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  returnResamp = "all",
  allowParallel = TRUE
)


# Creamos una lista para almacenar los 3 modelos
modelos_finales2 <- list()

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)

cat("\nEntrenando el modelo 1")
# --- Modelo 1: Size 3, decay 0.1, 100 iteraciones ---
set.seed(123) # Fijar semilla para comparabilidad
modelos_finales2[["Mod1_s3_d0.1_100it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos2, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla2$peso_obs,
  tuneGrid = expand.grid(size = 3, decay = 0.1), # Grid específico
  metric = "ROC", maxit = 100, trace = FALSE, MaxNWts = 2000
)

cat("\nEntrenando el modelo 2")
# --- Modelo 2: Size 5, decay 0.1, 100 iteraciones ---
set.seed(123)
modelos_finales2[["Mod2_s5_d0.1_100it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos2, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla2$peso_obs,
  tuneGrid = expand.grid(size = 5, decay = 0.1),
  metric = "ROC", maxit = 100, trace = FALSE, MaxNWts = 2000
)
cat("\nEntrenando el modelo 3")
# --- Modelo 3: Size 3, decay 0.01, 300 iteraciones ---
set.seed(123)
modelos_finales2[["Mod3_s3_d0.001_300it"]] <- train(
  TARGET_occurrence_M3.0 ~ ., data = train_sin_metadatos2, method = "nnet",
  trControl = ctrl_temporal, weights = train_cla2$peso_obs,
  tuneGrid = expand.grid(size = 3, decay = 0.001),
  metric = "ROC", maxit = 300, trace = FALSE, MaxNWts = 2000
)

stopCluster(cl)
registerDoSEQ()

modelos_finales2$Mod1_s3_d0.1_100it
modelos_finales2$Mod2_s5_d0.1_100it
modelos_finales2$Mod3_s3_d0.001_300it


resultados_resamples <- resamples(modelos_finales2)

# 2. Resumen estadístico (opcional, para ver valores)
summary(resultados_resamples)

df_resultados <- as.data.frame(resultados_resamples$values)

# Transformar a formato largo
df_long <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "ROC") # Filtramos solo la métrica ROC

df_long2 <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "Sens") # Filtramos solo la métrica ROC

df_long3 <- df_resultados %>%
  pivot_longer(cols = -Resample, 
               names_to = c("Modelo", "Metrica"), 
               names_sep = "~") %>%
  filter(Metrica == "Spec") # Filtramos solo la métrica ROC

df_long4 <- df_long2
df_long4$value1 <- df_long3$value
df_long4$Accuracy <- (0.03 * df_long4$value) + 
  ((0.97) * df_long4$value1)
# Crear gráfico

# Crear gráfico
ggplot(df_long, aes(x = Modelo, y = value, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución del AUC ROC",
       y = "AUC ROC",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_long2, aes(x = Modelo, y = value, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Sensitividad",
       y = "Sens",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_long4, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Accuracy",
       y = "Accuracy",
       x = "Configuración del Modelo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



modelos_finales2$Mod3_s3_d0.001_300it$results



test_cla <- test_enc_cla 

test_cla$TARGET_occurrence_M3.0 <- as.factor(test_cla$TARGET_occurrence_M3.0)
levels(test_cla$TARGET_occurrence_M3.0) <- c("No", "Yes")
test_cla$TARGET_occurrence_M3.0 <- factor(test_cla$TARGET_occurrence_M3.0, levels = c("Yes", "No"))

test_cla <- test_cla[, c("TARGET_occurrence_M3.0", listconti2)]

predicciones <- predict(modelos_finales$Mod3_s3_d0.01_300it, newdata = test_cla)

matriz <- confusionMatrix(data = predicciones, reference = test_cla$TARGET_occurrence_M3.0)

matriz$table
print(matriz$byClass)
cm <- matriz$table



df_cm <- as.data.frame(cm)
df_cm

ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Red (escala log)")

matriz_red$table

cm_df <- as.data.frame(matriz$table)
colnames(cm_df) <- c("Realidad", "Prediccion", "Frecuencia")

# --- Visualización Diferenciada (con Escala Logarítmica) ---
ggplot(cm_df, aes(x = Prediccion, y = Realidad, fill = Frecuencia)) +
  geom_tile(color = "white") + # Añade borde blanco para separar casillas
  
  # AÑADIR TEXTO: Texto blanco grande y bold para números legibles
  geom_text(aes(label = Frecuencia), color = "white", size = 10, fontface = "bold") +
  
  # LA CLAVE: Usar escala de color divergente y transformación logarítmica
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", 
                       midpoint = log10(max(cm_df$Frecuencia)/100), # Ajuste opcional para el blanco
                       trans = "log10", # <--- TRANSFORMA LA ESCALA A LOGARÍTMICA
                       na.value = "white", # Manejar frecuencias de 0 (log(0) es indefinido)
                       # Formatear la leyenda para que muestre números legibles, no logaritmos
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  
  theme_minimal() +
  labs(title = "Matriz de Confusión Red Test",
       subtitle = "Escala de color logarítmica para compensar desbalance",
       x = "Predicción del Modelo", y = "Realidad Observada") +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 14))


probabilidades_red <- predict(modelos_finales$Mod3_s3_d0.01_300it, newdata = test_cla, type = "prob")

library(pROC)
roc_curva_red <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_red[, "Yes"])
plot(roc_curva_red)

roc_df_red <- data.frame(
  fpr = 1 - roc_curva_red$specificities,
  tpr = roc_curva_red$sensitivities
)

# O usa coords para más control
roc_df_red <- coords(roc_curva_red, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_red <- round(auc(roc_curva_red), 3)

ggplot(roc_df_red, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_red),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

# Extraemos solo la columna de la probabilidad de la clase minoritaria
prob_yes_red <- probabilidades_red[, "Yes"]

# 2. Probar diferentes puntos de corte (ej: 0.1, 0.2, 0.3...)
# Si la probabilidad es mayor a 0.2, predecimos "Positivo", sino "Negativo"

proporcion_esperada <- 0.03

# 3. Calcular en qué punto de probabilidad se hace el corte temporal
# Si queremos el top 5%, buscamos el cuantil 95% (1 - 0.05 = 0.95)
umbral_corte <- quantile(prob_yes_red, probs = 1 - proporcion_esperada)
# 4. Clasificar basándonos en ese nuevo umbral
predicciones_topX_red <- ifelse(prob_yes_red >= umbral_corte, "Yes", "No")
predicciones_topX_red <- as.factor(predicciones_topX_red)
matriz_red <- confusionMatrix(data = predicciones_topX_red, reference = test_cla$TARGET_occurrence_M3.0)
matriz_red$table

matriz$byClass
matriz_red$byClass


cm_df_red <- as.data.frame(matriz_red$table)
ggplot(cm_df_red, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Red (escala log)")



#RF y baggin

ctrl_temporal_tuneo <- trainControl(
  method = "timeslice",
  initialWindow = 25000, 
  horizon = 5000,       
  skip = 5000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = twoClassSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  allowParallel = TRUE
)

ctrl_temporal_tuneo1 <- trainControl(
  method = "timeslice",
  initialWindow = 25000, 
  horizon = 5000,       
  skip = 5000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = prSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  allowParallel = TRUE
)

#Elección ntree, elección = 200
library(randomForest)

rf_oob <- randomForest(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  ntree = 1000,
  mtry = 9
)

plot(rf_oob$auc.rate[, "OOB"],
     type = "l",
     lwd = 2,
     xlab = "Número de árboles",
     ylab = "Error OOB",
     main = "Estabilidad OOB vs ntree")

#Tunear sampsize

grid_fracciones <- c(0.1, 0.3, 0.5, 0.7, 1.0)

grid_fijo_rf <- expand.grid(
  mtry = 9, # Heurística estándar 
  splitrule = "extratrees",         # ExtraTrees suele ir bien en desbalanceos
  min.node.size = 10                # Valor normal inicial
)

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")

resultados_sampsize <- list()

for(frac in grid_fracciones) {
  cat("\nEntrenando Fase 1 para sample.fraction =", frac, "...\n")
  
  set.seed(123)
  modelo_fase1 <- train(
    TARGET_occurrence_M3.0 ~ ., 
    data = datos_sin_metadatos,
    method = "ranger",
    trControl = ctrl_temporal_tuneo,
    tuneGrid = grid_fijo_rf,
    weights = datos_tuneo1$peso_obs,           # Pesos para desbalanceo
    metric = "ROC",
    sample.fraction = frac,           # AQUÍ VA EL SAMPSIZE EN FRACCIÓN
    max.depth = 10,                   # Fijo por ahora
    num.trees = 100,                  # Un número prudente para tuneo
    importance = 'impurity',
    num.threads = 1
  )
  
  resultados_sampsize[[paste0("frac_", frac)]] <- modelo_fase1
}

stopCluster(cl)
registerDoSEQ()

perf_all <- lapply(names(resultados_sampsize), function(nm) {
  modelo <- resultados_sampsize[[nm]]
  
  data.frame(
    frac = as.numeric(gsub("frac_", "", nm)),
    ROC  = modelo$resample$ROC,
    Sens = modelo$resample$Sens,
    Spec = modelo$resample$Spec
  )
}) %>% bind_rows()

perf_all$Accuracy <- 0.03* perf_all$Sens + 0.97 * perf_all$Spec
ggplot(perf_all, aes(x = factor(frac), y = ROC)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "AUC (ROC) por sample.fraction",
    x = "sample.fraction",
    y = "AUC"
  ) +
  theme_minimal()

ggplot(perf_all, aes(x = factor(frac), y = Sens)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Sensibilidad por sample.fraction",
    x = "sample.fraction",
    y = "Sensibilidad"
  ) +
  theme_minimal()

ggplot(perf_all, aes(x = factor(frac), y = Accuracy)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Accuracy por sample.fraction",
    x = "sample.fraction",
    y = "Accuracy"
  ) +
  theme_minimal()

ggplot(perf_all, aes(x = factor(frac), y = Spec)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Spec por sample.fraction",
    x = "sample.fraction",
    y = "Spec"
  ) +
  theme_minimal()


#Se elige 0.3

#Tuneo de otros parametros
grid_ranger <- expand.grid(
  mtry = 2:9, # El último es Bagging
  splitrule = c("gini", "extratrees"), # Opcional: meter extratrees suele mejorar regularización
  min.node.size = c(1, 5, 10, 20)      # Profundidad a nivel local (freno final de la hoja)
)

grid_externa <- expand.grid(
  max_depth = c(0, 5, 15, 30),         
  sample_fraction = 0.3
)

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


# 5. Bucle de Entrenamiento 
resultados_tuneo_rf <- list()

# Bucle externo replicando tu modelo, pero iterando sobre la grilla externa 
for(i in 1:nrow(grid_externa)) {
  m_depth <- grid_externa$max_depth[i]
  s_frac <- grid_externa$sample_fraction[i]
  
  cat("\n========================================\n")
  cat("Entrenando grid interno para max.depth =", m_depth, "y sample.fraction =", s_frac, "...\n")
  
  tiempo_bucle_inicio <- Sys.time()
  
  set.seed(123)
  modelo <- train(
    TARGET_occurrence_M3.0 ~ ., 
    data = datos_sin_metadatos,
    method = "ranger",
    trControl = ctrl_temporal_tuneo,
    tuneGrid = grid_ranger,
    weights = datos_tuneo1$peso_obs,     # Vector de pesos
    metric = "ROC",                      # Área Bajo la Curva
    num.trees = 200,                     # Se recomienda fijar en 500-1000 que estabiliza rápido
    max.depth = m_depth,                 # Inyectado desde el iterador
    sample.fraction = s_frac,            # Inyectado (Sampsize)
    importance = "impurity"              # Clave para luego hacer varImp(modelo)
  )
  
  nombre_modelo <- paste0("m_depth_", m_depth, "_frac_", s_frac)
  resultados_tuneo_rf[[nombre_modelo]] <- modelo
  
  tiempo_bucle_fin <- Sys.time()
  tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")
  
  cat("✅ Terminado el bloque >>", nombre_modelo, "\n")
  cat("⏱️ Tiempo empleado:", round(tiempo_diferencia, 2), "minutos\n")
}

# 6. Apagar procesamiento en paralelo
stopCluster(cl)
registerDoSEQ()


library(stringr)


df_resultados_rf <- bind_rows(lapply(names(resultados_tuneo_rf), function(nombre) {
  
  # Extraemos el modelo de la lista
  modelo <- resultados_tuneo_rf[[nombre]]
  
  # Extraemos la tabla de resultados de caret
  res <- modelo$results
  
  # Extraemos los valores de profundidad y fracción desde el string del nombre
  # El nombre es tipo: "m_depth_5_frac_0.5"
  partes <- unlist(strsplit(nombre, "_"))
  m_depth_val <- as.numeric(partes[3])
  s_frac_val  <- as.numeric(partes[5])
  
  # Añadimos esos metadatos a la tabla
  res$max_depth <- m_depth_val
  res$sample_fraction <- s_frac_val
  
  return(res)
}))

ggplot(df_resultados_rf, aes(x = factor(max_depth), 
                          y = Sens, 
                          color = factor(mtry), 
                          shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: Sensitividad)",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "Sensitividad",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))



df_resultados_rf$Accuracy  <- 0.03*df_resultados_rf$Sens + 0.97*df_resultados_rf$Spec



ggplot(df_resultados_rf, aes(x = factor(max_depth), 
                             y = ROC, 
                             color = factor(mtry), 
                             shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: AUC)",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "AUC",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))

ggplot(df_resultados_rf, aes(x = factor(max_depth), 
                             y = Accuracy, 
                             color = factor(mtry), 
                             shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: Accuracy)",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "Accuracy",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))


nombre_modelo <- "m_depth_5_frac_0.3"

# 2. Extraemos el dataframe con el resumen de métricas (ROC, Sens, Spec) de ese modelo
datos_plot <- resultados_tuneo_rf[[nombre_modelo]]$results
datos_plot$Accuracy <- 0.03*datos_plot$Sens + 0.97*+datos_plot$Spec
# 3. Creamos el gráfico estilo "puntos dispersos con formas y colores"
ggplot(datos_plot, aes(
  x = factor(mtry), 
  y = ROC, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (ROC) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.3",
    x = "Número de variables aleatorias (mtry)",
    y = "AUC (ROC)",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(datos_plot, aes(
  x = factor(mtry), 
  y = Sens, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (Sens) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.3",
    x = "Número de variables aleatorias (mtry)",
    y = "Sens",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


ggplot(datos_plot, aes(
  x = factor(mtry), 
  y = Accuracy, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (Accu) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.3",
    x = "Número de variables aleatorias (mtry)",
    y = "Accu",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



#Se elige las configuraciones gini, min.node.size 1 y 10, y mtry = 5, frac = 0.3, ntree = 200, extratrees


grid_especifico <- expand.grid(
  mtry = 5,
  splitrule = "extratrees",
  min.node.size = c(1, 10)
)

cat("\n========================================\n")
cat("Entrenando modelo Random Forest con configuración específica...\n")

cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")

tiempo_bucle_inicio <- Sys.time()

set.seed(123)
modelo_rf_especifico <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = train_sin_metadatos,
  method = "ranger",
  trControl = ctrl_temporal, # Tu objeto trainControl definido anteriormente
  tuneGrid = grid_especifico,      # La grilla con las combinaciones específicas
  weights = train_cla$peso_obs, # Pesos para el desbalanceo
  metric = "ROC",
  num.trees = 200,                 # Fijado en 200
  sample.fraction = 0.5,           # Fracción de muestreo fijada en 0.5
  importance = "impurity",        # Para ver la importancia de variables
  max.depth = 5               
)

tiempo_bucle_fin <- Sys.time()
tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")

cat("✅ Entrenamiento terminado\n")
cat("⏱️ Tiempo empleado:", round(tiempo_diferencia, 2), "minutos\n")



# Ver los resultados de los dos modelos (min.node.size = 1 vs 10)
print(modelo_rf_especifico)


modelo_rf_especifico$results

modelo_rf_especifico$results$Accuracy <- 0.03 * modelo_rf_especifico$results$Sens +
  0.97* modelo_rf_especifico$results$Spec

#Segundo conjunto


num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


# 5. Bucle de Entrenamiento 
resultados_tuneo_rf2 <- list()

# Bucle externo replicando tu modelo, pero iterando sobre la grilla externa 
for(i in 1:nrow(grid_externa)) {
  m_depth <- grid_externa$max_depth[i]
  s_frac <- grid_externa$sample_fraction[i]
  
  cat("\n========================================\n")
  cat("Entrenando grid interno para max.depth =", m_depth, "y sample.fraction =", s_frac, "...\n")
  
  tiempo_bucle_inicio <- Sys.time()
  
  set.seed(123)
  modelo <- train(
    TARGET_occurrence_M3.0 ~ ., 
    data = datos_sin_metadatos2,
    method = "ranger",
    trControl = ctrl_temporal_tuneo,
    tuneGrid = grid_ranger,
    weights = datos_tuneo1$peso_obs,     # Vector de pesos
    metric = "ROC",                      # Área Bajo la Curva
    num.trees = 200,                     # Se recomienda fijar en 500-1000 que estabiliza rápido
    max.depth = m_depth,                 # Inyectado desde el iterador
    sample.fraction = s_frac,            # Inyectado (Sampsize)
    importance = "impurity"              # Clave para luego hacer varImp(modelo)
  )
  
  nombre_modelo <- paste0("m_depth_", m_depth, "_frac_", s_frac)
  resultados_tuneo_rf2[[nombre_modelo]] <- modelo
  
  tiempo_bucle_fin <- Sys.time()
  tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")
  
  cat("✅ Terminado el bloque >>", nombre_modelo, "\n")
  cat("⏱️ Tiempo empleado:", round(tiempo_diferencia, 2), "minutos\n")
}

# 6. Apagar procesamiento en paralelo
stopCluster(cl)
registerDoSEQ()






df_resultados_rf2 <- bind_rows(lapply(names(resultados_tuneo_rf2), function(nombre) {
  
  # Extraemos el modelo de la lista
  modelo <- resultados_tuneo_rf2[[nombre]]
  
  # Extraemos la tabla de resultados de caret
  res <- modelo$results
  
  # Extraemos los valores de profundidad y fracción desde el string del nombre
  # El nombre es tipo: "m_depth_5_frac_0.5"
  partes <- unlist(strsplit(nombre, "_"))
  m_depth_val <- as.numeric(partes[3])
  s_frac_val  <- as.numeric(partes[5])
  
  # Añadimos esos metadatos a la tabla
  res$max_depth <- m_depth_val
  res$sample_fraction <- s_frac_val
  
  return(res)
}))

df_resultados_rf2 <- na.omit(df_resultados_rf2)

df_resultados_rf2$Accuracy <- 0.03 * df_resultados_rf2$Sens + 0.97 * df_resultados_rf2$Spec


ggplot(df_resultados_rf2, aes(x = factor(max_depth), 
                             y = Sens, 
                             color = factor(mtry), 
                             shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: SENSISTIVIDAD) 2",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "SENS",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))

ggplot(df_resultados_rf2, aes(x = factor(max_depth), 
                              y = ROC, 
                              color = factor(mtry), 
                              shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: AUC) 2",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "AUC",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))

ggplot(df_resultados_rf2, aes(x = factor(max_depth), 
                              y = Accuracy, 
                              color = factor(mtry), 
                              shape = factor(min.node.size))) +
  
  # Geometría de puntos separada ligeramente a lo ancho
  geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.8) +
  
  # Separar en un gráfico "gini" y otro "extratrees"
  facet_wrap(~ splitrule) + 
  
  # Tema visual limpio similar a tu imagen
  theme_bw() +
  
  # Etiquetas automáticas
  labs(
    title = "Rendimiento de Random Forest (Métrica: Accu) 2",
    subtitle = "Comparativa de hiperparámetros (gini vs extratrees)",
    x = "Profundidad Máxima (max_depth) \n(0 = Nodos sin restricción)",
    y = "Accu",
    color = "mtry",
    shape = "min.node.size"
  ) +
  
  # Estética adicional para la retícula y el texto
  theme(
    legend.position = "right",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_line(color = "white"),
    strip.background = element_rect(fill = "grey90")
  ) +
  
  # Te aseguras de tener símbolos bien distinguibles (círculo, triángulo, cuadrado, cruz)
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8))





nombre_modelo <- "m_depth_5_frac_0.3"

# 2. Extraemos el dataframe con el resumen de métricas (ROC, Sens, Spec) de ese modelo
datos_plot2 <- resultados_tuneo_rf2[[nombre_modelo]]$results

datos_plot2 <- na.omit(datos_plot2)
datos_plot2$Accuracy <- 0.03*datos_plot2$Sens + 0.97*datos_plot2$Spec
# 3. Creamos el gráfico estilo "puntos dispersos con formas y colores"
ggplot(datos_plot2, aes(
  x = factor(mtry), 
  y = ROC, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (AUC) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.5",
    x = "Número de variables aleatorias (mtry)",
    y = "AUC",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(datos_plot2, aes(
  x = factor(mtry), 
  y = Sens, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (SENS) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.5",
    x = "Número de variables aleatorias (mtry)",
    y = "SENS",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(datos_plot2, aes(
  x = factor(mtry), 
  y = Accuracy, 
  color = splitrule,                  # Diferenciar gini vs extratrees por color
  shape = factor(min.node.size)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento Random Forest (Accu) - Profundidad Max:", 5),
    subtitle = "Fracción de muestreo: 0.5",
    x = "Número de variables aleatorias (mtry)",
    y = "Accu",
    color = "Regla de división\n(splitrule)",
    shape = "Tamaño min. del nodo\n(min.node.size)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))






#mtry = 4, nodesize = 5, 10, extratrees

grid_especifico2 <- expand.grid(
  mtry = 4,
  splitrule = "extratrees",
  min.node.size = c(5, 10)
)

train_cla2 <- train_cla2 %>%
  mutate(
    # Asignamos el peso según la clase
    peso_obs = ifelse(TARGET_occurrence_M3.0 == "Yes", 2*peso_yes, peso_no),
    # Lo transformamos al formato que exige tidymodels
    peso_obs = importance_weights(peso_obs) 
  )


set.seed(123)
modelo_rf_especifico2 <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = train_sin_metadatos2,
  method = "ranger",
  trControl = ctrl_temporal, # Tu objeto trainControl definido anteriormente
  tuneGrid = grid_especifico2,      # La grilla con las combinaciones específicas
  weights = train_cla$peso_obs, # Pesos para el desbalanceo
  metric = "ROC",
  num.trees = 200,                 # Fijado en 200
  sample.fraction = 0.5,           # Fracción de muestreo fijada en 0.5
  importance = "impurity",        # Para ver la importancia de variables
  max.depth = 5               
)
varImp(modelo_rf_especifico2)
modelo_rf_especifico2$results
modelo_rf_especifico2$results$Accuracy <- 0.03*modelo_rf_especifico2$results$Sens +
  0.97*modelo_rf_especifico2$results$Spec

# Tus datos originales
datos_rf <- rbind(modelo_rf_especifico$results,modelo_rf_especifico2$results)
datos_rf
datos_rf$AccurcySD <- 0.03*datos_rf$SensSD + 0.97*datos_rf$SpecSD
n_folds <- 10

# Crear data frame simulando las métricas de cada fold
set.seed(123)
datos_boxplot <- data.frame(
  AUC = c(
    rnorm(n_folds, mean = datos_rf$ROC[1], sd = datos_rf$ROCSD[1]),
    rnorm(n_folds, mean = datos_rf$ROC[2], sd = datos_rf$ROCSD[2]),
    rnorm(n_folds, mean = datos_rf$ROC[3], sd = datos_rf$ROCSD[1]),
    rnorm(n_folds, mean = datos_rf$ROC[4], sd = datos_rf$ROCSD[2])
  ),
  Sensibilidad = c(
    rnorm(n_folds, mean = datos_rf$Sens[1], sd = datos_rf$SensSD[1]),
    rnorm(n_folds, mean = datos_rf$Sens[2], sd = datos_rf$SensSD[2]),
    rnorm(n_folds, mean = datos_rf$Sens[3], sd = datos_rf$SensSD[1]),
    rnorm(n_folds, mean = datos_rf$Sens[4], sd = datos_rf$SensSD[2])
  ),
  Accuracy = c(
    rnorm(n_folds, mean = datos_rf$Accuracy[1], sd = datos_rf$AccurcySD[1]),
    rnorm(n_folds, mean = datos_rf$Accuracy[2], sd = datos_rf$AccurcySD[2]),
    rnorm(n_folds, mean = datos_rf$Accuracy[3], sd = datos_rf$AccurcySD[1]),
    rnorm(n_folds, mean = datos_rf$Accuracy[4], sd = datos_rf$AccurcySD[2])
  ),
  Modelo = rep(paste("m",datos_rf$mtry, "n", datos_rf$min.node.size), each = n_folds)
)


# Boxplot para AUC
ggplot(datos_boxplot, aes(x = Modelo, y = AUC, fill = Modelo)) +
  geom_boxplot() +
  labs(title = "Boxplot de AUC por modelo",
       y = "AUC", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot para Sensibilidad
ggplot(datos_boxplot, aes(x = Modelo, y = Sensibilidad, fill = Modelo)) +
  geom_boxplot() +
  labs(title = "Boxplot de Sensibilidad por modelo",
       y = "Sensibilidad", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos_boxplot, aes(x = Modelo, y = Accuracy, fill = Modelo)) +
  geom_boxplot() +
  labs(title = "Boxplot de Sensibilidad por modelo",
       y = "Accuracy", x = "") +
  theme_minimal() +
  theme(legend.position = "none")




test_cla2 <- test_enc_cla 

test_cla2$TARGET_occurrence_M3.0 <- as.factor(test_cla2$TARGET_occurrence_M3.0)
levels(test_cla2$TARGET_occurrence_M3.0) <- c("No", "Yes")
test_cla2$TARGET_occurrence_M3.0 <- factor(test_cla2$TARGET_occurrence_M3.0, levels = c("Yes", "No"))


test_cla2 <- test_cla2[, c("TARGET_occurrence_M3.0", listconti17)]

predicciones_rf <- predict(modelo_rf_especifico2, newdata = test_cla2)

matriz_rf <- confusionMatrix(data = predicciones_rf, reference = test_cla2$TARGET_occurrence_M3.0)

print(matriz_rf$byClass)
matriz_rf$table
cm_df_rf <- as.data.frame(matriz_rf$table)

ggplot(cm_df_rf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Red (escala log)")


colnames(cm_df_rf) <- c("Realidad", "Prediccion", "Frecuencia")

# --- Visualización Diferenciada (con Escala Logarítmica) ---
ggplot(cm_df_rf, aes(x = Prediccion, y = Realidad, fill = Frecuencia)) +
  geom_tile(color = "white") + # Añade borde blanco para separar casillas
  
  # AÑADIR TEXTO: Texto blanco grande y bold para números legibles
  geom_text(aes(label = Frecuencia), color = "white", size = 10, fontface = "bold") +
  
  # LA CLAVE: Usar escala de color divergente y transformación logarítmica
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", 
                       midpoint = log10(max(cm_df$Frecuencia)/100), # Ajuste opcional para el blanco
                       trans = "log10", # <--- TRANSFORMA LA ESCALA A LOGARÍTMICA
                       na.value = "white", # Manejar frecuencias de 0 (log(0) es indefinido)
                       # Formatear la leyenda para que muestre números legibles, no logaritmos
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  
  theme_minimal() +
  labs(title = "Matriz de Confusión RF Test",
       subtitle = "Escala de color logarítmica para compensar desbalance",
       x = "Predicción del Modelo", y = "Realidad Observada") +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 14))



matriz_rf$byClass



probabilidades <- predict(modelo_rf_especifico2, newdata = test_cla2, type = "prob")

# Extraemos solo la columna de la probabilidad de la clase minoritaria
prob_clase_interes <- probabilidades[, "Yes"]

# 2. Probar diferentes puntos de corte (ej: 0.1, 0.2, 0.3...)
# Si la probabilidad es mayor a 0.2, predecimos "Positivo", sino "Negativo"
nuevo_threshold <- 0.5
predicciones_ajustadas <- ifelse(prob_clase_interes > nuevo_threshold, "Yes", "No")
predicciones_ajustadas <- as.factor(predicciones_ajustadas)
matriz_rf <- confusionMatrix(data = predicciones_ajustadas, reference = test_cla$TARGET_occurrence_M3.0)
matriz_rf$table
matriz_rf$byClass


set.seed(123)
modelo_rf_especifico2m <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = train_sin_metadatos2,
  method = "ranger",
  trControl = ctrl_temporal1, # Tu objeto trainControl definido anteriormente
  tuneGrid = grid_especifico2,      # La grilla con las combinaciones específicas
  weights = train_cla$peso_obs, # Pesos para el desbalanceo
  metric = "PR-AUC",
  num.trees = 200,                 # Fijado en 200
  sample.fraction = 0.5,           # Fracción de muestreo fijada en 0.5
  importance = "impurity",        # Para ver la importancia de variables
  max.depth = 5               
)
varImp(modelo_rf_especifico2)
modelo_rf_especifico2m$results


grid_ranger <- expand.grid(
  mtry = 2:9, # El último es Bagging
  splitrule = c("gini", "extratrees"), # Opcional: meter extratrees suele mejorar regularización
  min.node.size = c(1, 5, 10, 20)      # Profundidad a nivel local (freno final de la hoja)
)

grid_externa <- expand.grid(
  max_depth = c(0, 5, 15, 30),         
  sample_fraction = 0.5 
)


#pr-auc
resultados_tuneo_rf2m <- list()

# Bucle externo replicando tu modelo, pero iterando sobre la grilla externa 
for(i in 1:nrow(grid_externa)) {
  m_depth <- grid_externa$max_depth[i]
  s_frac <- grid_externa$sample_fraction[i]
  
  cat("\n========================================\n")
  cat("Entrenando grid interno para max.depth =", m_depth, "y sample.fraction =", s_frac, "...\n")
  
  tiempo_bucle_inicio <- Sys.time()
  
  set.seed(123)
  modelo <- train(
    TARGET_occurrence_M3.0 ~ ., 
    data = datos_sin_metadatos2,
    method = "ranger",
    trControl = ctrl_temporal_tuneo1,
    tuneGrid = grid_ranger,
    weights = datos_tuneo1$peso_obs,     # Vector de pesos
    metric = "ROC",                      # Área Bajo la Curva
    num.trees = 200,                     # Se recomienda fijar en 500-1000 que estabiliza rápido
    max.depth = m_depth,                 # Inyectado desde el iterador
    sample.fraction = s_frac,            # Inyectado (Sampsize)
    importance = "impurity"              # Clave para luego hacer varImp(modelo)
  )
  
  nombre_modelo <- paste0("m_depth_", m_depth, "_frac_", s_frac)
  resultados_tuneo_rf2m[[nombre_modelo]] <- modelo
  
  tiempo_bucle_fin <- Sys.time()
  tiempo_diferencia <- difftime(tiempo_bucle_fin, tiempo_bucle_inicio, units = "mins")
  
  cat("✅ Terminado el bloque >>", nombre_modelo, "\n")
  cat("⏱️ Tiempo empleado:", round(tiempo_diferencia, 2), "minutos\n")
}

# 6. Apagar procesamiento en paralelo
stopCluster(cl)
registerDoSEQ()

grid_especifico2[1,]

grid_ranger <- expand.grid(
  mtry = 2:9, # El último es Bagging
  splitrule = c("gini", "extratrees"), # Opcional: meter extratrees suele mejorar regularización
  min.node.size = c(1, 5, 10, 20)      # Profundidad a nivel local (freno final de la hoja)
)

set.seed(123)
modelo_rf_especifico2 <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = train_sin_metadatos2,
  method = "ranger",
  trControl = ctrl_temporal,
  tuneGrid = expand.grid(mtry=4, splitrule="extratrees",min.node.size=10),
  weights = train_cla$peso_obs,     # Vector de pesos
  metric = "ROC",                      # Área Bajo la Curva
  num.trees = 200,                     # Se recomienda fijar en 500-1000 que estabiliza rápido
  max.depth = 5,                 # Inyectado desde el iterador
  sample.fraction = 0.3,            # Inyectado (Sampsize)
  importance = "impurity"              # Clave para luego hacer varImp(modelo)
)




pred <- predict(modelo_rf_especifico2, newdata = test_cla2)

mat<- confusionMatrix(data = pred, reference = test_cla$TARGET_occurrence_M3.0)
print(mat$byClass)

mat$table

cm <- mat$table
df_cm <- as.data.frame(cm)
df_cm
ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Red (escala log)")



probabilidades <- predict(resultados_tuneo_rf2m$m_depth_0_frac_0.5, newdata = test_cla2, type = "prob")

# Extraemos solo la columna de la probabilidad de la clase minoritaria
prob_yes <- probabilidades[, "Yes"]

# 2. Probar diferentes puntos de corte (ej: 0.1, 0.2, 0.3...)
# Si la probabilidad es mayor a 0.2, predecimos "Positivo", sino "Negativo"

proporcion_esperada <- 0.03

# 3. Calcular en qué punto de probabilidad se hace el corte temporal
# Si queremos el top 5%, buscamos el cuantil 95% (1 - 0.05 = 0.95)
umbral_corte <- quantile(prob_yes, probs = 1 - proporcion_esperada)
# 4. Clasificar basándonos en ese nuevo umbral
predicciones_topX <- ifelse(prob_yes >= umbral_corte, "Yes", "No")
predicciones_topX <- as.factor(predicciones_topX)
nuevo_threshold <- 0.1
predicciones_ajustadas <- ifelse(prob_clase_interes > nuevo_threshold, "Yes", "No")
predicciones_ajustadas <- as.factor(predicciones_ajustadas)
matriz_rf <- confusionMatrix(data = predicciones_topX, reference = test_cla$TARGET_occurrence_M3.0)
matriz_rf$table
matriz_rf$byClass

probabilidades <- predict(modelo_rf_especifico2, newdata = test_cla2, type = "prob")


# Extraemos solo la columna de la probabilidad de la clase minoritaria
prob_yes <- probabilidades[, "Yes"]

# 2. Probar diferentes puntos de corte (ej: 0.1, 0.2, 0.3...)
# Si la probabilidad es mayor a 0.2, predecimos "Positivo", sino "Negativo"

proporcion_esperada <- 0.03

# 3. Calcular en qué punto de probabilidad se hace el corte temporal
# Si queremos el top 5%, buscamos el cuantil 95% (1 - 0.05 = 0.95)
umbral_corte <- quantile(prob_yes, probs = 1 - proporcion_esperada)
# 4. Clasificar basándonos en ese nuevo umbral
predicciones_topX <- ifelse(prob_yes >= umbral_corte, "Yes", "No")
predicciones_topX <- as.factor(predicciones_topX)
nuevo_threshold <- 0.1
matriz_rf <- confusionMatrix(data = predicciones_topX, reference = test_cla$TARGET_occurrence_M3.0)
matriz_rf$table
matriz_rf$byClass


cm <- matriz_rf$table
df_cm <- as.data.frame(cm)
df_cm
ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Red (escala log)")


cm_df_rf <- as.data.frame(matriz_rf$table)
colnames(cm_df_rf) <- c("Realidad", "Prediccion", "Frecuencia")

# --- Visualización Diferenciada (con Escala Logarítmica) ---
ggplot(cm_df_rf, aes(x = Prediccion, y = Realidad, fill = Frecuencia)) +
  geom_tile(color = "white") + # Añade borde blanco para separar casillas
  
  # AÑADIR TEXTO: Texto blanco grande y bold para números legibles
  geom_text(aes(label = Frecuencia), color = "black", size = 10, fontface = "bold") +
  
  # LA CLAVE: Usar escala de color divergente y transformación logarítmica
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", 
                       midpoint = log10(max(cm_df$Frecuencia)/100), # Ajuste opcional para el blanco
                       trans = "log10", # <--- TRANSFORMA LA ESCALA A LOGARÍTMICA
                       na.value = "white", # Manejar frecuencias de 0 (log(0) es indefinido)
                       # Formatear la leyenda para que muestre números legibles, no logaritmos
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  
  theme_minimal() +
  labs(title = "Matriz de Confusión RF Test",
       subtitle = "Escala de color logarítmica para compensar desbalance",
       x = "Predicción del Modelo", y = "Realidad Observada") +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 14))



probabilidades_rf <- predict(modelo_rf_especifico2, newdata = test_cla2, type = "prob")

library(pROC)
roc_curva_rf <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_rf[, "Yes"])
plot(roc_curva_rf)

roc_df_rf <- data.frame(
  fpr = 1 - roc_curva_rf$specificities,
  tpr = roc_curva_rf$sensitivities
)

# O usa coords para más control
roc_df_rf <- coords(roc_curva_rf, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_rf <- round(auc(roc_curva_rf), 3)

ggplot(roc_df_rf, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_rf),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))




#Gradient Boosting


# 18. Definición del Grid Base para GBM (Tuneo exhaustivo inicial de Jerarquía 1 y 3)
grid_gbm_base <- expand.grid(
  interaction.depth = c(2, 4, 6),
  n.minobsinnode = c(5, 10, 20),
  shrinkage = c(0.001, 0.01, 0.05, 0.1),
  n.trees = c(50, 100, 200, 500, 1000)
)

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


# Entrenamiento del modelo base
modelo_gbm <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "gbm",
  trControl = ctrl_temporal_tuneo, # Importante: classProbs=TRUE, summaryFunction=twoClassSummary
  tuneGrid = grid_gbm_base,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC", 
  bag.fraction = 1.0, # Orden 2: Fijo por ahora, se deja para refinamiento
  verbose = FALSE
)

stopCluster(cl)
registerDoSEQ()

resultados_gbm <- modelo_gbm$results
resultados_gbm$Accuracy <- 0.03*resultados_gbm$Sens + 0.97*resultados_gbm$Spec
resultados_gbm$AccuracySD <- 0.03*resultados_gbm$SensSD + 0.97*resultados_gbm$SpecSD

ggplot(resultados_gbm, aes(
  x = factor(n.trees), 
  y = ROC, 
  color = shrinkage,                  # Diferenciar gini vs extratrees por color
  shape = factor(interaction.depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento gbm (AUC):"),
    subtitle = "",
    x = "n.trees",
    y = "AUC",
    color = "shrinkage",
    shape = "interaction.depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(resultados_gbm, aes(
  x = factor(n.trees), 
  y = Sens, 
  color = shrinkage,                  # Diferenciar gini vs extratrees por color
  shape = factor(interaction.depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento gbm (Sens):"),
    subtitle = "",
    x = "n.trees",
    y = "Sens",
    color = "shrinkage",
    shape = "interaction.depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


ggplot(resultados_gbm, aes(
  x = factor(n.trees), 
  y = Accuracy, 
  color = shrinkage,                  # Diferenciar gini vs extratrees por color
  shape = factor(interaction.depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento gbm (Accu):"),
    subtitle = "",
    x = "n.trees",
    y = "Accu",
    color = "shrinkage",
    shape = "interaction.depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

grid_gbm_refinamiento <- expand.grid(
  n.trees           = 50,              # Valor óptimo FIJADO
  shrinkage         = 0.1,             # Valor óptimo FIJADO
  interaction.depth = 2,               # Valor óptimo FIJADO
  n.minobsinnode    = c(10, 20, 30)    # A TUNEAR: Variar esto altera el sesgo/varianza
)

# 2. Entrenamos el modelo de refinamiento. 
# El 'subsample' en gbm no está en el tuneGrid por defecto de caret, 
# se pasa como argumento extra (bag.fraction) hacia la librería gbm.
modelo_gbm_refinado <- train(
  TARGET_occurrence_M3.0 ~ .,
  data      = datos_sin_metadatos,
  method    = "gbm",
  trControl = ctrl_temporal_tuneo,
  tuneGrid  = grid_gbm_refinamiento,
  weights   = datos_tuneo1$peso_obs, 
  metric    = "ROC",
  
  # --- TUNEO AVANZADO (Nivel 2 de jerarquía) ---
  bag.fraction = c(0.8), # Subsample: por ej. bajar a 0.8 o 0.7 para intentar rebajar la varianza
  
  verbose   = FALSE   # Para ocultar el texto en la consola
)

resultados_gbm[resultados_gbm$n.trees==50 & resultados_gbm$shrinkage==0.1 &
                 resultados_gbm$interaction.depth==2,]
resultados_gbm_ref <- modelo_gbm_refinado$results
resultados_gbm_ref$Accuracy <- 0.03*resultados_gbm_ref$Sens + 0.97*resultados_gbm_ref$Spec
resultados_gbm_ref$AccuracySD <- 0.03*resultados_gbm_ref$SensSD + 0.97*resultados_gbm_ref$SpecSD


comp_gbm <- rbind(resultados_gbm_ref, resultados_gbm[resultados_gbm$n.trees==50 & resultados_gbm$shrinkage==0.1 &
                                                       resultados_gbm$interaction.depth==2,])

comp_gbm$mod <- c("ref_10", "ref_20", "ref_30", "ori_5", "ori_10","ori_30")

ggplot(comp_gbm, aes(x = factor(mod), y = ROC)) +
  
  geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5) +
  labs(title = "Comparación de AUC entre Modelos",
       x = "Modelo",
       y = "AUC") +
  ylim(0.9, 0.99999) +  # Zoom a la zona de interés
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(ROC ,5)), vjust = -0.5, size = 3) +
  geom_errorbar(aes(ymin = ROC - ROCSD, 
                  ymax = ROC + ROCSD),
              width = 0.2, size = 1, color = "red") 


n_sim <- 10

# Crear datos simulados
set.seed(123)  # Para reproducibilidad
datos_boxplot_gbm <- data.frame()

for(i in 1:nrow(comp_gbm)) {
  # Simular valores normalmente distribuidos
  auc_sim <- rnorm(n_sim, comp_gbm$ROC[i], comp_gbm$ROCSD[i])
  sens_sim <- rnorm(n_sim, comp_gbm$Sens[i], comp_gbm$SensSD[i])
  acc_sim <- rnorm(n_sim, comp_gbm$Accuracy[i], comp_gbm$AccuracySD[i])
  
  # Limitar valores entre 0 y 1
  auc_sim <- pmax(pmin(auc_sim, 1), 0)
  sens_sim <- pmax(pmin(sens_sim, 1), 0)
  acc_sim <- pmax(pmin(acc_sim, 1), 0)
  
  temp_df <- data.frame(
    mod = comp_gbm$mod[i],
    AUC = auc_sim,
    Sensibilidad = sens_sim,
    Accuracy = acc_sim
  )
  
  # Combinar
  datos_boxplot_gbm <- rbind(datos_boxplot_gbm, temp_df)
}


boxplot(ROC_sim ~ mod, 
        data = datos_boxplot_gbm,
        main = "Distribución de AUC",
        xlab = "Modelo",
        ylab = "AUC",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas

boxplot(Sensibilidad~ mod, 
        data = datos_boxplot_gbm,
        main = "Distribución de SENS",
        xlab = "Modelo",
        ylab = "sens",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas

boxplot(Accuracy ~ mod, 
        data = datos_boxplot_gbm,
        main = "Distribución de Accu",
        xlab = "Modelo",
        ylab = "Accu",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas


#Ori_30

modelo_gbm_final <- train(
  TARGET_occurrence_M3.0 ~ .,
  data      = datos_sin_metadatos,
  method    = "gbm",
  trControl = ctrl_temporal_tuneo,
  tuneGrid  = expand.grid(n.trees           = 50,              # Valor óptimo FIJADO
                          shrinkage         = 0.1,             # Valor óptimo FIJADO
                          interaction.depth = 2,               # Valor óptimo FIJADO
                          n.minobsinnode    = 30 ),
  weights   = datos_tuneo1$peso_obs, 
  metric    = "ROC",
  
  # --- TUNEO AVANZADO (Nivel 2 de jerarquía) ---
  bag.fraction = 1, # Subsample: por ej. bajar a 0.8 o 0.7 para intentar rebajar la varianza
  
  verbose   = FALSE   # Para ocultar el texto en la consola
)




pred_gbm <- predict(modelo_gbm_final, newdata = test_cla)

mat_gbm<- confusionMatrix(data = pred_gbm, reference = test_cla$TARGET_occurrence_M3.0)
print(mat_gbm$byClass)

mat_gbm$table


cm <- mat_gbm$table
df_cm_gbm <- as.data.frame(cm)
df_cm_gbm
ggplot(df_cm_gbm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión GBM (escala log)")

probabilidades_gbm <- predict(modelo_gbm_final, newdata = test_cla, type = "prob")
prob_yes_gbm <- probabilidades_gbm[, "Yes"]
proporcion_esperada <- 0.03
umbral_corte <- quantile(prob_yes_gbm, probs = 1 - proporcion_esperada)
predicciones_topX_gbm <- ifelse(prob_yes_gbm >= umbral_corte, "Yes", "No")
predicciones_topX_gbm <- as.factor(predicciones_topX_gbm)
matriz_gbm <- confusionMatrix(data = predicciones_topX_gbm, reference = test_cla$TARGET_occurrence_M3.0)
matriz_gbm$table
matriz_gbm$byClass

cm <- matriz_gbm$table
df_cm_gbm <- as.data.frame(cm)
df_cm_gbm
ggplot(df_cm_gbm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión GBM (escala log)")




roc_curva_gbm <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_gbm[, "Yes"])

roc_df_gbm <- data.frame(
  fpr = 1 - roc_curva_gbm$specificities,
  tpr = roc_curva_gbm$sensitivities
)

# O usa coords para más control
roc_df_gbm <- coords(roc_curva_gbm, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_gbm <- round(auc(roc_curva_gbm), 4)

ggplot(roc_df_gbm, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_gbm),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))




#XGboost


grid_xgboost_ini <- expand.grid(
  max_depth = c(2,4,6),                        # Nivel 1: PREFIJADO
  min_child_weight = c(5,10,20),                # Nivel 1: PREFIJADO
  subsample = 1,                        # Nivel 2: PREFIJADO (o probar 0.8 en refinamiento para atajar varianza)
  colsample_bytree = 1,                 # Nivel 2: PREFIJADO
  gamma = 0,                            # Nivel 2: PREFIJADO
  nrounds = c(50, 100, 200, 500),       # Nivel 3: A TUNEAR
  eta = c(0.01, 0.05, 0.1)              # Nivel 3: A TUNEAR
)

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


modelo_xgboost_ini <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "xgbTree",
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_xgboost_ini,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  verbosity = 0
)



stopCluster(cl)
registerDoSEQ()

resultados_xgb_ini <- modelo_xgboost_ini$results
resultados_xgb_ini$Accuracy <- 0.03*resultados_xgb_ini$Sens + 0.97*resultados_xgb_ini$Spec
resultados_xgb_ini$AccuracySD <- 0.03*resultados_xgb_ini$SensSD + 0.97*resultados_xgb_ini$SpecSD


ggplot(resultados_xgb_ini, aes(
  x = factor(nrounds), 
  y = ROC, 
  color = min_child_weight,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (AUC):"),
    subtitle = "",
    x = "nrounds",
    y = "AUC",
    color = "min_child_weight",
    shape = "max_depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(resultados_xgb_ini, aes(
  x = factor(nrounds), 
  y = Sens, 
  color = min_child_weight,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (Sens):"),
    subtitle = "",
    x = "nrounds",
    y = "AUC",
    color = "min_child_weight",
    shape = "max_depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


ggplot(resultados_xgb_ini, aes(
  x = factor(nrounds), 
  y = Accuracy, 
  color = min_child_weight,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (Acc):"),
    subtitle = "",
    x = "nrounds",
    y = "AUC",
    color = "min_child_weight",
    shape = "max_depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))





grid_xgboost <- expand.grid(
  max_depth = 2,                        # Nivel 1: PREFIJADO
  min_child_weight = 20,                # Nivel 1: PREFIJADO
  subsample = 1,                        # Nivel 2: PREFIJADO (o probar 0.8 en refinamiento para atajar varianza)
  colsample_bytree = 1,                 # Nivel 2: PREFIJADO
  gamma = 0,                            # Nivel 2: PREFIJADO
  nrounds = c(50, 100, 200, 500),       # Nivel 3: A TUNEAR
  eta = c(0.01, 0.05, 0.1)              # Nivel 3: A TUNEAR
)

num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


modelo_xgboost <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "xgbTree",
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_xgboost,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  verbosity = 0
)



stopCluster(cl)
registerDoSEQ()



resultados_xgb <- modelo_xgboost$results
resultados_xgb$Accuracy <- 0.03*resultados_xgb$Sens + 0.97*resultados_xgb$Spec
resultados_xgb$AccuracySD <- 0.03*resultados_xgb$SensSD + 0.97*resultados_xgb$SpecSD



ggplot(resultados_xgb, aes(
  x = factor(nrounds), 
  y = ROC, 
  color = eta,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (AUC):"),
    subtitle = "",
    x = "(nrounds)",
    y = "AUC",
    color = "(eta)",
    shape = "(max_depth)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

ggplot(resultados_xgb, aes(
  x = factor(nrounds), 
  y = Sens, 
  color = eta,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (Sens):"),
    subtitle = "",
    x = "(nrounds)",
    y = "Sens",
    color = "(eta)",
    shape = "(max_depth)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


ggplot(resultados_xgb, aes(
  x = factor(nrounds), 
  y = Accuracy, 
  color = eta,                  # Diferenciar gini vs extratrees por color
  shape = factor(max_depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (Accu):"),
    subtitle = "",
    x = "(nrounds)",
    y = "Accu",
    color = "(eta)",
    shape = "(max_depth)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


#Refinamiento final

grid_xgboost_ref <- expand.grid(
  max_depth = 2,                        # Nivel 1: PREFIJADO
  min_child_weight = 20,                # Nivel 1: PREFIJADO
  subsample = c(0.6,0.8,1),                        # Nivel 2: PREFIJADO (o probar 0.8 en refinamiento para atajar varianza)
  colsample_bytree = c(0.6,0.8,1),                 # Nivel 2: PREFIJADO
  gamma = c(0,1,2),                            # Nivel 2: PREFIJADO
  nrounds = 50,       # Nivel 3: A TUNEAR
  eta = 0.05              # Nivel 3: A TUNEAR
)



num_nucleos <- detectCores()
cl <- makeCluster(num_nucleos - 1)
registerDoParallel(cl)
cat("🚀 Procesamiento en paralelo activado con", num_nucleos - 1, "núcleos.\n")


modelo_xgboost_ref <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "xgbTree",
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_xgboost_ref,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  verbosity = 0
)



stopCluster(cl)
registerDoSEQ()



resultados_xgb_ref <- modelo_xgboost_ref$results
resultados_xgb_ref$Accuracy <- 0.03*resultados_xgb_ref$Sens + 0.97*resultados_xgb_ref$Spec
resultados_xgb_ref$AccuracySD <- 0.03*resultados_xgb_ref$SensSD + 0.97*resultados_xgb_ref$SpecSD


comp_xgb <- resultados_xgb_ref[c(8,25),]
comp_xgb$names <- c("gam0_col1_sub0.8", "gam2_col1_sub0.6")

n_sim <- 10

# Crear datos simulados
set.seed(123)  # Para reproducibilidad
datos_boxplot_xgb <- data.frame()

for(i in 1:nrow(comp_xgb)) {
  # Simular valores normalmente distribuidos
  auc_sim <- rnorm(n_sim, comp_xgb$ROC[i], comp_xgb$ROCSD[i])
  sens_sim <- rnorm(n_sim, comp_xgb$Sens[i], comp_xgb$SensSD[i])
  acc_sim <- rnorm(n_sim, comp_xgb$Accuracy[i], comp_xgb$AccuracySD[i])
  
  # Limitar valores entre 0 y 1
  auc_sim <- pmax(pmin(auc_sim, 1), 0)
  sens_sim <- pmax(pmin(sens_sim, 1), 0)
  acc_sim <- pmax(pmin(acc_sim, 1), 0)
  
  temp_df <- data.frame(
    mod = comp_xgb$names[i],
    AUC = auc_sim,
    Sensibilidad = sens_sim,
    Accuracy = acc_sim
  )
  
  # Combinar
  datos_boxplot_xgb <- rbind(datos_boxplot_xgb, temp_df)
}

boxplot(AUC ~ mod, 
        data = datos_boxplot_xgb,
        main = "Distribución de AUC",
        xlab = "Modelo",
        ylab = "AUC",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas

boxplot(Sensibilidad~ mod, 
        data = datos_boxplot_xgb,
        main = "Distribución de SENS",
        xlab = "Modelo",
        ylab = "sens",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas

boxplot(Accuracy ~ mod, 
        data = datos_boxplot_xgb,
        main = "Distribución de Accu",
        xlab = "Modelo",
        ylab = "Accu",
        col = c(rep("lightblue", 3), rep("lightgreen", 3)),
        las = 2)  # Rotar etiquetas



ggplot(resultados_xgb_ref, aes(
  x = factor(subsample), 
  y = ROC, 
  color = colsample_bytree,                  # Diferenciar gini vs extratrees por color
  shape = factor(gamma)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (AUC):"),
    subtitle = "",
    x = "(subsample)",
    y = "AUC",
    color = "(colsample_bytree)",
    shape = "(gamma)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



ggplot(resultados_xgb_ref, aes(
  x = factor(subsample), 
  y = Sens, 
  color = colsample_bytree,                  # Diferenciar gini vs extratrees por color
  shape = factor(gamma)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (SENS:"),
    subtitle = "",
    x = " (subsample)",
    y = "SENS",
    color = "(colsample_bytree)",
    shape = "(gamma)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



ggplot(resultados_xgb_ref, aes(
  x = factor(subsample), 
  y = Accuracy, 
  color = colsample_bytree,                  # Diferenciar gini vs extratrees por color
  shape = factor(gamma)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento xgb (Accu):"),
    subtitle = "",
    x = "(subsample)",
    y = "Accu",
    color = "(colsample_bytree)",
    shape = "(gamma)"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



grid_xgboost_final <- expand.grid(
  max_depth = 2,                        # Nivel 1: PREFIJADO
  min_child_weight = 20,                # Nivel 1: PREFIJADO
  subsample = 0.8,                        # Nivel 2: PREFIJADO (o probar 0.8 en refinamiento para atajar varianza)
  colsample_bytree = 1,                 # Nivel 2: PREFIJADO
  gamma = 0,                            # Nivel 2: PREFIJADO
  nrounds = 50,       # Nivel 3: A TUNEAR
  eta = 0.05              # Nivel 3: A TUNEAR
)

modelo_xgboost_final <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "xgbTree",
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_xgboost_final,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  verbosity = 0
)


pred_xgbm <- predict(modelo_xgboost_final, newdata = test_cla)

mat_xgbm <- confusionMatrix(data = pred_xgbm, reference = test_cla$TARGET_occurrence_M3.0)
print(mat_xgbm$byClass)

mat_xgbm$table


cm <- mat_xgbm$table
df_cm_xgbm <- as.data.frame(cm)
df_cm_xgbm
ggplot(df_cm_xgbm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión XGB (escala log)")

probabilidades_xgbm <- predict(modelo_xgboost_final, newdata = test_cla, type = "prob")
prob_yes_xgbm <- probabilidades_xgbm[, "Yes"]
proporcion_esperada <- 0.03
umbral_corte <- quantile(prob_yes_xgbm, probs = 1 - proporcion_esperada)
predicciones_topX_xgbm <- ifelse(prob_yes_xgbm >= umbral_corte, "Yes", "No")
predicciones_topX_xgbm <- as.factor(predicciones_topX_xgbm)
matriz_xgbm <- confusionMatrix(data = predicciones_topX_xgbm, reference = test_cla$TARGET_occurrence_M3.0)
matriz_xgbm$table
matriz_xgbm$byClass

cm <- matriz_xgbm$table
df_cm_xgbm <- as.data.frame(cm)
df_cm_xgbm
ggplot(df_cm_xgbm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión GBM (escala log)")




roc_curva_xgbm <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_xgbm[, "Yes"])

roc_df_xgbm <- data.frame(
  fpr = 1 - roc_curva_xgbm$specificities,
  tpr = roc_curva_xgbm$sensitivities
)

# O usa coords para más control
roc_df_xgbm <- coords(roc_curva_xgbm, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_xgbm <- round(auc(roc_curva_xgbm), 4)

ggplot(roc_df_xgbm, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_xgbm),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))






#catboost
library(catboost)
x_cat <- datos_sin_metadatos[,listconti2]
y_cat <- datos_sin_metadatos[,"TARGET_occurrence_M3.0"]
  
grid_catboost <- expand.grid(
  depth = c(2,4,6),                            # Nivel 1: PREFIJADO                      # Nivel 2: PREFIJADO
  rsm = 1,                              # Nivel 2: PREFIJADO (colsample_bylevel)
  l2_leaf_reg = 3,                      # Nivel 2: PREFIJADO (regularización)
  iterations =  500,    # Nivel 3: A TUNEAR
  learning_rate =  0.01 ,   # Nivel 3: A TUNEAR
  border_count = 64              # Suele pedirlo caret en catboost, valor por defecto
)

y_cat <- y_cat[[1]]
y_cat <- as.factor(y_cat)
str(y_cat) 

modelo_catboost <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 20,
  subsample=1
)

modelo_catboost_10 <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 10,
  subsample=1
)

modelo_catboost_5 <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 5,
  subsample=1
)

modelo_catboost$results$min_data_in_leaf <- c(20,20,20)

modelo_catboost_10$results$min_data_in_leaf <- c(10,10,10)

modelo_catboost_5$results$min_data_in_leaf <- c(5,5,5)


res_catboost_ini <- rbind(modelo_catboost$results,modelo_catboost_10$results
                          ,modelo_catboost_5$results)

res_catboost_ini



ggplot(res_catboost_ini, aes(
  x = factor(depth), 
  y = ROC, 
  color = learning_rate,                  # Diferenciar gini vs extratrees por color
  shape = factor(min_data_in_leaf)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento cat (AUC):"),
    subtitle = "",
    x = "La profundidad (depth)",
    y = "AUC",
    color = "learning_rate",
    shape = "min_data_in_leaf"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


grid_catboost2 <- expand.grid(
  depth = 2,                            # Nivel 1: PREFIJADO                      # Nivel 2: PREFIJADO
  rsm = 1,                              # Nivel 2: PREFIJADO (colsample_bylevel)
  l2_leaf_reg = 3,                      # Nivel 2: PREFIJADO (regularización)
  iterations =  c(30,50,100,300,500),    # Nivel 3: A TUNEAR
  learning_rate =  c(0.001,0.01,0.05,0.1) ,   # Nivel 3: A TUNEAR
  border_count = 64              # Suele pedirlo caret en catboost, valor por defecto
)
modelo_catboost_fase2 <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost2,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 10,
  subsample=1
)

res_catboost2 <- modelo_catboost_fase2$results


ggplot(res_catboost2, aes(
  x = factor(iterations), 
  y = ROC, 
  color = learning_rate,                  # Diferenciar gini vs extratrees por color
  shape = factor(depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento cat (AUC):"),
    subtitle = "",
    x = "La profundidad (iterations)",
    y = "AUC",
    color = "learning_rate",
    shape = "depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



ggplot(res_catboost2, aes(
  x = factor(iterations), 
  y = Sens, 
  color = learning_rate,                  # Diferenciar gini vs extratrees por color
  shape = factor(depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento cat (SENS):"),
    subtitle = "",
    x = "La profundidad (iterations)",
    y = "SENS",
    color = "learning_rate",
    shape = "depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))

#iterations = 300, lr = 0.05

grid_catboost_ref <- expand.grid(
  depth = 2,                            # Nivel 1: PREFIJADO                      # Nivel 2: PREFIJADO
  rsm = c(0.6,0.8,1),                              # Nivel 2: PREFIJADO (colsample_bylevel)
  l2_leaf_reg = c(3,5,7),                      # Nivel 2: PREFIJADO (regularización)
  iterations =  300,    # Nivel 3: A TUNEAR
  learning_rate = 0.05 ,   # Nivel 3: A TUNEAR
  border_count = 64              # Suele pedirlo caret en catboost, valor por defecto
)

modelo_catboost_ref <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost_ref,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 10,
  subsample=1
)

modelo_catboost_ref$results

res_catboost_ref <- modelo_catboost_ref$results


ggplot(res_catboost_ref, aes(
  x = factor(rsm), 
  y = ROC, 
  color = l2_leaf_reg,                  # Diferenciar gini vs extratrees por color
  shape = factor(depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento cat (AUC):"),
    subtitle = "",
    x = "La profundidad (rsm)",
    y = "AUC",
    color = "l2_leaf_reg",
    shape = "depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))



ggplot(res_catboost_ref, aes(
  x = factor(rsm), 
  y = Sens, 
  color = l2_leaf_reg,                  # Diferenciar gini vs extratrees por color
  shape = factor(depth)       # Diferenciar tamaño de nodo hoja por forma
)) +
  geom_point(size = 4, alpha = 0.8) +   # Tamaño de los puntos
  
  # Añadimos títulos y etiquetas
  labs(
    title = paste("Rendimiento cat (SENS):"),
    subtitle = "",
    x = "La profundidad (rsm)",
    y = "SENS",
    color = "l2_leaf_reg",
    shape = "depth"
  ) +
  
  # Estilo visual limpio y fondo cuadriculado, similar al de tu imagen
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    legend.background = element_rect(fill = "gray95", color = "gray80")
  ) +
  
  # Ajustar la escala de formas para usar círculos, triángulos, cuadrados, cruces, etc.
  scale_shape_manual(values = c(16, 17, 15, 3, 8))


grid_catboost_final <- expand.grid(
  depth = 2,                            # Nivel 1: PREFIJADO                      # Nivel 2: PREFIJADO
  rsm = 0.6,                              # Nivel 2: PREFIJADO (colsample_bylevel)
  l2_leaf_reg = 7,                      # Nivel 2: PREFIJADO (regularización)
  iterations =  300,    # Nivel 3: A TUNEAR
  learning_rate = 0.05 ,   # Nivel 3: A TUNEAR
  border_count = 64              # Suele pedirlo caret en catboost, valor por defecto
)

modelo_catboost_final <- train(
  x_cat,
  y_cat,
  method = catboost.caret,
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_catboost_final,
  weights = datos_tuneo1$peso_obs,
  metric = "ROC",
  logging_level = 'Silent',
  min_data_in_leaf= 10,
  subsample=1
)


pred_cat <- predict(modelo_catboost_final, newdata = test_cla)

mat_cat <- confusionMatrix(data = pred_cat, reference = test_cla$TARGET_occurrence_M3.0)
print(mat_cat$byClass)

mat_cat$table


cm <- mat_cat$table
df_cm_cat <- as.data.frame(cm)
df_cm_cat
ggplot(df_cm_cat, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión XGB (escala log)")

probabilidades_cat <- predict(modelo_catboost_final, newdata = test_cla, type = "prob")
prob_yes_cat <- probabilidades_cat[, "Yes"]
proporcion_esperada <- 0.03
umbral_corte <- quantile(prob_yes_cat, probs = 1 - proporcion_esperada)
predicciones_topX_cat <- ifelse(prob_yes_cat >= umbral_corte, "Yes", "No")
predicciones_topX_cat <- as.factor(predicciones_topX_cat)
matriz_cat <- confusionMatrix(data = predicciones_topX_cat, reference = test_cla$TARGET_occurrence_M3.0)
matriz_cat$table
matriz_cat$byClass

cm <- matriz_cat$table
df_cm_cat <- as.data.frame(cm)
df_cm_cat
ggplot(df_cm_cat, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión GBM (escala log)")




roc_curva_cat <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_cat[, "Yes"])

roc_df_cat <- data.frame(
  fpr = 1 - roc_curva_cat$specificities,
  tpr = roc_curva_cat$sensitivities
)

# O usa coords para más control
roc_df_cat <- coords(roc_curva_cat, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_cat <- round(auc(roc_curva_cat), 4)

ggplot(roc_df_cat, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_cat),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))




#rsm=0.6, l2_leaf_reg = 7

#Regresión logística:

ctrl_temporal_tuneo2 <- trainControl(
  method = "timeslice",
  initialWindow = 25000, 
  horizon = 5000,       
  skip = 5000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = prSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  allowParallel = TRUE,
  savePredictions = "final"
)

ctrl_temporal2 <- trainControl(
  method = "timeslice",
  initialWindow = 500000, 
  horizon = 50000,       
  skip = 50000,      # FILAS a predecir hacia el futuro
  fixedWindow = TRUE,    # FALSE = los datos viejos no se borran, la ventana crece
  summaryFunction = prSummary, # Evalúa por AUC (Sensibilidad, Especificidad)
  classProbs = TRUE,
  allowParallel = TRUE,
  savePredictions = "final"
)

grid_glmnet <- expand.grid(
  alpha = seq(0, 1, length = 5),    # Mezcla: 0 = Ridge, 1 = Lasso
  lambda = 10^seq(-3, 1, length = 10) # Fuerza de la penalización
)

# 2. Entrenas el modelo
modelo_logistico_penalizado <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = train_sin_metadatos,
  method = "glmnet",
  family = "binomial",
  trControl = ctrl_temporal,
  weights = train_cla$peso_obs,
  tuneGrid = grid_glmnet,              # Aquí inyectamos la grilla de hiperparámetro
  metric = "AUC"
)


predicciones_log <- predict(modelo_logistico_penalizado, newdata = test_cla)

matriz_log <- confusionMatrix(data = predicciones_log, reference = test_cla$TARGET_occurrence_M3.0)

print(matriz_log$byClass)

matriz_log$table

cm <- matriz_log$table
df_cm_log <- as.data.frame(cm)
df_cm_log
ggplot(df_cm_log, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión Logística (escala log)")

probabilidades_log <- predict(modelo_logistico_penalizado, newdata = test_cla, type = "prob")
prob_yes_log <- probabilidades_log[, "Yes"]
proporcion_esperada <- 0.03
umbral_corte <- quantile(prob_yes_log, probs = 1 - proporcion_esperada)
predicciones_topX_log <- ifelse(prob_yes_log >= umbral_corte, "Yes", "No")
predicciones_topX_log <- as.factor(predicciones_topX_log)
matriz_log <- confusionMatrix(data = predicciones_topX_log, reference = test_cla$TARGET_occurrence_M3.0)
matriz_log$table
matriz_log$byClass




roc_curva_log <- roc(test_cla$TARGET_occurrence_M3.0, probabilidades_log[, "Yes"])


roc_df_log <- data.frame(
  fpr = 1 - roc_curva_log$specificities,
  tpr = roc_curva_log$sensitivities
)

# O usa coords para más control
roc_df_log <- coords(roc_curva_log, ret = c("fpr", "tpr"), transpose = FALSE)
auc_val_log <- round(auc(roc_curva_log), 3)

ggplot(roc_df_log, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Curva ROC",
    subtitle = paste("Área bajo la curva (AUC) =", auc_val_log),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))




#Arbol

library(MLmetrics)
library(rpart)

grid_arbol <- expand.grid(
  max_depth = c(5, 10, 15, 20),           # max_depth
  min_samples_split = c(10, 20, 50),      # minsplit
  min_samples_leaf = c(5, 10, 20)         # minbucket
)

resultados_tuneo <- data.frame()
mejor_f1 <- 0
mejor_modelo_arbol <- NULL

# 3. Bucle para iterar las combinaciones (ya que caret no tunea minsplit/minbucket nativamente)
for (i in 1:nrow(grid_arbol)) {
  
  # Extraer hiperparámetros actuales
  p_maxdepth <- grid_arbol$max_depth[i]
  p_minsplit <- grid_arbol$min_samples_split[i]
  p_minleaf  <- grid_arbol$min_samples_leaf[i]
  
  set.seed(123) # Para reproducibilidad en la ventana temporal
  
  # Entrenar el modelo con los parámetros actuales
  modelo_tmp <- train(
    TARGET_occurrence_M3.0 ~ .,
    data = datos_sin_metadatos2,
    method = "rpart2", # Usamos rpart2 para poder mapear maxdepth en tuneGrid
    tuneGrid = data.frame(maxdepth = p_maxdepth),
    trControl = ctrl_temporal_tuneo2,
    metric = "F", # Indicamos F para optimizar según el F1-Score
    weights = datos_tuneo1$peso_obs,
    control = rpart.control( # Inyectamos las reglas de split y hojas
      minsplit = p_minsplit, 
      minbucket = p_minleaf
    )
  )
  
  # Extraer el F1-Score resultante de esta combinación
  f1_actual <- max(modelo_tmp$results$F, na.rm = TRUE)
  
  # Guardar los registros en nuestro dataframe de resultados
  resultados_tuneo <- rbind(resultados_tuneo, data.frame(
    max_depth = p_maxdepth,
    min_samples_split = p_minsplit,
    min_samples_leaf = p_minleaf,
    F1_Score = f1_actual
  ))
  
  # Detectar si es el mejor modelo y guardarlo
  if (!is.na(f1_actual) && f1_actual > mejor_f1) {
    mejor_f1 <- f1_actual
    mejor_modelo_arbol <- modelo_tmp
  }
}


grid_arbol <- expand.grid(
  max_depth = c(5, 10, 15, 20),           # max_depth
  min_samples_split = c(10, 20, 50),      # minsplit
  min_samples_leaf = c(5, 10, 20)         # minbucket
)


modelo_tmp <- train(
  TARGET_occurrence_M3.0 ~ .,
  data = datos_sin_metadatos,
  method = "rpart2", # Usamos rpart2 para poder mapear maxdepth en tuneGrid
  tuneGrid = expand.grid(maxdepth=5),
  trControl = ctrl_temporal_tuneo,
  metric = "AUC", # Indicamos F para optimizar según el F1-Score
  weights = datos_tuneo1$peso_obs,
  control = rpart.control( # Inyectamos las reglas de split y hojas
    minsplit = 1, 
    minbucket = 1
  )
)



library(rpart.plot)

# Graficar el árbol
rpart.plot(modelo_tmp$finalModel, 
           main = "Árbol de Decisión",
           type = 4,       # Dibuja las etiquetas de las divisiones de forma limpia
           extra = 104,    # Muestra las probabilidades por clase y el % de observaciones del nodo
           under = TRUE,   # Pone las etiquetas debajo de las cajas
           faclen = 0,     # No abrevia los nombres de las variables categóricas
           box.palette = "auto", # Colores automáticos según la clase
           tweak = 1.2)    # Aumenta un poco el tamaño del texto (ajustar si es necesario)


grid_arbol_denso <- expand.grid(
  cp = c(0, 0.0001, 0.001, 0.005) 
)

# 2. Entrenamos el modelo relajando los límites de rpart
modelo_arbol_profundo <- train(
  TARGET_occurrence_M3.0 ~ ., 
  data = datos_sin_metadatos,
  method = "rpart",
  trControl = ctrl_temporal_tuneo,
  tuneGrid = grid_arbol_denso,
  weights = datos_tuneo1$peso_obs,
  metric = "AUC",
  # 3. Aquí relajamos las reglas de partición para FORZAR los splits:
  control = rpart.control(
    minsplit = 5,    # Intenta dividir incluso si solo hay 5 observaciones en una rama
    minbucket = 2,   # Permite hojas finales con solo 2 observaciones
    maxdepth = 10    # Permite al árbol tener hasta 10 niveles de profundidad
  )
)

# Visualizar el mejor árbol (para ver si ahora está más grande)
library(rpart.plot)
rpart.plot(modelo_arbol_profundo$finalModel)


coef(modelo_logistico_penalizado$finalModel)
modelo_logistico_penalizado$finalModel$param


coeficientes <- coef(modelo_logistico_penalizado$finalModel, 
                     modelo_logistico_penalizado$finalModel$lambdaOpt)


as.matrix(coeficientes)




dput(colnames(datos_tuneo1))





library(haven)
arch_exp <- train_sin_metadatos
names(arch_exp) <- gsub("[./]", "_",names(arch_exp))  
dput(names(arch_exp))
colnames(arch_exp) <- c("TARGET_occurrence_M3_0", "target_cla_enc_30d", "days_since_last_eq", 
                        "adj_roll30_energia", "roll30_energia", "roll30_mag_max", "lon", 
                        "eq_count", "lat", "lag1_eq_count")

write_xpt(arch_exp, "D:/TFM/datos.xpt", version = 5)




varImp(modelos_finales$Mod3_s3_d0.01_300it)


imp_df <- data.frame(
  Variable = rownames(imp$importance),
  Overall = imp$importance$Overall
)

# Ordenar
imp_df <- imp_df[order(imp_df$Overall, decreasing = TRUE), ]

# Graficar
ggplot(imp_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de variables - NNET",
    x = "Variables",
    y = "Importancia (Overall)"
  ) +
  theme_minimal()


library(pdp)

pdp::partial(modelos_finales$Mod3_s3_d0.01_300it, pred.var = "roll30_energia") |>
  plot()


matriz_tabla <- as.table(matrix(c(184249, 1241, 3276, 2229),
                                nrow = 2,
                                byrow = TRUE,
                                dimnames = list(Reference = c("No", "Yes"),
                                                Prediction = c("No", "Yes"))))
matriz_tabla

df_cm <- as.data.frame(matriz_tabla)
df_cm


ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión KNN (escala log)")



matriz_tabla <- as.table(matrix(c(185374, 116,   # Fila 0: No reales
                                  2122, 3383),
                                nrow = 2,
                                byrow = TRUE,
                                dimnames = list(Reference = c("No", "Yes"),
                                                Prediction = c("No", "Yes"))))
matriz_tabla

df_cm <- as.data.frame(matriz_tabla)
df_cm


ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión RF2 (escala log)")



library(ggplot2)


matriz_tabla <- as.table(matrix(c(87532 ,  1045,   # Fila 0: No reales
                                  3404, 3809),
                                nrow = 2,
                                byrow = TRUE,
                                dimnames = list(Reference = c("No", "Yes"),
                                                Prediction = c("No", "Yes"))))
matriz_tabla

df_cm <- as.data.frame(matriz_tabla)
df_cm


ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "red", trans = "log") +
  theme_minimal() +
  labs(title = "Matriz de confusión (Chile)")




