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


library(slider)
library(lubridate)
library(dplyr)

#Para el target de clasificacion de 3.0
train_enc_reg <- train %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_reg_enc_30d = slide_index_dbl(
      .x = TARGET_mag_max_next_7d,               # Variable a promediar
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
    target_reg_enc_30d = ifelse(is.na(target_reg_enc_30d), mean(TARGET_mag_max_next_7d, na.rm = TRUE), target_reg_enc_30d)
  )

#Para el target de clasificacion de 3.0, test
test_enc_reg <- test %>%
  arrange(date) %>%
  group_by(cell_id) %>%
  mutate(
    target_reg_enc_30d = slide_index_dbl(
      .x = TARGET_mag_max_next_7d,               # Variable a promediar
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
    target_reg_enc_30d = ifelse(is.na(target_reg_enc_30d), mean(TARGET_mag_max_next_7d, na.rm = TRUE), target_reg_enc_30d)
  )



#Selección de variables

vars_exlu <- c("TARGET_occurrence_M4.5", 
               "date","cell_id", "TARGET_occurrence_M3.0", "updated")

archivo1 <- train_enc_reg[train_enc_reg$date < as.Date("2000-06-01"), ]
archivo1<- archivo1[,-which(names(archivo1) %in% vars_exlu)]

archivo2 <- train_enc_reg[train_enc_reg$date < as.Date("2000-06-01"), ]
archivo2<- archivo2[,-which(names(archivo1) %in% vars_exlu)]

# -----------------------------------------------------
# STEPAIC
# -----------------------------------------------------
full<-glm(TARGET_mag_max_next_7d~.,data=archivo1,family = gaussian(link = "identity"))
null<-glm(TARGET_mag_max_next_7d~1,data=archivo1,family = gaussian(link = "identity"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),trace=FALSE)

vec<-(names(selec1[[1]]))

length(vec)

dput(vec)

listconti1 <- setdiff(names(selec1[[1]]), "(Intercept)")

listconti1 <- c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", 
  "roll30_energia", "adj_roll30_mag_max", "gap", "roll7_eq_count", 
  "roll30_eq_count", "locationSource_ci", "adj_roll30_energia", 
  "main_depth", "eq_count", "rms", "days_since_last_eq", "lat", 
  "lon", "net_us", "locationSource_nc", "nst", "adj_roll7_mag_max"
)

k_BIC = log(nrow(archivo1))

selec2 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = k_BIC)
listconti2 <- setdiff(names(selec2[[1]]), "(Intercept)")

dput(listconti2)

listconti2<- c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_energia", 
  "adj_roll30_mag_max", "gap", "roll7_eq_count", "roll30_eq_count", 
  "locationSource_ci", "adj_roll30_energia", "main_depth", "eq_count", 
  "rms", "days_since_last_eq", "lat")

selec3 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = 15)
listconti3 <- setdiff(names(selec3[[1]]), "(Intercept)")

dput(listconti3)

listconti3<- c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_energia", 
  "adj_roll30_mag_max", "gap", "roll7_eq_count", "roll30_eq_count", 
  "locationSource_ci", "adj_roll30_energia", "eq_count", "rms")

source("funcion steprepetido.R")

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
           "target_reg_enc_30d")

# STEPWISE con k general
selec3 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = 15)
listconti3 <- setdiff(names(selec3[[1]]), "(Intercept)")

dput(listconti3)



lis<-steprepetido(data=archivo1,vardep=c("TARGET_mag_max_next_7d"),
                    listconti=lista,
                    sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")

listconti4<-dput(lis[[2]][[1]])
dput(listconti4)

listconti4 <- c("target_reg_enc_30d", "roll7_mag_max", "roll30_mag_max", "roll30_energia", 
  "roll7_eq_count", "gap", "adj_roll30_mag_max", "roll30_eq_count", 
  "adj_roll30_energia", "main_depth", "locationSource_ci", "lat", 
  "net_us", "days_since_last_eq", "nst", "locationSource_nc", "lon", 
  "rms", "main_mag", "status_reviewed", "dmin", "main_lat", "lat_bin", 
  "depthError", "locationSource_nn", "energia_acumulada_diaria", 
  "adj_num_eqs_x", "adj_max_mag_x")


lis<-steprepetido(data=archivo1,vardep="TARGET_mag_max_next_7d",
                         listconti=lista,
                         sinicio=sinicio,sfinal=sfinal,porcen=0.8,criterio="BIC")
listconti5<-dput(lis[[2]][[1]])
dput(listconti5)

listconti5 <- c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_energia", 
  "main_depth", "locationSource_ci", "roll7_eq_count", "roll30_eq_count", 
  "adj_roll30_mag_max", "eq_count", "adj_roll30_energia", "gap", 
  "net_us", "lat")

listconti6<-dput(lis[[2]][[2]])
dput(listconti6)

listconti6<-c("target_reg_enc_30d", "roll7_mag_max", "roll30_mag_max", "roll30_energia", 
  "adj_roll30_mag_max", "roll7_eq_count", "gap", "roll30_eq_count", 
  "locationSource_ci", "adj_roll30_energia", "rms", "eq_count", 
  "depthError", "days_since_last_eq")


x <- archivo1[,-which(names(archivo1) %in% c("TARGET_mag_max_next_7d"))]
y <- archivo1$TARGET_mag_max_next_7d


control <- rfeControl(functions=rfFuncs, method="cv", number=4)
# run the RFE algorithm
results <- rfe(x, y, sizes=c(1:70), rfeControl=control)

cosa<-as.data.frame(results$results)

# Resultados en gráfico
ggplot(cosa,aes(y=MAE, x=Variables))+geom_point()+geom_line()+ 
  scale_y_continuous(breaks = cosa$MAE) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")

selecrfe<-results$optVariables[1:7]  

dput(selecrfe)


listconti7 <-dput(selecrfe)

listconti7<-c("target_reg_enc_30d", "lon", "lat", "days_since_last_eq", "adj_roll30_mag_max", 
  "roll30_energia", "adj_roll30_energia")



# MMPC

t1 <- 0.05   # Filtro estándar (Permisivo)
t2 <- 0.01   # Filtro exigente (Estricto)
t3 <- 0.001  # Filtro de alta confianza (Ultra estricto)

max_k_mmpc <- 3

mmpc2 <- MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t1, test = "testIndReg")
listconti8 <- names(x[, c(mmpc2$selectedVars)])
dput(listconti8)

listconti8<-c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_eq_count", 
  "adj_roll30_eq_count", "gap", "roll30_energia", "lat", "days_since_last_eq", 
  "locationSource_ci")

mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t2, test = "testIndReg")
listconti9 <- names(x[, c(mmpc2$selectedVars)])
dput(listconti9)

listconti9<-c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_eq_count", 
  "adj_roll30_eq_count", "gap", "roll30_energia", "lat")


mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t3, test = "testIndReg")
listconti10 <- names(x[, c(mmpc2$selectedVars)])
cat("Fin MMPC...\n")  

dput(listconti10)



listconti10<-c("target_reg_enc_30d", "roll7_energia", "roll30_mag_max", "roll30_eq_count", 
  "adj_roll30_eq_count", "gap", "roll30_energia")




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


listconti11<-c("eq_count", "energia_acumulada_diaria", "main_mag", "main_depth", 
  "main_lat", "main_lon", "magError", "depthError", "horizontalError", 
  "nst", "gap", "dmin", "rms", "magNst", "is_converted_mag", "lat_bin", 
  "lon_bin", "roll7_eq_count", "roll30_eq_count", "roll7_mag_max", 
  "roll30_mag_max", "roll7_energia", "roll30_energia", "days_since_last_eq", 
  "lat", "lon", "adj_num_eqs_x", "adj_max_mag_x", "adj_num_eqs_y", 
  "adj_max_mag_y", "adj_roll7_eq_count", "adj_roll7_mag_max", "adj_roll30_energia", 
  "adj_roll30_eq_count", "adj_roll30_mag_max", "locationSource_ci", 
  "magSource_ci", "status_reviewed", "target_reg_enc_30d")



#Mutual Information

library(infotheo)

x_discretizada <- discretize(x, disc = "equalwidth", nbins = 10)
y_discretizada <- discretize(y, disc = "equalwidth", nbins = 10)

valores_mi <- sapply(x_discretizada, function(columna) {
  mutinformation(columna, y_discretizada)
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

listconti12<-c("target_reg_enc_30d", "roll30_energia", "roll30_mag_max", "roll30_eq_count", 
  "roll7_mag_max", "roll7_energia", "lon", "lat", "adj_roll30_energia", 
  "adj_roll30_eq_count", "adj_roll30_mag_max", "days_since_last_eq", 
  "roll7_eq_count", "adj_roll7_mag_max", "lat_bin", "main_lat", 
  "magError", "main_mag", "gap", "energia_acumulada_diaria", "lag1_mag_max", 
  "status_reviewed", "main_lon", "lon_bin", "locationSource_empty", 
  "magSource_empty", "net_empty", "status_empty", "energia_lag1", 
  "is_converted_mag", "magNst", "rms", "adj_roll7_eq_count", "locationSource_ci", 
  "magSource_ci", "nst")

#Lasso

library(glmnet)

x_matrix <- model.matrix(~ . - 1, data = x)

cv_lasso <- cv.glmnet(x_matrix, y, family = "gaussian", alpha = 1, nfolds = 4)

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

listconti13<-c("eq_count", "main_depth", "horizontalError", "gap", "dmin", 
  "lag1_eq_count", "roll7_eq_count", "roll30_eq_count", "roll7_mag_max", 
  "roll30_mag_max", "roll7_energia", "days_since_last_eq", "lat", 
  "adj_roll7_mag_max", "adj_roll30_eq_count", "adj_roll30_mag_max", 
  "locationSource_ci", "locationSource_nn", "locationSource_us", 
  "magSource_ci", "magSource_nn", "magSource_us", "target_reg_enc_30d"
)


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


ranking_rf <- data.frame(
  Variable = rownames(importancia_matriz),
  Importancia = importancia_matriz[, "%IncMSE"]
) %>% 
  arrange(desc(Importancia))
# Ver las 20 variables más importantes
print(head(ranking_rf, 40))

# Graficar el ranking de forma visual (Muestra las variables más críticas arriba)
varImpPlot(modelo_rf, type = 1, scale = FALSE, main = "Importancia de Variables (Random Forest)")


variables_rf_final <- ranking_rf$Variable[1:37]

listconti14 <- variables_rf_final
dput(listconti14)

listconti14<-c("target_reg_enc_30d", "lat", "lon", "days_since_last_eq", "adj_roll30_energia", 
  "adj_roll30_mag_max", "adj_roll7_mag_max", "roll30_energia", 
  "adj_roll30_eq_count", "roll30_eq_count", "roll30_mag_max", "roll7_mag_max", 
  "adj_roll7_eq_count", "roll7_energia", "roll7_eq_count", "adj_max_mag_y", 
  "adj_max_mag_x", "main_lat", "nst", "adj_num_eqs_y", "lat_bin", 
  "adj_num_eqs_x", "magNst", "lon_bin", "gap", "main_mag", "status_reviewed", 
  "is_converted_mag", "net_empty", "main_lon", "energia_acumulada_diaria", 
  "rms", "locationSource_empty", "horizontalError", "magSource_empty", 
  "dmin", "eq_count")


#XGBoost

library(xgboost)



modelo_xgb <- xgboost(
  data = x_matrix,
  label = y,
  nrounds = 100,                    # Número de iteraciones/árboles
  objective = "reg:squarederror",    # Clasificación binaria
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

listconti15<-c("target_reg_enc_30d", "days_since_last_eq", "roll30_energia", 
  "roll30_eq_count", "adj_roll30_energia", "roll30_mag_max", "roll7_mag_max", 
  "lat", "adj_roll30_eq_count", "adj_roll7_mag_max", "adj_roll30_mag_max", 
  "lon", "nst", "adj_roll7_eq_count", "roll7_energia", "main_lat"
)



# -----------------------------------------------------
# COMPARACION VIA CV REPETIDA Y BOXPLOT
# -----------------------------------------------------

source("cruzadas avnnet y lin.R")

data<-archivo1

medias1<-cruzadalin(data=data,
                    vardep="TARGET_mag_max_next_7d",listconti=listconti1,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias1$modelo="STEPAIC"

medias2<-cruzadalin(data=data,
                    vardep="TARGET_mag_max_next_7d",listconti=listconti2,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias2$modelo="STEPBIC"

medias3<-cruzadalin(data=data,
                    vardep="TARGET_mag_max_next_7d",listconti=listconti3,
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo="STEPk15"




medias4<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti4,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias4$modelo="STEPrepAIC1"

medias5<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti5,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="STEPrepBIC1"

medias6<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti6,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias6$modelo="STEPrepBIC2"

medias7<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti7,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias7$modelo="RFE"



medias8<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti8,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias8$modelo="MMPC-0.05"


medias9<-cruzadalin(data=data,
                          vardep="TARGET_mag_max_next_7d",listconti=listconti9,
                          listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias9$modelo="MMPC-0.01"

medias10<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti10,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias10$modelo="MMPC-0.001"

medias11<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti11,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias11$modelo="Boruta"

medias12<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti12,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias12$modelo="MI"

medias13<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti13,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias13$modelo="Lasso"

medias14<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti14,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias14$modelo="RF"


medias15<-cruzadalin(data=data,
                           vardep="TARGET_mag_max_next_7d",listconti=listconti15,
                           listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias15$modelo="XGB"



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

boxplot(data=union1,col="pink",error~modelo,main="Error cuadratico")
text(x = 1:length(n_vars),
     y = aggregate(error ~ modelo, union1, max)$error ,labels = n_vars,
     col = "red",  font = 2, cex =1  )

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


tabla <- crear_tabla_presencia(union1, modelos)
htmltools::save_html(tabla, "tabla_presencia_variables_reg.html")


listconti16 <- c("target_reg_enc_30d", "roll30_mag_max",
                 "roll30_energia",	"roll30_eq_count",	"gap",
                 "roll7_energia",
                 "adj_roll30_mag_max"	,"lat",	"adj_roll30_energia"
                 ,"days_since_last_eq"	,"roll7_eq_count",	"locationSource_ci")



medias16<-cruzadalin(data=data,
                     vardep="TARGET_mag_max_next_7d",listconti=listconti16,
                     listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias16$modelo="Mas_10"


union2<-rbind(medias1,medias2,medias3,medias5, medias6,medias16)


orden <- c("STEPAIC", "STEPBIC","STEPk15", "STEPrepBIC1", "STEPrepBIC2","Mas_10")
listas_conti <- list(listconti1, listconti2, listconti3,listconti5,listconti6,
                      listconti16)
n_vars <- sapply(listas_conti, length)


union2$modelo <- factor(union2$modelo, levels = orden)  


boxplot(data=union2,col="pink",error~modelo,main="Error Cuadratico")
text(x = 1:length(n_vars),
     y = aggregate(error ~ modelo, union2, max)$error ,labels = n_vars,
     col = "red",  font = 2, cex =1  )


















