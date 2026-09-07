# Función para realizar la selección de variables y comparación de modelos
comparar_modelos_binaria<- function(data, vardep, num_vars_rfe=6,
                             k_stepAIC = 2, k_general = 14,sinicio=12345,sfinal=12385, max_k_mmpc = 3, max_vars_rfe = 20,t1=0.003,t2=0.01,t3=0.05,
                             grupos=4,semilla=12345,repe=20) {
  
  library(MASS)
  library(MXM)
  library(car)
  library(parallel)
  library(doParallel)
  library(caret)
  library(ggplot2)
  library(pROC)
  source("funcion steprepetido binaria.R")
  source("cruzadas avnnet y log binaria.R")

  start_time <- Sys.time()  
  data<-as.data.frame(unclass(data))
  
  data[[vardep]]<-as.character(data[[vardep]])
  
  formu1<-formula(paste("factor(",vardep,")~.",sep=""))
  formu2<-formula(paste("factor(",vardep,")~1",sep=""))
  
  lista <- setdiff(names(data), vardep)
  x <- data[, lista]
  y <- data[, vardep]
  
  y<-as.factor(y)
  
  
  k_BIC = log(nrow(data))
  
  cat("Ejecutando STEPWISE AIC...\n")
  # STEPWISE AIC
  
  full<-glm(formu1,data=data,family = binomial(link="logit"))
  null<-glm(formu2,data=data,family = binomial(link="logit"))
  
  selec1<-stepAIC(null,scope=list(upper=full),
                  direction="both",family = binomial(link="logit"),trace=FALSE)
  
  listconti1 <- setdiff(names(selec1[[1]]), "(Intercept)")
  
  cat("Ejecutando STEPWISE BIC...\n")
  # STEPWISE BIC
  
  selec2 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = k_BIC)
  listconti2 <- setdiff(names(selec2[[1]]), "(Intercept)")
  
  cat("Ejecutando STEPWISE con k general...\n")
  # STEPWISE con k general
  selec3 <- stepAIC(null, scope = list(upper = full), direction = "both", trace = FALSE, k = k_general)
  listconti3 <- setdiff(names(selec3[[1]]), "(Intercept)")
  
  cat("Ejecutando STEPWISE repetido AIC...\n")
  
  lis<-steprepetidobinaria(data=data,vardep=vardep,
                      listconti=lista,
                      sinicio=sinicio,sfinal=sfinal,porcen=0.8,criterio="AIC")
  listconti4<-dput(lis[[2]][[1]])
  
  lis<-steprepetidobinaria(data=data,vardep=vardep,
                      listconti=lista,
                      sinicio=sinicio,sfinal=sfinal,porcen=0.8,criterio="BIC")
  listconti5<-dput(lis[[2]][[1]])
  listconti6 <- tryCatch({
    lis[[2]][[2]]
  }, error = function(e) {
    listconti5
  })
  
  
    cat("Ejecutando RFE...\n")
  # RFE
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 4)
  results <- rfe(x, y, sizes = c(1:num_vars_rfe), rfeControl = control)
  listconti7 <- results$optVariables[1:num_vars_rfe]
  listconti7 <- listconti7[!is.na(listconti7)]
  
  cat("Ejecutando MMPC...\n")
  # MMPC
  
  mmpc2 <- MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t1, test = "testIndLogistic")
  listconti8 <- names(x[, c(mmpc2$selectedVars)])
  
  mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t2, test = "testIndLogistic")
  listconti9 <- names(x[, c(mmpc2$selectedVars)])
  
  mmpc2 <-MXM::mmpc2(y, x, max_k = max_k_mmpc, threshold = t3, test = "testIndLogistic")
  listconti10 <- names(x[, c(mmpc2$selectedVars)])
  cat("Fin MMPC...\n")  
  
  # Comparaciones vía CV y boxplot

  medias1 <- cruzadalogistica(data=data, vardep=vardep, listconti=listconti1, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos )
  cat("fin medias1 \n")
  medias2 <- cruzadalogistica(data, vardep, listconti=listconti2, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias2 \n")
  medias3 <- cruzadalogistica(data, vardep, listconti=listconti3, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias3 \n")
  medias4 <- cruzadalogistica(data, vardep, listconti=listconti4, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias4 \n")
  medias5 <- cruzadalogistica(data, vardep, listconti=listconti5, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias5 \n")
  medias6 <- cruzadalogistica(data, vardep, listconti=listconti6, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias6 \n")
  medias7 <- cruzadalogistica(data, vardep, listconti=listconti7, listclass=c(""),repe = repe, sinicio = semilla,grupos=grupos)
  cat("fin medias7 \n")
  medias8 <- cruzadalogistica(data, vardep, listconti=listconti8, listclass=c(""),repe = repe, sinicio = semilla)
  cat("fin medias8 \n")
  medias9 <- cruzadalogistica(data, vardep, listconti=listconti9, listclass=c(""),repe = repe, sinicio = semilla)
  cat("fin medias9 \n")
  medias10 <- cruzadalogistica(data, vardep, listconti=listconti10, listclass=c(""),repe = repe, sinicio = semilla)
  
  modelos<-c("STEPAIC","STEPBIC","STEPk","STEPrepAIC","STEPrepBIC1","STEPrepBIC2","RFE","MMPC003","MMPC01","MMPC05")
  
  medias1$modelo<-modelos[1]
  medias2$modelo<-modelos[2]
  medias3$modelo<-modelos[3]
  medias4$modelo<-modelos[4]
  medias5$modelo<-modelos[5]
  medias6$modelo<-modelos[6]
  medias7$modelo<-modelos[7]
  medias8$modelo<-modelos[8]
  medias9$modelo<-modelos[9]    
  medias10$modelo<-modelos[10]
  
  cat("fin mediastodos \n")  
  union1 <- rbind(medias1, medias2, medias3, medias4, medias5,medias6, medias7, medias8, medias9, medias10)

  orden <- c("STEPAIC", "STEPBIC","STEPk", "STEPrepAIC", "STEPrepBIC1", 
             "STEPrepBIC2", "RFE", "MMPC003", "MMPC01", "MMPC05")
  listas_conti <- list(listconti1, listconti2, listconti3, listconti4, 
                       listconti5, listconti6, listconti7, listconti8, listconti9,listconti10)
  n_vars <- sapply(listas_conti, length)
  
  union1$modelo <- factor(union1$modelo, levels = orden)  
    max_error <- max(union1$auc)
  
  # Con colores:
  num_modelos <- length(unique(union1$modelo))
  colores <- rainbow(num_modelos)
  
  rango_auc <- max(union1$auc) - min(union1$auc)
  offset <- rango_auc * 0.05  # 5% del rango
  
  
  par(cex.axis=0.8, las=2)
  grafi1<-boxplot(data=union1,main="AUC",
          auc~modelo,xlab="",ylim = c(min(union1$auc), max_error * 1.10),col = colores)
  
  text(x = 1:length(n_vars),
       y = aggregate(auc ~ modelo, union1, max)$auc + offset,labels = n_vars,
       col = "red",  font = 2, cex =1  )
  
  max_error <- max(union1$tasa)
  
  grafi2<-boxplot(data=union1,main="TASA DE FALLOS",
                  tasa~modelo,xlab="",ylim = c(min(union1$tasa), max_error * 1.10),col = colores)
  
  
  rango_tasa <- max(union1$tasa) - min(union1$tasa)
  offset <- rango_tasa * 0.05  # 5% del rango
  
  text(x = 1:length(n_vars),
       y = aggregate(tasa ~ modelo, union1, max)$tasa + offset,
       labels = n_vars,
       col = "red",  font = 2, cex = 1)
  

  
    max_error <- max(union1$sensi)
  
  grafi3<-boxplot(data=union1,main="SENSITIVIDAD",
                  sensi~modelo,xlab="",ylim = c(min(union1$sensi), max_error * 1.10),col = colores)
  
  
  rango_tasa <- max(union1$sensi) - min(union1$sensi)
  offset <- rango_tasa * 0.05  # 5% del rango
  
  text(x = 1:length(n_vars),
       y = aggregate(sensi ~ modelo, union1, max)$sensi + offset,
       labels = n_vars,
       col = "red",  font = 2, cex = 1)
  
  
  
    
  
  
  resultados <- list(medias = union1,boxplot = grafi1,listconti = list(
    STEPAIC = listconti1,
    STEPBIC = listconti2,
    STEPk = listconti3,
    STEPrepAIC = listconti4,
    STEPrepBIC1 = listconti5,
    STEPrepBIC2 = listconti6,
    RFE = listconti7,
    MMPC003 = listconti8,
    MMPC01 = listconti9,
    MMPC05 = listconti10
  ))
  
  end_time <- Sys.time()
  cat("Tiempo total de ejecución: ", end_time - start_time, "\n")
  
  return(resultados)
  
}

# 
# load("germanbis.Rda")
# data<-germanbis
# vardep<-"bad"
# result<-comparar_modelos_binaria(data, vardep, num_vars_rfe=7,
#   k_stepAIC = 2, k_general = 14,sinicio=12345,sfinal=12355,
#   max_k_mmpc = 3, max_vars_rfe = 20,t1=0.003,t2=0.01,t3=0.05,
#   grupos=4,semilla=12345,repe=50)
# union1<-result[[1]]
# grafi1<-result[[2]]
# print(grafi1)
# modelos<-result[[3]]
# result$listconti
# crear_tabla_modelos_binaria_auc(union1, modelos)
# crear_tabla_modelos_binaria_tasa(union1, modelos)
# tabla <- crear_tabla_presencia(union1, modelos)
# htmltools::save_html(tabla, "tabla_presencia_variables.html")


 
