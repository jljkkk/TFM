# *********************************
# CRUZADA LOGISTICA
# *********************************
# Se requiere que la variable objetivo vardep esté categorizada como ("Yes","No"), 
# con mayúscula inicial y sin espacios, y que el nivel positivo sea "Yes". 
# 
# con ifelse se puede crear esta variable a partir de la original, por ejemplo:
# data$chd<-ifelse(data$chd==1,"Yes","No")

cruzadalogistica <- function(data=data,vardep=NULL,
                             listconti=NULL,listclass=NULL,grupos=4,sinicio=1234,repe=5)
{
  library(dummies)
  library(MASS)
  library(reshape)
  library(caret)
  library(pROC)
  
  if (any(listclass==c(""))==FALSE)
  {
    for (i in 1:dim(array(listclass))) {
      numindi<-which(names(data)==listclass[[i]])
      data[,numindi]<-as.character(data[,numindi])
      data[,numindi]<-as.factor(data[,numindi])
    }
  }   
  
  # data[,vardep]<-as.factor(data[,vardep])
  
  # Creo la formula para la logistica
  
  if (any(listclass==c(""))==FALSE)
  {
    koko<-c(listconti,listclass)
  }  else   {
    koko<-c(listconti)
  }
  
  modelo<-paste(koko,sep="",collapse="+")
  formu<-formula(paste(factor(vardep),"~",modelo,sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                        savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  regresion <- train(formu,data=data,weights = pesos_vector,
                     trControl=control,method="glm",family = binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
  }
  
  sensit<-function(x,y) {
    confu<-confusionMatrix(x,y,positive="Yes")
    sensi<-confu[["byClass"]][["Sensitivity"]]
    return(sensi)
  }
  
  
  # Aplicamos función sobre cada Repetición
  
  
  tabla<-table(preditest$Rep)
  listarep<-c(names(tabla))
  medias<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    tasa=1-tasafallos(paso1$pred,paso1$obs)  
    medias<-rbind(medias,tasa)
  }
  names(medias)<-"tasa"
  
  
  # CalculamoS AUC  por cada Repetición de cv 
  # Definimnos función
  
  auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
  }
  
  # Aplicamos función sobre cada Repetición
  
  mediasbis<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
    auc <- suppressMessages(roc(paso1$obs, paso1$Yes)$auc)    
    mediasbis<-rbind(mediasbis,auc)
  }
  names(mediasbis)<-"auc"
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  mediasbis<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
    sensi <- sensit(paso1$pred,paso1$obs)  
    mediasbis<-rbind(mediasbis,sensi)
  }
  names(mediasbis)<-"sensi"
  
  # Unimos la info de auc y de tasafallos
  
  medias$sensi<-mediasbis$sensi
  
  
  mediasbis<-data.frame()
  for (repi in listarep) {
    paso1<-preditest[which(preditest$Rep==repi),]
    paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
    confu<-confusionMatrix(paso1$pred,paso1$obs,positive="Yes")
    espe<-confu[["byClass"]][["Specificity"]]
    mediasbis<-rbind(mediasbis,espe)
  }
  names(mediasbis)<-"espe"
  
  # Unimos la info de auc y de tasafallos
  
  medias$espe<-mediasbis$espe
  
  return(medias)
  
}




# *********************************
# CRUZADA avNNet
# **************


cruzadaavnnetbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=F)
  { 
    
    # Preparación del archivo
    
    library(caret)
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    
    # Identificar variables binarias (0-1) que NO debemos estandarizar
    variables_binarias <- c()
    
    for (var in listconti) {
      valores_unicos <- unique(na.omit(databis[[var]]))
      
      # Verificar si la variable es binaria (solo contiene 0 y 1)
      if (length(valores_unicos) == 2 && 
          all(sort(valores_unicos) == c(0, 1))) {
        variables_binarias <- c(variables_binarias, var)
      }
    }
    
    # Crear lista de variables a estandarizar (excluyendo las binarias)
    variables_a_estandarizar <- setdiff(listconti, variables_binarias)
    
    # Estandarizar solo las variables no binarias
    if (length(variables_a_estandarizar) > 0) {
      databis[, variables_a_estandarizar] <- scale(databis[, variables_a_estandarizar])
    }
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = FALSE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    sensit<-function(x,y) {
      confu<-confusionMatrix(x,y,positive="Yes")
      sensi<-confu[["byClass"]][["Sensitivity"]]
      return(sensi)
    }
    
    
    # Aplicamos función sobre cada Repetición
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      tasa=1-tasafallos(paso1$pred,paso1$obs)  
      medias<-rbind(medias,tasa)
    }
    names(medias)<-"tasa"
    
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
      auc <- suppressMessages(roc(paso1$obs, paso1$Yes)$auc)    
      mediasbis<-rbind(mediasbis,auc)
    }
    names(mediasbis)<-"auc"
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
      sensi <- sensit(paso1$pred,paso1$obs)  
      mediasbis<-rbind(mediasbis,sensi)
    }
    names(mediasbis)<-"sensi"
    
    # Unimos la info de auc y de tasafallos
    
    medias$sensi<-mediasbis$sensi
    
    
    mediasbis<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      paso1$obs <- factor(paso1$obs, levels = c("No", "Yes"))
      confu<-confusionMatrix(paso1$pred,paso1$obs,positive="Yes")
      espe<-confu[["byClass"]][["Specificity"]]
      mediasbis<-rbind(mediasbis,espe)
    }
    names(mediasbis)<-"espe"
    
    # Unimos la info de auc y de tasafallos
    
    medias$espe<-mediasbis$espe
    
    return(medias)
    
  }


## Ejemplo de utilización cruzada LOGISTICA Y AVNNET

# load ("saheartbis.Rda")
#
# medias1<-cruzadalogistica(data=saheartbis,
#  vardep="chd",listconti=c("sbp", "tobacco", "ldl",
#   "adiposity",  "obesity", "famhist.Absent"),
#  listclass=c(""), grupos=4,sinicio=1234,repe=5)
#
# medias1$modelo="Logistica"
#
#
# medias2<-cruzadaavnnetbin(data=saheartbis,
#  vardep="chd",listconti=c("sbp", "tobacco",
#   "ldl", "adiposity",  "obesity", "famhist.Absent"),
#  listclass=c(""),grupos=4,sinicio=1234,repe=5,
#   size=c(5),decay=c(0.1),repeticiones=5,itera=200)
#
# medias2$modelo="avnnet"
#
# union1<-rbind(medias1,medias2)
#
# par(cex.axis=0.5)
# boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
# boxplot(data=union1,auc~modelo,main="AUC")
# 
# 
# 
