cruzadaavnnet<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=FALSE)
    
  { 
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
      
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = TRUE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }

cruzadalin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5)
    
  { 
    
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
    
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    lineal<- train(formu,data=databis,
                   method="lm",trControl=control)
    
    print(lineal$results)
    
    preditest<-lineal$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }


