crear_tabla_modelos_binaria_auc<- function(union1, modelos) {
  library(kableExtra)
  library(dplyr)
  
  # IMPORTANTE: Mantener el orden de aparición, no alfabético
  union1$modelo <- as.character(union1$modelo)  # Convertir a character
  metodos <- unique(union1$modelo)  # Esto mantiene el orden de aparición
  
  # Calcular estadísticas de error para cada método
  stats_error <- sapply(metodos, function(m) {
    errores <- union1$auc[union1$modelo == m]
    c(media = mean(errores, na.rm = TRUE),
      sd = sd(errores, na.rm = TRUE))
  })
  
  # Crear el data frame
  tabla <- data.frame(
    Método = metodos,
    Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) {
        paste(modelos[[m]], collapse = " + ")
      } else {
        ""
      }
    }),
    N_Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) {
        length(modelos[[m]])
      } else {
        0
      }
    }),
    Error_CV = sprintf("%.4f ± %.4f", stats_error["media",], stats_error["sd",]),
    stringsAsFactors = FALSE
  )
  
  # Crear tabla formateada
  tabla_formateada <- tabla %>%
    kable(
      format = "html",
      align = c("l", "l", "c", "c"),
      col.names = c("Método de Selección", "Variables Seleccionadas", 
                    "Nº Variables", "AUC CV (media ± sd)"),
      caption = "Resultados de Métodos de Selección de Variables",
      row.names = FALSE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    row_spec(0, bold = TRUE, background = "#4A90E2", color = "white") %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2, width = "8cm", monospace = TRUE) %>%
    column_spec(3, width = "2.5cm", background = "#FFFACD") %>%
    column_spec(4, width = "4cm", background = "#E8F8E8")
  
  return(tabla_formateada)
}


crear_tabla_modelos_binaria_tasa<- function(union1, modelos) {
  library(kableExtra)
  library(dplyr)
  
  # IMPORTANTE: Mantener el orden de aparición, no alfabético
  union1$modelo <- as.character(union1$modelo)  # Convertir a character
  metodos <- unique(union1$modelo)  # Esto mantiene el orden de aparición
  
  # Calcular estadísticas de error para cada método
  stats_error <- sapply(metodos, function(m) {
    errores <- union1$tasa[union1$modelo == m]
    c(media = mean(errores, na.rm = TRUE),
      sd = sd(errores, na.rm = TRUE))
  })
  
  # Crear el data frame
  tabla <- data.frame(
    Método = metodos,
    Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) {
        paste(modelos[[m]], collapse = " + ")
      } else {
        ""
      }
    }),
    N_Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) {
        length(modelos[[m]])
      } else {
        0
      }
    }),
    Error_CV = sprintf("%.4f ± %.4f", stats_error["media",], stats_error["sd",]),
    stringsAsFactors = FALSE
  )
  
  # Crear tabla formateada
  tabla_formateada <- tabla %>%
    kable(
      format = "html",
      align = c("l", "l", "c", "c"),
      col.names = c("Método de Selección", "Variables Seleccionadas", 
                    "Nº Variables", "TASA FALLOS CV"),
      caption = "Resultados de Métodos de Selección de Variables",
      row.names = FALSE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    row_spec(0, bold = TRUE, background = "#4A90E2", color = "white") %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2, width = "8cm", monospace = TRUE) %>%
    column_spec(3, width = "2.5cm", background = "#FFFACD") %>%
    column_spec(4, width = "4cm", background = "#E8F8E8")
  
  return(tabla_formateada)
}



# Uso:
# tabla_formateada <- crear_tabla_modelos(union1, modelos)
# tabla_formateada


# crear_tabla_modelos(union1, modelos)



aumentar_tabla_modelos_multiple_auc <- function(union1, modelos, 
                                            lista_medias_propios = NULL, 
                                            lista_listas_propias = NULL,
                                            lista_nombres_propios = NULL) {
  library(kableExtra)
  library(dplyr)
  
  # Datos originales
  metodos <- unique(union1$modelo)
  
  stats_error <- sapply(metodos, function(m) {
    errores <- union1$auc[union1$modelo == m]
    c(media = mean(errores, na.rm = TRUE),
      sd = sd(errores, na.rm = TRUE))
  })
  
  tabla_original <- data.frame(
    Método = metodos,
    Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) {
        paste(modelos[[m]], collapse = " + ")
      } else ""
    }),
    N_Variables = sapply(metodos, function(m) {
      if(m %in% names(modelos)) length(modelos[[m]]) else 0
    }),
    Error_Media = stats_error["media",],
    Error_SD = stats_error["sd",],
    stringsAsFactors = FALSE
  )
  
  # Añadir modelos propios
  if(!is.null(lista_medias_propios) && !is.null(lista_listas_propias)) {
    if(is.null(lista_nombres_propios)) {
      lista_nombres_propios <- sapply(lista_medias_propios, 
                                      function(x) unique(x$modelo)[1])
    }
    
    for(i in seq_along(lista_medias_propios)) {
      nueva_fila <- data.frame(
        Método = lista_nombres_propios[i],
        Variables = paste(lista_listas_propias[[i]], collapse = " + "),
        N_Variables = length(lista_listas_propias[[i]]),
        Error_Media = mean(lista_medias_propios[[i]]$error, na.rm = TRUE),
        Error_SD = sd(lista_medias_propios[[i]]$error, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      tabla_original <- rbind(tabla_original, nueva_fila)
    }
  }
  
  # Formatear
  tabla_original$Error_CV <- sprintf("%.4f ± %.4f", 
                                     tabla_original$Error_Media, 
                                     tabla_original$Error_SD)
  
  # Tabla formateada
  tabla_formateada <- tabla_original %>%
    select(Método, Variables, N_Variables, Error_CV) %>%
    kable(
      format = "html",
      align = c("l", "l", "c", "c"),
      col.names = c("Método de Selección", "Variables Seleccionadas", 
                    "Nº Variables", "Error CV (media ± sd)"),
      caption = "Resultados de Métodos de Selección de Variables"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    row_spec(0, bold = TRUE, background = "#4A90E2", color = "white") %>%
    column_spec(1, bold = TRUE, width = "4cm") %>%
    column_spec(2, width = "8cm", monospace = TRUE) %>%
    column_spec(3, width = "2.5cm", background = "#FFFACD") %>%
    column_spec(4, width = "4cm", background = "#E8F8E8")
  
  return(tabla_formateada)
}

