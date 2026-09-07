crear_tabla_presencia <- function(union1, modelos, compacta = F, 
                                  rotar_nombres = F, 
                                  tamaño_fuente = 16,
                                  encabezado_bold = F) {
  library(dplyr)
  library(kableExtra)
  
  # Preparar datos
  union1$modelo <- as.character(union1$modelo)
  metodos <- unique(union1$modelo)
  todas_vars <- unique(unlist(modelos))
  
  matriz_presencia <- matrix(0, nrow = length(metodos), ncol = length(todas_vars))
  rownames(matriz_presencia) <- metodos
  colnames(matriz_presencia) <- todas_vars
  
  for(i in 1:length(metodos)) {
    metodo <- metodos[i]
    if(metodo %in% names(modelos)) {
      vars_presentes <- modelos[[metodo]]
      matriz_presencia[i, vars_presentes] <- 1
    }
  }
  
  frecuencias <- colSums(matriz_presencia)
  orden_vars <- order(frecuencias, decreasing = TRUE)
  matriz_presencia <- matriz_presencia[, orden_vars]
  
  df_tabla <- as.data.frame(matriz_presencia)
  totales <- colSums(df_tabla)
  df_tabla <- rbind(df_tabla, totales)
  rownames(df_tabla)[nrow(df_tabla)] <- "TOTAL"
  
  # Aplicar colores directamente con cell_spec
  df_tabla_coloreada <- df_tabla
  for(i in 1:(nrow(df_tabla)-1)) {
    for(j in 1:ncol(df_tabla)) {
      color <- ifelse(df_tabla[i, j] == 1, "#B4D7E8", "white")
      df_tabla_coloreada[i, j] <- cell_spec(
        df_tabla[i, j], 
        background = color,
        color = "black",
        bold = FALSE,
        align = "c"
      )
    }
  }
  
  # Colorear la fila TOTAL
  for(j in 1:ncol(df_tabla)) {
    df_tabla_coloreada[nrow(df_tabla), j] <- cell_spec(
      df_tabla[nrow(df_tabla), j],
      background = "#f0f0f0",
      color = "black",
      bold = TRUE,
      align = "c"
    )
  }
  
  # CSS condicional según modo compacto
  if(compacta) {
    padding_val <- "3px 4px"
    height_val <- ifelse(rotar_nombres, "150px", "30px")
    width_header <- ifelse(rotar_nombres, "max-width: 25px", "max-width: 100px")
    width_cell <- "td { min-width: 25px !important; max-width: 25px !important; }"
  } else {
    padding_val <- "8px"
    height_val <- ifelse(rotar_nombres, "150px", "auto")
    width_header <- ""
    width_cell <- ""
  }
  
  writing_mode <- ifelse(rotar_nombres, "vertical-rl", "horizontal-tb")
  
  # Controlar font-weight según parámetro
  font_weight_val <- ifelse(encabezado_bold, "bold", "normal")
  
  css_base <- sprintf('
    <style>
      table { 
        border-collapse: collapse !important; 
        border: 2px solid #4472C4 !important;
        font-size: %dpx !important;
        margin: 0 auto;
      }
      th, td { 
        border: 1px solid #999 !important; 
        padding: %s !important;
        white-space: nowrap;
      }
      thead th {
        writing-mode: %s !important;
        text-orientation: mixed !important;
        vertical-align: bottom !important;
        height: %s !important;
        %s !important;
        background-color: #4472C4 !important;
        color: white !important;
        font-weight: %s !important;
      }
      tbody th {
        background-color: #f0f0f0 !important;
        font-weight: bold !important;
        border-right: 2px solid #999 !important;
      }
      tbody tr:last-child {
        border-top: 2px solid #4472C4 !important;
      }
      %s
      @media print {
        * { -webkit-print-color-adjust: exact !important; 
            print-color-adjust: exact !important; 
            color-adjust: exact !important; }
      }
    </style>',
                      tamaño_fuente, padding_val, writing_mode, height_val, width_header, 
                      font_weight_val, width_cell
  )
  
  tabla_html <- df_tabla_coloreada %>%
    kbl(align = "c", escape = FALSE, format = "html") %>%
    kable_styling(full_width = FALSE) %>%
    row_spec(0, background = "#4472C4", color = "white") %>%  # Sin bold aquí
    column_spec(1, bold = TRUE, background = "#f0f0f0", border_right = TRUE)
  
  tabla_final <- paste0(css_base, as.character(tabla_html))
  
  return(htmltools::HTML(tabla_final))
}


# # USO:
# 
# 
# tabla_normal <- crear_tabla_presencia(union1, modelos)
# htmltools::save_html(tabla_normal, "tabla_color.html")
# 
# # Para MUCHAS variables (compacta):
# tabla_compacta <- crear_tabla_presencia(union1, modelos, 
#                                         compacta = TRUE, 
#                                         rotar_nombres = TRUE,
#                                         tamaño_fuente = 9)
# htmltools::save_html(tabla_compacta, "tabla_color.html")
# 
# 
# # Para POCAS variables:
# tabla_normal <- crear_tabla_presencia(union1, modelos, 
#                                       compacta = FALSE, 
#                                       rotar_nombres = FALSE,
#                                       tamaño_fuente = 16,
#                                       encabezado_bold = F)
# 
# htmltools::save_html(tabla_normal, "tabla_color.html")
# 
# # Intermedio (solo rotar nombres, sin compactar):
# tabla_intermedia <- crear_tabla_presencia(union1, modelos, 
#                                           compacta = FALSE, 
#                                           rotar_nombres = TRUE,
#                                           tamaño_fuente = 14,encabezado_bold = T)
# 
# htmltools::save_html(tabla_intermedia, "tabla_color.html")



