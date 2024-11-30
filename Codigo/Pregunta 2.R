# *Análisis de Correspondencias Detallado - Sostenimiento-Modalidad
# Noviembre 2024
# Carrera de Ciencia de Datos
# Estadística IV
# Autores: Francisco Flores, Sebastián Reyes, Sebastián Enríquez

# *Configuración General --------------------------------------------------
# Configuración de rutas y librerías
PATHS <- list(
  input_data = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Datos\\Base_Mineduc_2009_2024.xlsx",
  output_base = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\Pregunta2\\",
  descriptivos = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\Pregunta2\\Descriptivos\\",
  biplots = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\Pregunta2\\Biplots\\"
)

# *Definición de Años de Análisis -----------------------------------------
years <- 2010:2024

# Crear directorios si no existen
for (path in unlist(PATHS)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Crear subdirectorios para Pregunta 2
subdirs <- c("General", "Urbano", "Rural")
for (tipo in subdirs) {
  dir.create(file.path(PATHS$descriptivos, tipo), recursive = TRUE)
  dir.create(file.path(PATHS$biplots, tipo), recursive = TRUE)
}

# *Carga de Librerías -----------------------------------------------------
required_packages <- c("readxl", "dplyr", "ggplot2", "FactoMineR", 
                       "factoextra", "skimr", "tidyr", "janitor")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# *Carga de Datos ---------------------------------------------------------
datos <- read_excel(PATHS$input_data)

# *Preparación y Limpieza de Datos ----------------------------------------
datos <- datos %>%
  filter(Año %in% years) %>%
  mutate(
    Año = as.integer(Año),
    Modalidad = ifelse(is.na(Modalidad), "Desconocida", Modalidad),
    Sostenimiento = factor(Sostenimiento, levels = c("Particular", "Fiscal", 
                                                     "Fiscomisional", "Municipal", "Otro")),
    Provincia = factor(Provincia),
    Área = factor(Área, levels = c("Urbana", "Rural")),
    Modalidad = factor(Modalidad)  # Sin especificar niveles para mantener todas las categorías
  )

# Separar datos por áreas y años
datos_urbano <- datos %>% filter(Área == "Urbana")
datos_rural <- datos %>% filter(Área == "Rural")
lista_datos_year <- split(datos, datos$Año)
lista_datos_urbano <- split(datos_urbano, datos_urbano$Año)
lista_datos_rural <- split(datos_rural, datos_rural$Año)

# *Definición de Paletas de Colores ---------------------------------------
COLOR_PALETTES <- list(
  sostenimiento = c(
    "Particular" = "#87CEFA", 
    "Fiscal" = "#FF6347", 
    "Fiscomisional" = "#FFA500", 
    "Municipal" = "#90EE90", 
    "Otro" = "#DDA0DD"
  ),
  modalidad = c(
    "Presencial" = "#ADD8E6",
    "Presencial y Semipresencial" = "#FFB6C1",
    "Semipresencial" = "#FFDAB9",
    "A Distancia" = "#F0E68C",
    "Semipresencial y A Distancia" = "#DDA0DD",
    "Otros" = "#98FB98",
    "Presencial, Semipresencial y A Distancia" = "#B0C4DE"
  )
)

# *Funciones Auxiliares ---------------------------------------------------

# Función de visualización de Sostenimiento y Modalidad con escala logarítmica
plot_sostenimiento_modalidad <- function(data, title_suffix = "", area_type = "General") {
  ggplot(data, aes(x = Modalidad, fill = Sostenimiento)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = COLOR_PALETTES$sostenimiento) +
    labs(
      title = paste("Distribución de Modalidad por Sostenimiento -", area_type, title_suffix),
      x = "Modalidad", y = "Conteo (Escala Logarítmica)", fill = "Sostenimiento"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_log10(labels = scales::comma)  # Escala logarítmica
}

# *Visualización Descriptiva ----------------------------------------------
# Guardar gráficos descriptivos para General, Urbano y Rural
for (año in years) {
  if (año %in% names(lista_datos_year)) {
    # Gráficos generales, urbanos y rurales
    ggsave(
      filename = file.path(PATHS$descriptivos, "General", paste0("modalidad_sostenimiento_", año, ".png")),
      plot = plot_sostenimiento_modalidad(lista_datos_year[[as.character(año)]], paste("Año", año), "General"),
      width = 12, height = 8, bg = "white"
    )
    
    ggsave(
      filename = file.path(PATHS$descriptivos, "Urbano", paste0("modalidad_sostenimiento_", año, ".png")),
      plot = plot_sostenimiento_modalidad(lista_datos_urbano[[as.character(año)]], paste("Año", año), "Urbano"),
      width = 12, height = 8, bg = "white"
    )
    
    ggsave(
      filename = file.path(PATHS$descriptivos, "Rural", paste0("modalidad_sostenimiento_", año, ".png")),
      plot = plot_sostenimiento_modalidad(lista_datos_rural[[as.character(año)]], paste("Año", año), "Rural"),
      width = 12, height = 8, bg = "white"
    )
  }
}

# *Funciones para Análisis de Correspondencias ----------------------------

# Función para generar resumen de Análisis de Correspondencias
resumen_ac <- function(ac_result) {
  cat("Resumen del Análisis de Correspondencias:\n")
  cat("Chi-cuadrado de independencia:", ac_result$chisq, "\n")
  cat("Valor p:", ac_result$p.value, "\n\n")
  
  cat("Autovalores:\n")
  print(ac_result$eig)
  
  cat("\nContribución de filas y columnas por dimensión:\n")
  print(summary(ac_result))
}

# Función de Análisis de Correspondencias con resumen
realizar_ac_con_resumen <- function(data, cols, title_suffix, output_folder) {
  freq_matrix <- xtabs(~ ., data = data[, cols])
  
  if (sum(freq_matrix) == 0 || nrow(freq_matrix) < 2 || ncol(freq_matrix) < 2) {
    warning(paste("Matriz de frecuencias no válida para", title_suffix))
    return(NULL)
  }
  
  ac <- CA(freq_matrix, graph = FALSE)
  
  # Generar resumen y guardarlo
  sink(file.path(output_folder, paste0("resumen_", tolower(gsub(" ", "_", title_suffix)), ".txt")))
  resumen_ac(ac)
  sink()
  
  # Biplot sin restricción de escala
  biplot <- fviz_ca_biplot(ac, repel = TRUE) +
    labs(title = paste("Biplot de", title_suffix), 
         x = "Dimensión 1", 
         y = "Dimensión 2") +
    theme_minimal()
  
  ggsave(
    filename = file.path(output_folder, paste0("biplot_", tolower(gsub(" ", "_", title_suffix)), ".png")),
    plot = biplot, width = 10, height = 8, bg = "white"
  )
  
  return(ac)
}

# *Análisis de Correspondencias -------------------------------------------
for (año in years) {
  if (año %in% names(lista_datos_year)) {
    # Análisis general
    realizar_ac_con_resumen(
      lista_datos_year[[as.character(año)]], 
      c("Sostenimiento", "Modalidad"), 
      paste("Sostenimiento y Modalidad - Año", año),
      file.path(PATHS$biplots, "General")
    )
    
    # Análisis urbano
    realizar_ac_con_resumen(
      lista_datos_urbano[[as.character(año)]], 
      c("Sostenimiento", "Modalidad"), 
      paste("Sostenimiento y Modalidad Urbano - Año", año),
      file.path(PATHS$biplots, "Urbano")
    )
    
    # Análisis rural
    realizar_ac_con_resumen(
      lista_datos_rural[[as.character(año)]], 
      c("Sostenimiento", "Modalidad"), 
      paste("Sostenimiento y Modalidad Rural - Año", año),
      file.path(PATHS$biplots, "Rural")
    )
  }
}

# *Conclusión ------------------------------------------------------------
print("Análisis de Correspondencias Sostenimiento-Modalidad completado. Resultados guardados en las carpetas respectivas.")