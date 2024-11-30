# *Análisis de Correspondencias Detallado - Provincia-Sostenimiento
# Noviembre 2024
# Carrera de Ciencia de Datos
# Estadística IV
# Autores: Francisco Flores, Sebastián Reyes, Sebastián Enríquez

# *Configuración General --------------------------------------------------
# Configuración de rutas y librerías
PATHS <- list(
  input_data = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Datos\\Base_Mineduc_2009_2024.xlsx",
  output_base = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\",
  pregunta1_descriptivos = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\Pregunta1\\Descriptivos\\",
  pregunta1_biplots = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\AC_Mini_Educ\\Pregunta1\\Biplots\\"
)

# *Definición de Años de Análisis -----------------------------------------
years <- 2010:2024

# Crear directorios si no existen
for (path in unlist(PATHS)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Crear subdirectorios para Pregunta 1
subdirs <- c("General", "Urbano", "Rural")
for (tipo in subdirs) {
  dir.create(file.path(PATHS$pregunta1_descriptivos, tipo), recursive = TRUE)
  dir.create(file.path(PATHS$pregunta1_biplots, tipo), recursive = TRUE)
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
    Modalidad = factor(Modalidad, levels = c("Presencial", "Semipresencial", "A Distancia", "Otros"))
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
  area = c("Urbana" = "#8B0000", "Rural" = "#00008B")
)

# *Funciones Auxiliares ---------------------------------------------------

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
  
  # Biplot con escala fija
  biplot <- fviz_ca_biplot(ac, repel = TRUE) +
    labs(title = paste("Biplot de", title_suffix), 
         x = "Dimensión 1", 
         y = "Dimensión 2") +
    theme_minimal() +
    coord_fixed()  # Escala fija
  
  ggsave(
    filename = file.path(output_folder, paste0("biplot_", tolower(gsub(" ", "_", title_suffix)), ".png")),
    plot = biplot, width = 10, height = 8, bg = "white"
  )
  
  return(ac)
}

# Función de visualización de Provincia-Sostenimiento
plot_provincia_sostenimiento <- function(data, title_suffix = "", area_type = "General") {
  ggplot(data, aes(x = Sostenimiento, fill = Sostenimiento)) +
    geom_bar() +
    facet_wrap(~ Provincia, scales = "fixed") +  # Escala fija
    scale_fill_manual(values = COLOR_PALETTES$sostenimiento) +
    labs(
      title = paste("Distribución de Sostenimiento por Provincia -", area_type, title_suffix),
      x = "Sostenimiento", y = "Conteo", fill = "Sostenimiento"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(labels = scales::comma)
}

# *Visualización Descriptiva ----------------------------------------------
for (año in years) {
  if (año %in% names(lista_datos_year)) {
    # Gráficos generales, urbanos y rurales
    ggsave(
      filename = file.path(PATHS$pregunta1_descriptivos, "General", paste0("provincia_sostenimiento_", año, ".png")),
      plot = plot_provincia_sostenimiento(lista_datos_year[[as.character(año)]], paste("Año", año), "General"),
      width = 12, height = 8, bg = "white"
    )
    
    ggsave(
      filename = file.path(PATHS$pregunta1_descriptivos, "Urbano", paste0("provincia_sostenimiento_", año, ".png")),
      plot = plot_provincia_sostenimiento(lista_datos_urbano[[as.character(año)]], paste("Año", año), "Urbano"),
      width = 12, height = 8, bg = "white"
    )
    
    ggsave(
      filename = file.path(PATHS$pregunta1_descriptivos, "Rural", paste0("provincia_sostenimiento_", año, ".png")),
      plot = plot_provincia_sostenimiento(lista_datos_rural[[as.character(año)]], paste("Año", año), "Rural"),
      width = 12, height = 8, bg = "white"
    )
  }
}

# *Análisis de Correspondencias -------------------------------------------
for (año in years) {
  if (año %in% names(lista_datos_year)) {
    # Análisis General
    realizar_ac_con_resumen(
      lista_datos_year[[as.character(año)]], 
      c("Provincia", "Sostenimiento"), 
      paste("Provincia y Sostenimiento - Año", año),
      file.path(PATHS$pregunta1_biplots, "General")
    )
    
    # Análisis Urbano
    realizar_ac_con_resumen(
      lista_datos_urbano[[as.character(año)]], 
      c("Provincia", "Sostenimiento"), 
      paste("Provincia y Sostenimiento Urbano - Año", año),
      file.path(PATHS$pregunta1_biplots, "Urbano")
    )
    
    # Análisis Rural
    realizar_ac_con_resumen(
      lista_datos_rural[[as.character(año)]], 
      c("Provincia", "Sostenimiento"), 
      paste("Provincia y Sostenimiento Rural - Año", año),
      file.path(PATHS$pregunta1_biplots, "Rural")
    )
  }
}

# *Conclusión ------------------------------------------------------------
print("Análisis de Correspondencias Provincia-Sostenimiento completado. Resultados guardados en las carpetas respectivas.")