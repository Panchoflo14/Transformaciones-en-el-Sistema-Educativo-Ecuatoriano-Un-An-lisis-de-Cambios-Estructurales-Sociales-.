# *Análisis de Cambios en Instituciones Educativas por Tamaño y Área
# Noviembre 2024
# Carrera de Ciencia de Datos
# Estadística IV
# Autores: Francisco Flores, Sebastián Reyes, Sebastián Enríquez

# *Configuración General -------------------------------------------------
# *Configuración de rutas y librerías
PATHS <- list(
  input_data = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Datos\\Base_Mineduc_2009_2024.xlsx",
  output_base = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Clusters_Mini_Educ\\Pregunta3\\",
  descriptivos = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Clusters_Mini_Educ\\Pregunta3\\Descriptivos\\",
  clusters = "C:\\Users\\panch\\OneDrive\\Documents\\PUCE\\Estadistica IV\\Trabajo Final\\Clusters_Mini_Educ\\Pregunta3\\Clusters\\"
)

# *Definición de Años de Análisis -----------------------------------------
years <- 2010:2024

# *Creación de Directorios -----------------------------------------------
# Crear directorios 
for (path in c(PATHS$output_base)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Crear subdirectorios para Descriptivos y Clusters
subdirs <- c("General", "Urbano", "Rural")
for (tipo in subdirs) {
  # Solo para No AC
  dir.create(file.path(PATHS$descriptivos, tipo), recursive = TRUE)
  
  # Crear subcarpetas para clusters
  dir.create(file.path(PATHS$clusters, tipo, "boxplots"), recursive = TRUE)
  dir.create(file.path(PATHS$clusters, tipo, "distribucion"), recursive = TRUE)
}

# *Carga de Librerías -----------------------------------------------------
required_packages <- c("readxl", "dplyr", "ggplot2", "tidyr", 
                       "skimr", "purrr", "scales")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# *Carga y Preparación de Datos -------------------------------------------
# *Cargar datos
data <- read_excel(PATHS$input_data)

# *Limpieza y Transformación de Datos
data <- data %>%
  filter(Año %in% years) %>%
  mutate(
    Total_Estudiantes = n_3_4 + n_basica_inicio + n_bachi_inicio,
    Área = factor(Área, levels = c("Urbana", "Rural"))
  )

# *Separación de Datos por Año y Área --------------------------------------
lista_datos_year <- split(data, data$Año)
lista_datos_urbano <- split(data[data$Área == "Urbana", ], data[data$Área == "Urbana", ]$Año)
lista_datos_rural <- split(data[data$Área == "Rural", ], data[data$Área == "Rural", ]$Año)

# *Funciones Auxiliares ---------------------------------------------------
# *Función para guardar gráficos
save_plot <- function(plot, filename, folder_path, width = 12, height = 8) {
  # Asegurarse de que la carpeta exista
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  ggsave(
    filename = file.path(folder_path, filename),
    plot = plot,
    width = width, 
    height = height, 
    bg = "white"
  )
}

# *Función de Clustering Mejorada
realizar_clustering <- function(data, area_type = "General", año = NULL) {
  set.seed(123)
  
  # Verificar si hay datos suficientes
  if (nrow(data) < 10) {
    warning(paste("Datos insuficientes para clustering en", area_type))
    return(NULL)
  }
  
  # Seleccionar rutas
  descriptivos_path <- PATHS$descriptivos
  clusters_path <- PATHS$clusters
  
  # Escalar variables
  data_scaled <- data %>%
    select(n_3_4, n_basica_inicio, n_bachi_inicio, Total_Estudiantes) %>%
    scale()
  
  # Realizar clustering
  clusters <- kmeans(data_scaled, centers = 3, nstart = 25)
  
  # Añadir cluster al dataset original
  data$Cluster <- clusters$cluster
  
  # *Gráfico de Distribución de Clusters (Boxplot)
  plot_clusters <- ggplot(data, aes(x = factor(Cluster), y = Total_Estudiantes, fill = Área)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Distribución de Clusters -", area_type, 
                    ifelse(!is.null(año), paste("- Año", año), "")),
      x = "Cluster", 
      y = "Total de Estudiantes", 
      fill = "Área"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
  
  # Guardar gráfico de clusters (boxplot)
  save_plot(
    plot_clusters, 
    paste0("boxplot_clusters_", tolower(area_type), 
           ifelse(!is.null(año), paste0("_", año), ""), ".png"), 
    file.path(clusters_path, tolower(area_type), "boxplots")
  )
  
  # *Gráfico de Distribución de Estudiantes por Cluster (Histograma)
  plot_estudiantes <- ggplot(data, aes(x = Total_Estudiantes, fill = factor(Cluster))) +
    geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
    scale_x_log10(labels = scales::comma) +
    labs(
      title = paste("Distribución de Estudiantes por Cluster -", area_type, 
                    ifelse(!is.null(año), paste("- Año", año), "")),
      x = "Total de Estudiantes (Log10)", 
      y = "Frecuencia", 
      fill = "Cluster"
    ) +
    theme_minimal()
  
  # Guardar gráfico de distribución de estudiantes (histograma)
  save_plot(
    plot_estudiantes, 
    paste0("distribucion_clusters_", tolower(area_type), 
           ifelse(!is.null(año), paste0("_", año), ""), ".png"), 
    file.path(clusters_path, tolower(area_type), "distribucion")
  )
  
  return(data)
}

# *Análisis de Clustering ------------------------------------------------
for (año in years) {
  if (año %in% names(lista_datos_year)) {
    # *Clustering General
    realizar_clustering(
      lista_datos_year[[as.character(año)]], 
      "General",
      año
    )
    
    # *Clustering Urbano
    realizar_clustering(
      lista_datos_urbano[[as.character(año)]], 
      "Urbano",
      año
    )
    
    # *Clustering Rural
    realizar_clustering(
      lista_datos_rural[[as.character(año)]], 
      "Rural",
      año
    )
  }
}

# *Conclusión ------------------------------------------------------------
print("Análisis de Clustering de Instituciones Educativas completado. Resultados guardados en las carpetas respectivas.")