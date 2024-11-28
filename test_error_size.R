closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("V2/Fun_v2.R")

#   SIG	TREND
#TT	***	+
#TA	***	-
#TC	
#  
#SA	***	+
#SD	***	-
#SC	

# Data ----
directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V4/"
resultados_dir <- paste0(directorio, "resultados/")
if (!dir.exists(resultados_dir)) {
  dir.create(resultados_dir)
}
# Load the required data
Data <- readRDS(paste0(directorio,"lista completa SA_SC_SD.RDS"))
#lista completa SA_SC_SD.RDS

# Preprocess Data
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX / 10
Data$TMIN <- Data$TMIN / 10
Data[, c(4:7)] <- round(Data[, c(4:7)], 4)

x <- "Año_Mes" # Variable independiente
y <- c("Lat") # Variables dependiente


# Define unique species and sampling sequence
spp <- unique(Data$species)
secuencia <- seq(0.00005, 0.001, 0.0001)

# Crear una tabla vacía para los resultados finales
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)

# Crear una tabla vacía para almacenar los resultados finales
final_table <- data.frame(
  Records = numeric(),
  SA_error = numeric(),
  SC_error = numeric(),
  SD_error = numeric()
)

# Iterar sobre la secuencia de muestreos
for (i in seq_along(secuencia)) {
  # Realizar un muestreo de los datos
  sampled_data <- Data %>% sample_frac(secuencia[i])
  
  # Verificar si hay datos después del muestreo
  if (nrow(sampled_data) == 0) {
    warning(paste("Muestreo vacío para secuencia:", secuencia[i]))
    next
  }
  
  # Configurar clúster de paralelización
  numCores <- detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Ejecutar el cálculo de tendencias en paralelo
  tabla_ind <- foreach(
    sp = spp,
    .combine = rbind,
    .packages = c("tidyverse", "jtools")
  ) %dopar% {
    resultado <- spp_trend(sampled_data, sp, y, n_min = 50)
    if (!is.null(resultado) && nrow(resultado) > 0) {
      resultado[, 4] <- round(resultado[, 4], 4)  # Redondear columna 4
    }
    return(resultado)
  }
  
  # Detener el clúster
  stopCluster(cl)
  
  # Bonferroni correction
  bonferroni <- 0.005 / length(spp)
  
  # Procesar la tabla resultante
  if (!is.null(tabla_ind) && nrow(tabla_ind) > 0) {
    Tabla_sig_mean <- tabla_ind %>%
      dplyr::select(c(Spp, Trend, t, p, Variable, Dif_t, Dif_pvalue)) %>%
      pivot_wider(
        names_from = Variable, 
        values_from = c(Trend, t, p, Dif_t, Dif_pvalue),
        names_sep = "_"
      ) %>%
      mutate(
        Spatial = case_when(
          p_Lat >= bonferroni ~ "SC",
          p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat > 0 ~ "SA",
          p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat < 0 ~ "SD",
          TRUE ~ "SC"
        )
      ) %>%
      # Separar y validar la columna Spp
      separate(Spp, c("A", "Spatial_G", "B"), sep = "_", remove = FALSE, fill = "right") %>%
      mutate(Spatial_G = ifelse(is.na(Spatial_G), "Unknown", Spatial_G))
    
    # Crear tabla de frecuencias, manejando posibles problemas de dimensiones
    a <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)
    
    # Calcular los errores, manejando casos donde la tabla esté incompleta
    SA_error <- ifelse("1" %in% rownames(a) & "2" %in% colnames(a), a[1, 2], 0) +
      ifelse("1" %in% rownames(a) & "3" %in% colnames(a), a[1, 3], 0)
    SC_error <- ifelse("2" %in% rownames(a) & "1" %in% colnames(a), a[2, 1], 0) +
      ifelse("2" %in% rownames(a) & "3" %in% colnames(a), a[2, 3], 0)
    SD_error <- ifelse("3" %in% rownames(a) & "1" %in% colnames(a), a[3, 1], 0) +
      ifelse("3" %in% rownames(a) & "2" %in% colnames(a), a[3, 2], 0)
    
    # Almacenar los resultados en la tabla final
    final_table <- rbind(final_table, data.frame(
      Records = secuencia[i],
      SA_error = SA_error,
      SC_error = SC_error,
      SD_error = SD_error
    ))
  } else {
    warning(paste("No se generaron resultados para secuencia:", secuencia[i]))
  }
}

# Mostrar la tabla final
print(final_table)




kkdvk <- sampled_data %>%
  group_by(species) %>%
  summarise(num_registros = n())
