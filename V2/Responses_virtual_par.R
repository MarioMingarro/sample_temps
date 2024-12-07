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

Data <- readRDS(paste0(directorio,"lista completa SA_SC_SD.RDS")) 
# "muestreo_aleat_SA_SC_SD_percent_5e-04.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.001.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.0025.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.005.RDS"    
# "muestreo_aleat_SA_SC_SD_percent_0.01.RDS"    
# "muestreo_aleat_SA_SC_SD_percent_0.02.RDS"     


# Modificar fechas
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX/10
Data$TMIN <- Data$TMIN/10


# SPATIAL ----
x <- "Año_Mes" # Variable independiente
y <- c("Lat") # Variables dependiente
Data[,c(4:7)] <-round(Data[,c(4:7)],4) 

spp <- unique(Data$species)
secuencia <-  seq(0.00005, 0.001, 0.0001)
for (i in secuencia ){
  

# Configurar el clúster de paralelización
numCores <- detectCores() - 10  # Usar todos menos 1 núcleo
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Comenzar la paralelización
library(tictoc)  # Para medir el tiempo
tic()

# Dividir las tareas entre los núcleos
tabla_ind <- foreach(
  i = 1:length(spp), 
  .combine = rbind, 
  .packages = c("tidyverse", "tictoc", "jtools", "viridisLite")
) %dopar% {
  # Ejecutar spp_trend para cada elemento de spp
  resultado <- spp_trend(Data, spp[i], y, n_min = 50)
  resultado[, 4] <- round(resultado[, 4], 4)  # Redondear la columna 4
  return(resultado)
}

toc()

# Detener el clúster
stopCluster(cl)

bonferroni <- 0.005/length(spp)

Tabla_sig_mean <-
  tabla_ind %>%
  # Selecciona las variables
  dplyr::select(c(Spp, Trend,t, p, Variable,Dif_t, Dif_pvalue )) %>%  
  # Cambia la estructura de la tabla
  pivot_wider(names_from = Variable, 
              values_from = c(Trend,t,p,Dif_t,Dif_pvalue)) %>%
  # filter(
  #   p_Lat <= 0.01) %>% 
  # Añade columna de spatial y le asigna una categoría según los pvalues de latitud, longitud o elevacion
  mutate(
    Spatial =
      case_when(
        p_Lat >= bonferroni ~ "SC",
        p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat > 0 ~ "SA",
        p_Lat < bonferroni & Dif_pvalue_Lat <= bonferroni & Trend_Lat < 0 ~ "SD",
        TRUE ~ "SC"))  %>%
  # Une el numero de registros obtenidos del conjunto global de datos
  left_join(
    Data %>%
      group_by(species) %>%
      summarise(Registros = n()),
    by = c("Spp" = "species"))  %>% 
  separate(Spp,c("A", "Spatial_G", "B"), sep = "_", remove = FALSE) %>% 
  subset(select = -c(A,B))


a <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)

SA_E <- a[1,2] + a[1,3]
SC_E <- a[2,1] + a[2,3]
SD_E <- a[3,1] + a[3,2]

res_E <- cbind(SA_E, SC_E, SD_E)
res_E <- res_E %>% mutate(muestra = i)
}


write_xlsx(Tabla_sig_mean, paste0(resultados_dir,"resultados_aleat_SA_SC_SD_", "0.02", ".xlsx"))


Data <- readRDS(paste0(directorio,"muestreo_aleat_SA_SC_SD_percent_0.02.RDS")) 

model_g <-
  lm(formula(paste("Lat",  # Crea formula utilizando la variable del bucle
                   "Año_Mes",
                   sep = " ~ ")),
     data = Data)


# "muestreo_aleat_SA_SC_SD_percent_5e-04.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.001.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.0025.RDS"   
# "muestreo_aleat_SA_SC_SD_percent_0.005.RDS"    
# "muestreo_aleat_SA_SC_SD_percent_0.01.RDS"    
# "muestreo_aleat_SA_SC_SD_percent_0.02.RDS"   
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX/10
Data$TMIN <- Data$TMIN/10


# SPATIAL ----
x <- "Año_Mes" # Variable independiente
y <- c("Lat") # Variables dependiente
Data[,c(4:7)] <-round(Data[,c(4:7)],4)


a <- general_trend(Data,y)
b <- general_trend(Data,y)
c <- general_trend(Data,y)
d <- general_trend(Data,y)
e <- general_trend(Data,y)
f <- general_trend(Data,y)


res <- rbind(a,b,c,d,e,f)
res <- mutate(res, "muestra" = c("5e-04","0.001", "0.0025", "0.005", "0.01", "0.02"))
write_xlsx(res, paste0(resultados_dir,"resultados_general_SA_SC_SD_muestreo.xlsx"))





























# Iterar sobre los diferentes tamaños de muestreo
# Configurar el clúster de paralelización
numCores <- detectCores() - 1  # Usar todos menos 1 núcleo
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Especificar los tamaños de muestreo
sampling_sizes <- c(0.005, 0.01, 0.05, 0.1, 0.25)

# Almacenar resultados de Kappa para cada tamaño de muestra
kappa_results <- data.frame(SampleSize = numeric(0), Kappa_U = numeric(0))

# Comenzar el análisis
tic()

# Iterar sobre los diferentes tamaños de muestreo (ahora usando for)
for (sampling_size in sampling_sizes) {
  
  tryCatch({
    # Realizar un muestreo aleatorio del conjunto de datos según el tamaño de muestra
    set.seed(123)  # Para reproducibilidad
    sampled_data <- Data[sample(1:nrow(Data), size = floor(nrow(Data) * sampling_size), replace = FALSE), ]
    
    # Usar foreach para iterar sobre spp
    tabla_ind_list <- foreach(i = 1:length(spp), .packages = c("tidyverse",  "tictoc", "vcd", "writexl")) %dopar% {
      spp_trend(sampled_data, spp[i], y, n_min = 50)
    }
    
    # Unir los resultados de cada iteración de spp_trend
    tabla_ind <- do.call(rbind, tabla_ind_list)
    tabla_ind[, 4] <- round(tabla_ind[, 4], 4)
    
    # Preparar la tabla para calcular Kappa
    Tabla_sig_mean <- tabla_ind %>%
      dplyr::select(c(Spp, Trend, t, p, Variable, Dif_t, Dif_pvalue)) %>%
      pivot_wider(names_from = Variable, values_from = c(Trend, t, p, Dif_t, Dif_pvalue)) %>%
      mutate(
        Spatial = case_when(
          p_Lat >= 0.0002 ~ "SC",
          Dif_pvalue_Lat <= 0.0002 & Trend_Lat > 0 ~ "SA",
          Dif_pvalue_Lat <= 0.0002 & Trend_Lat < 0 ~ "SD",
          TRUE ~ "SC"
        )
      ) %>%
      left_join(
        sampled_data %>%
          group_by(species) %>%
          summarise(Registros = n()),
        by = c("Spp" = "species")
      ) %>%
      separate(Spp, c("A", "Spatial_G", "B"), sep = "_", remove = FALSE) %>%
      subset(select = -c(A, B))
    
    write_xlsx(Tabla_sig_mean, paste0(resultados_dir,"resultados_aleat_SA_SC_SD_", sampling_size, ".xlsx"))
    
    tabla <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)
    
    dimnames(tabla) <- list(
      Simulado = c("SA", "SC", "SD"),  # Tres categorías
      Clasificado = c("SA", "SC", "SD")  # Tres categorías
    )
    
    kappa <- Kappa(tabla)  # Fleiss' Kappa
    
    # Asegurarse de que accedemos correctamente a los valores de Kappa
    kappa_unweighted <- kappa$Unweighted[1]  # Acceder al valor Unweighted
    
    # Almacenar el resultado
    kappa_results <- rbind(kappa_results, data.frame(SampleSize = sampling_size, Kappa_U = kappa_unweighted))
    
  }, error = function(e) {
    # Si ocurre un error, lo manejamos y lo reportamos
    message("Error procesando tamaño de muestra ", sampling_size, ": ", e)
    kappa_results <- rbind(kappa_results, data.frame(SampleSize = sampling_size, Kappa_U = NA))
  })
}

# Finalizar el análisis
toc()

# Detener el clúster
stopCluster(cl)


# Graficar los resultados
ggplot(kappa_results, aes(x = SampleSize*100, y = Kappa_U)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Índice Kappa según el tamaño de la muestra",
    x = "Tamaño de la muestra (proporción)",
    y = "Índice Kappa",
    caption = "Cálculo realizado con el índice de Fleiss Kappa"
  ) +
  theme_minimal()
