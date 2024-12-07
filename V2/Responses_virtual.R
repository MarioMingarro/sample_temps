closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")

#   SIG	TREND
#TT	***	+
#TA	***	-
#TC	
#  
#SA	***	+
#SD	***	-
#SC	

# Data ----
directorio <- "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V3/"
Data <- readRDS(paste0(directorio, "lista completa SA_SC_SD.RDS")) 


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

# All species -----
tabla_general <- general_trend(Data,y)

# Indivudual species ----
## Mean ----
spp <- unique(Data$species) # Creamos un vector con los nombres de las especies


tic()
tabla_ind <- spp_trend(Data, spp, y, n_min = 50)
tabla_ind[,4] <- round(tabla_ind[,4],4)
toc()



library(foreach)
library(doParallel)

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
        p_Lat >= 0.0002~ "SC",
        Dif_pvalue_Lat <= 0.0002 & Trend_Lat > 0 ~ "SA",
        Dif_pvalue_Lat <= 0.0002 & Trend_Lat < 0 ~ "SD",
        TRUE ~ "SC"))  %>%
  # Une el numero de registros obtenidos del conjunto global de datos
  left_join(
    Data %>%
      group_by(species) %>%
      summarise(Registros = n()),
    by = c("Spp" = "species"))  %>% 
  separate(Spp,c("A", "Spatial_G", "B"), sep = "_", remove = FALSE) %>% 
  subset(select = -c(A,B))

table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)
round(prop.table(table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)),3)


table <- as.table(
  rbind(
    c(96, 4, 0), c(24, 76, 0),
    c(0, 0, 100)
  )
)
dimnames(table) <- list(
  Simulado = c("SA", "SC", "SC"),
  Clasificado = c("SA", "SC", "SC")
)
anxiety
library("vcd")
Kappa(table)


# Crear la tabla de contingencia
tabla <- table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)

# Asignar nombres a las filas y columnas
dimnames(tabla) <- list(
  Evaluador1 = c("SA", "SC", "SD"),
  Evaluador2 = c("SA", "SC", "SD")
)

tabla_long <- as.table(tabla)


kappa_fleiss <- kappam.fleiss(tabla_long)


print(kappa_fleiss)


write_xlsx(Tabla_sig_mean, "C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V3/resultados_aleat_SA_SC_SD_percent_0.01.xlsx" )








ind <- Data %>%
  filter(species == "virtualsp_SC_164")

ggplot() + 
  geom_smooth(data= Data, aes(x = year, y = Lat, col = thermal_O), method = "lm")+
  geom_smooth(data= Data, aes(x = year, y = Lat), col = "black", linetype = "dashed", method = "lm")+
  scale_color_viridis_d(option = "C") +
  labs(x = "Year",y = "Latitude", color = "Response")+
  theme_dark()

  geom_smooth(data= ind, aes(x = year, y = Lat), col ="black", method = "lm")+
  ggtitle(paste0("virtualsp_SC_164"))

ggplot(data = Tabla_sig_mean) + 
  geom_point(aes(x = seq_len(nrow(Tabla_sig_mean)),  
      y = Trend_Lat, color = Spatial_G)) +
  scale_color_viridis_d(option = "C") +         
  theme_dark() +                 
  labs(x = "n",y = "Trend", color = "Response"             
  )

  geom_smooth(data= Data, aes(x = year, y = Lat), col = "gold", method = "lm")+
  geom_smooth(data= ind, aes(x = year, y = Lat), col ="black", linetype = "dashed",method = "lm")

ggplot() + 
  geom_smooth(data= Data, aes(x = year, y = Lat),col = "dodgerblue", fill = "dodgerblue", method = "lm") +
  geom_smooth(data= ind, aes(x = year, y = Lat),col = "dodgerblue4", fill = "dodgerblue4", method = "lm")+
  geom_smooth(data= Data, aes(x = year, y = Lat),col = "gold", fill = "gold", method = "lm") +
  geom_smooth(data= ind, aes(x = year, y = Lat),col = "gold4", fill = "gold4", method = "lm")+
  ggtitle(paste0(spp[i]))+
  labs(x= "Year", y = "Lat")+
  theme_minimal()


# THERMAL ----

Data <- readRDS("C:/A_TRABAJO/A_JORGE/SPP_VIRTUALES/V2/muestreo_aleat_SA_SC_SD_percent_0.01.RDS") # Cargamos datos

ggplot() + 
  geom_smooth(data= Data, aes(x = year, y = y, col =spatialthermaltype), method = "lm")+
  geom_smooth(data= ind, aes(x = year, y = Lat), col ="black", method = "lm")

# Modificar fechas
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","Año_Mes")
Data$TMAX <- Data$TMAX/10
Data$TMIN <- Data$TMIN/10




x <- "Año_Mes" # Variable independiente
y <- c("TMAX", "TMIN") # Variables dependiente
Data[,c(4:7)] <-round(Data[,c(4:7)],4) 

# All species -----
tabla_general <- general_trend(Data,y)

# Indivudual species ----
## Mean ----
spp <- unique(Data$species) # Creamos un vector con los nombres de las especies


tic()
tabla_ind <- spp_trend(Data, spp, y, n_min = 50)
tabla_ind[,4] <- round(tabla_ind[,4],4)
toc()
Tabla_sig_mean <-
  tabla_ind %>%
  # Selecciona las variables
  dplyr::select(c(Spp, Trend,t, p, Variable,Dif_t, Dif_pvalue )) %>%  
  # Cambia la estructura de la tabla
  pivot_wider(names_from = Variable, 
              values_from = c(Trend,t,p,Dif_t,Dif_pvalue)) %>%
  # Añade columna de thermal y le asigna una categoría segun los pvalues de tmax o tmin y de si las tendencias son positivas
  mutate(
    Thermal =
      case_when(
        Dif_pvalue_TMIN <= 0.01 & Trend_TMIN < 0 ~ "TAn",
        Dif_pvalue_TMIN <= 0.01 & Trend_TMIN > 0 ~ "TTn",
        Dif_pvalue_TMAX <= 0.01 & Trend_TMAX < 0 ~ "TAx",
        Dif_pvalue_TMAX <= 0.01 & Trend_TMAX > 0 ~ "TTx",
        p_TMIN <= 0.01  ~ "TC",
        p_TMAX <= 0.01  ~ "TC",
        TRUE ~ "TC")) %>%
  # Une el numero de registros obtenidos del conjunto global de datos
  left_join(
    Data %>%
      group_by(species) %>%
      summarise(Registros = n()),
    by = c("Spp" = "species"))  %>% 
  separate(Spp,c("A", "Thermal_G", "B"), sep = "_", remove = FALSE) %>% 
  subset(select = -c(A,B))

round(prop.table(table(Tabla_sig_mean$Thermal_G, Tabla_sig_mean$Thermal)),3)


spp <- unique(Data$species)
ind <- Data %>%
  filter(species == spp[5])

ggplot() + 
  geom_smooth(data= Data, aes(x = year, y = TMIN),col = "dodgerblue", fill = "dodgerblue", method = "lm") +
  geom_smooth(data= ind, aes(x = year, y = TMIN),col = "dodgerblue4", fill = "dodgerblue4", method = "lm")+
  geom_smooth(data= Data, aes(x = year, y = TMAX),col = "gold", fill = "gold", method = "lm") +
  geom_smooth(data= ind, aes(x = year, y = TMAX),col = "gold4", fill = "gold4", method = "lm")+
  ggtitle(paste0(spp[i]))+
  labs(x= "Year", y = "TMIN & TMAX")+
  theme_minimal()


world_map <- ne_countries(scale = "medium", returnclass = "sf")
ggplot()+
  geom_sf(data = world_map)+
  geom_point(data = ind, aes(Long, Lat, col = Año_Mes), alpha =.2)+
  labs(title = spp[i], subtitle = paste0("0.1%   count = ", nrow(ind)))+
  scale_colour_viridis_c(option = "D",trans = "sqrt", alpha = .8)+
  coord_sf(xlim = c(-20,50), ylim = c(35,75), expand = FALSE)+
  theme(axis.text = element_text(angle = 45, size =8),
        axis.title = element_blank(),
        legend.title = element_blank())
