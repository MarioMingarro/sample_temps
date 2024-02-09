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
Data <- readRDS("B:/A_JORGE/A_VIRTUALES/SD_TT_ocs_percent_0.02.rds") # Cargamos datos

Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes

colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","spatial_O","Año_Mes")

x <- "Año_Mes" # Variable independiente
y <- c("Lat", "TMAX", "TMIN") # Variables dependiente
Data[,c(4:7)] <-round(Data[,c(4:7)],2) 

# Percentil

tabla_ind90 <- spp_trend_percentil(Data, spp, y, umbral = 50, percentil = 0.90)

p10 <- quantile(Data$Lat, 0.10)
p45 <- quantile(Data$Lat, 0.45)
p55 <- quantile(Data$Lat, 0.55)
p90 <- quantile(Data$Lat, 0.90)

Data_10 <- Data %>% filter(Lat <= p10)
Data_45_55 <- Data %>% 
  filter(Lat <= p55) %>% 
  filter(Lat >= p45)
Data_90 <- Data %>% filter(Lat <= p90)

# All species -----
tabla_general <- general_trend(Data_10,y)

# Indivudual species ----
spp <- unique(Data$species) # Creamos un vector con los nombres de las especies
spp <- spp[1:10]

tic()
tabla_ind <- trend_calc(Data, spp,y, umbral = 50)
toc()

tabla_ind_10 <- trend_calc(Data_10,spp, y,umbral = 50)
tabla_ind_90 <- trend_calc(Data_90,spp, y,umbral = 50)

tabla_ind[,4] <- round(tabla_ind[,4],4)

## Significance ----
Tabla_sig <-
  tabla_ind %>%
  # Selecciona las variables
  dplyr::select(c(Spp, Trend, Variable, Dif_pvalue)) %>%  
    # Cambia la estructura de la tabla
  pivot_wider(names_from = Variable, 
              values_from = c(Trend,Dif_pvalue)) %>%
  # Añade columna de spatial y le asigna una categoría según los pvalues de latitud, longitud o elevacion
  mutate(
    Spatial =
      case_when(
        Dif_pvalue_Lat <= 0.01 & Trend_Lat > 0 ~ "SA",
        Dif_pvalue_Lat <= 0.01 & Trend_Lat < 0 ~ "SD",
        TRUE ~ "SC"))  %>%
  # Añade columna de thermal y le asigna una categoría segun los pvalues de tmax o tmin y de si las tendencias son positivas
  mutate(
    Thermal =
      case_when(
        Dif_pvalue_TMIN <= 0.01 & Trend_TMIN < 0 ~ "TA",
        Dif_pvalue_TMIN <= 0.01 & Trend_TMIN > 0 ~ "TT",
        Dif_pvalue_TMAX <= 0.01 & Trend_TMAX < 0 ~ "TA",
        Dif_pvalue_TMAX <= 0.01 & Trend_TMAX > 0 ~ "TT",
        TRUE ~ "TC")) %>%
  # Une el numero de registros obtenidos del conjunto global de datos
  left_join(
    Data %>%
      group_by(species) %>%
      summarise(Registros = n()),
    by = c("Spp" = "species")) 


#Resultados

Tabla_sig <- Tabla_sig %>% 
  separate(Spp,c("A", "Spatial_G", "Thermal_G", "B"), sep = "_", remove = FALSE)
Tabla_sig <- Tabla_sig %>% 
  subset(select = -c(A,B))


a <- round(prop.table(table(Tabla_sig$Thermal_G, Tabla_sig$Thermal)),3)
b <- round(prop.table(table(Tabla_sig$Spatial_G, Tabla_sig$Spatial)),3)

---------
  
  
Thermal_acc <- 0
p <- length(a)/(0.5*length(a))
for (k in 1:p) {
  Thermal_acc <- Thermal_acc + a[k, k]
}
spatial_acc <- 0
for (k in 1:2) {
  spatial_acc <- spatial_acc + b[1, 1]
}
d <- cbind(Thermal_acc, spatial_acc)
acc <- rbind(acc, d)


tabla_proporciones <- Tabla_sig %>%
  mutate(Tendencia = ifelse(Trend_Lat > 0, "Positiva", "Negativa")) %>%
  select(Tendencia, Spatial_G) %>%
  group_by(Tendencia, Spatial_G) %>%
  summarise(N = n())


spatial_acc=1
writexl::write_xlsx(Tabla_sig, "B:/A_JORGE/A_VIRTUALES/RESULT/Significancia_ssp_02.xlsx")
# Crea una tabla resumen y calcula el porcentaje de las estrategias
Tabla_res <- Tabla_sig %>% 
  group_by(Spatial,Thermal) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(Frecuency = (n *100) / sum(n))


