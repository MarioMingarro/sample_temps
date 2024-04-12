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
Data <- readRDS("B:/A_JORGE/A_VIRTUALES/DATA/ALEATORIAS/selected_ocs_percent_0.01.rds") # Cargamos datos


arc <- list.files("B:/A_JORGE/A_VIRTUALES/DATA/GRADIENTS/01/", full.names = T)
Data <- data.frame()
for(i in 1:9){
  kk <- readRDS(arc[i])
  Data <- rbind(Data, kk)
}
rm(kk)
Data$Año_Mes <- Data$month * 0.075
Data$Año_Mes <- Data$year + Data$Año_Mes
Data$tmaxValues <- Data$tmaxValues/10
Data$tminValues <- Data$tminValues/10

colnames(Data) <- c("species","year","month","Long","Lat","TMAX","TMIN","thermal_O","spatial_O","Año_Mes")

x <- "Año_Mes" # Variable independiente
y <- c("Lat", "TMAX", "TMIN") # Variables dependiente
Data[,c(4:7)] <-round(Data[,c(4:7)],4) 

# Map ----
world_map <- ne_countries(scale = "medium", returnclass = "sf")
directorio <- "B:/A_JORGE/A_VIRTUALES/MAPS/SAMPLED_005/"
spp <- unique(Data$species)
tic()
spp_plotting(spp, Data, world_map, directorio)
toc()

# All species -----
tabla_general <- general_trend(Data,y)

# Indivudual species ----
## Mean ----
spp <- unique(Data$species) # Creamos un vector con los nombres de las especies

tic()
tabla_ind <- spp_trend(Data, spp, y, n_min = 50)
toc()
tabla_ind[,4] <- round(tabla_ind[,4],4)

### Significance ----
Tabla_sig_mean <-
  tabla_ind %>%
  # Selecciona las variables
  dplyr::select(c(Spp, Trend,t, p, Variable,Dif_t, Dif_pvalue )) %>%  
  # Cambia la estructura de la tabla
  pivot_wider(names_from = Variable, 
              values_from = c(Trend,t,p,Dif_t,Dif_pvalue)) %>%
  # Añade columna de spatial y le asigna una categoría según los pvalues de latitud, longitud o elevacion
  mutate(
    Spatial =
      case_when(
        Dif_pvalue_Lat <= 0.05 & Trend_Lat > 0 ~ "SA",
        Dif_pvalue_Lat <= 0.05 & Trend_Lat < 0 ~ "SD",
        TRUE ~ "SC"))  %>%
  # Añade columna de thermal y le asigna una categoría segun los pvalues de tmax o tmin y de si las tendencias son positivas
  mutate(
    Thermal =
      case_when(
        Dif_pvalue_TMIN <= 0.05 & Trend_TMIN < 0 ~ "TA",
        Dif_pvalue_TMIN <= 0.05 & Trend_TMIN > 0 ~ "TT",
        Dif_pvalue_TMAX <= 0.05 & Trend_TMAX < 0 ~ "TA",
        Dif_pvalue_TMAX <= 0.05 & Trend_TMAX > 0 ~ "TT",
        TRUE ~ "TC")) %>%
  # Une el numero de registros obtenidos del conjunto global de datos
  left_join(
    Data %>%
      group_by(species) %>%
      summarise(Registros = n()),
    by = c("Spp" = "species"))  %>% 
  separate(Spp,c("A", "Spatial_G", "Thermal_G", "B"), sep = "_", remove = FALSE) %>% 
  subset(select = -c(A,B))

write_xlsx(Tabla_sig_mean, "B:/A_JORGE/A_VIRTUALES/RESULT/GRADIENTS/01/all_01.xlsx")
write_xlsx(tabla_general,  "B:/A_JORGE/A_VIRTUALES/RESULT/GRADIENTS/01/general_01.xlsx")

round(prop.table(table(Tabla_sig_mean$Thermal_G, Tabla_sig_mean$Thermal)),3)
round(prop.table(table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)),3)


## Percentil ----
#x=5 lat; x=6 Tmax; x=7 Tmin
spp <- unique(Data$species)

percentil = 0.10
tic()
tabla_ind <-data.frame()
y <- "Lat"
tabla_Lat <- spp_trend_percentil(Data, spp, y, n_min = 50,  percentil, variable=5)
y <- "TMAX"
tabla_TMAX <- spp_trend_percentil(Data, spp, y, n_min = 50, percentil, variable=6)
y <- "TMIN"
tabla_TMIN <- spp_trend_percentil(Data, spp, y, n_min = 50, percentil, variable=7)
toc()
tabla_ind <- rbind(tabla_ind,tabla_Lat,tabla_TMAX,tabla_TMIN)
tabla_ind[,4] <- round(tabla_ind[,4],4)

### Significance ----
Tabla_sig_10 <-
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
    by = c("Spp" = "species"))  %>% 
  separate(Spp,c("A", "Spatial_G", "Thermal_G", "B"), sep = "_", remove = FALSE) %>% 
  subset(select = -c(A,B))

Tabla_sig_10 <- mutate(Tabla_sig_10, Percentil = "P10")
Tabla_sig_50 <- mutate(Tabla_sig_50, Percentil = "P50")
Tabla_sig_90 <- mutate(Tabla_sig_90, Percentil = "P90")

Tabla_sig_mean <- mutate(Tabla_sig_mean, Percentil = "ALL")
Tabla_sig <- rbind(Tabla_sig_mean,Tabla_sig_10,Tabla_sig_50,Tabla_sig_90)

writexl::write_xlsx(Tabla_sig, "B:/A_JORGE/A_VIRTUALES/resultados_percentiles_sampled_spp_05.xlsx")
Tabla_sig %>% 
  group_by(Spp)


round(prop.table(table(Tabla_sig_mean$Thermal_G, Tabla_sig_mean$Thermal)),3)
round(prop.table(table(Tabla_sig_mean$Spatial_G, Tabla_sig_mean$Spatial)),3)
round(prop.table(table(Tabla_sig_10$Thermal_G, Tabla_sig_10$Thermal)),3)
round(prop.table(table(Tabla_sig_10$Spatial_G, Tabla_sig_10$Spatial)),3)
round(prop.table(table(Tabla_sig_50$Thermal_G, Tabla_sig_50$Thermal)),3)
round(prop.table(table(Tabla_sig_50$Spatial_G, Tabla_sig_50$Spatial)),3)
round(prop.table(table(Tabla_sig_90$Thermal_G, Tabla_sig_90$Thermal)),3)
round(prop.table(table(Tabla_sig_90$Spatial_G, Tabla_sig_90$Spatial)),3)

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


