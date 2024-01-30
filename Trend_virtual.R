library(readxl)
library(tidyverse)
library(writexl)
library(foreach)
library(doParallel)
library(tictoc)
library(jtools)

Data_1 <- readRDS("B:/A_JORGE/A_VIRTUALES/selected_ocs_percent_0.02.rds") # Cargamos datos
Data_1$Año_Mes <- Data_1$month * 0.075
Data_1$Año_Mes <- Data_1$year + Data_1$Año_Mes

colnames(Data_1) <- c("species","year","month","Lat","Long","TMAX","TMIN","thermal_O","spatial_O","Año_Mes")

x <- "Año_Mes" # Variable independiente
y <- c("Lat", "TMAX", "TMIN") # Variables dependiente

Data <- Data_1
# Generamos la tabla con las tendencias y otras medidas de las tendencias para el set global de datos

# All species -----

# Creamos tabla vacía para almacenar los resultados
tabla_general <- data.frame(
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "P95_max" = numeric(),
  "P95_min" = numeric()
)


for (i in 1:length(y)) {        # Bucle para calcular las estadísticas de todas las variables independientes
  tabla <- data.frame(          # Tabla vacía donde se guardan resultados de cada variable independiente 
    "Variable" = NA,            # para después unir a la tabla general
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA
  )
  tabla$Variable <- y[i]  # Rellena primera columna con el nombre de la variable
  model_g <- lm(formula(paste(y[i],  # Crea formula utilizando la variable del bucle
                              paste(x, collapse = "+"),
                              sep = " ~ ")), 
                data = Data)
  tabla$Trend <- model_g$coefficients[[2]] # Tendencia
  tabla$t <- summary(model_g)$coefficients[2, 3] # t del modelo
  tabla$p <- summary(model_g)$coefficients[2, 4] # p del modelo
  tabla$P95_max <-  confint(model_g, "Año_Mes", level = .95)[, 2] # Intervalo de confianza max del 95%
  tabla$P95_min <-  confint(model_g, "Año_Mes", level = .95)[, 1] # Intervalo de confianza min del 95%
  tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una de las tablas individuales
}
plot(Data$Año_Mes, Data$TMIN)
ggplot(data = Data,aes(Año_Mes, TMIN))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)

writexl::write_xlsx(tabla_general, "B:/A_JORGE/A_VIRTUALES/RESULT/Trend_general_100002.xlsx")

rm(tabla, model_g, i)


# Generamos la tabla con las tendencias y otras medidas de las tendencias para cada una de las especies
# Además comparamos la tendencia de cada una de las especies con la tendencia del conjunto de datos

# Indivudual species ----
acc <- data.frame(n = numeric(0), 
                  Thermal_acc = numeric(0),
                  Spatial_acc = numeric(0))

for (l in seq(10000, 1000000, by = 10000)) {
  Data <- Data_1[1:l,]
spp <- unique(Data$species) # Creamos un vector con los nombres de las especies

#spp <- spp[1:10]

# Creamos funcion
#species_trend <-
  #function(spp, Data, y) 
    tabla_ind <- data.frame(                         
      "Spp" = NA,                                 
      "Variable" = NA,
      "Trend" = NA,
      "t" = NA,
      "p" = NA,
      "P95_max" = NA,
      "P95_min" = NA,
      "Dif_pvalue" = NA,
      "Dif_df" = NA,
      "Dif_F" = NA
    )
tabla_ind <- tabla_ind[-1,]

# Bucle para calcular las tendencias de cada una de las especies
tic()
for (n in 1:length(spp)){
    # Bucle para actuar sobre cada una de las especies
  
  # Filtra la especie  
    ind <- Data %>%
      filter(species == spp[n]) %>%
      mutate(group = "i")
    
    gen <- Data %>%
      mutate(group = "g")
    
    dat <- rbind(gen, ind)
    
    if (nrow(ind) > 50) {
      # Condicional "SI" para seleccionar aquellas especies con mas de 10 registros
      for (i in 1:length(y)) {
        # Bucle para cada una de las variables independientes
        tryCatch({
          # Implementa código que debe ejecutarse cuando se produce la condición de error
          tabla <-
            data.frame(
              # Crea tabla vacia para despues unificar a tabla de resultados
              "Spp" = NA,
              "Variable" = NA,
              "Trend" = NA,
              "t" = NA,
              "p" = NA,
              "P95_max" = NA,
              "P95_min" = NA,
              "Dif_pvalue" = NA,
              "Dif_df" = NA,
              "Dif_F" = NA
            )
          
          # Crea de nuevo el modelo general utilizando todos los datos
          model_g <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = Data)
          tabla$Spp <- unique(ind[[1]])
          tabla$Variable <- y[i]
          
          # Crea el modelo de cada especie para la posterior comparación, De aqui servirán los datos de tendencias
          model_i <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = ind)
          
          # Añade resultados en la tabla
          tabla$Trend <- model_i$coefficients[[2]]
          tabla$t <- summary(model_i)$coefficients[2, 3]
          tabla$p <- summary(model_i)$coefficients[2, 4]
          tabla$P95_max <- confint(model_i, "Año_Mes", level = .95)[, 2]
          tabla$P95_min <- confint(model_i, "Año_Mes", level = .95)[, 1]
          
          # Crea de nuevo el modelo de comparación y ver si existe diferencias acxorde al grupo
          model_int <- lm(formula(paste(y[i], paste(
            x, "*group", collapse = "+"
          ), sep = " ~ ")), data = dat)
          
          tabla$Dif_pvalue <- summary(model_int)$coefficients[4, 4]
          
          f <- anova(model_int)
          
          tabla$Dif_df <- paste0("1 - ", f$Df[4])
          tabla$Dif_F <- f$`F value`[3]
          tabla_ind <- rbind(tabla_ind, tabla) 
          
        }, error = function(e) {
          # Si la función tryChach da error ejecuta esta parte del codigo
          cat(
            paste0("WARNING: Specie ", ind[1, 1], " variable (", y[i], ") has"),
            # Indica que especie tiene el problema y por qué
            conditionMessage(e),
            "\n"
          )
        })
      }
    } else{
      print(paste0("Data for ", ind[1, 1], " specie are insufficient")) # Si en el condicional no hay suficientes registros
      # para una especie (<50) expresa el mensaje
    }
  }
toc()
#9873.55 sec elapsed

#write.csv2(tabla_ind, "B:/A_JORGE/A_VIRTUALES/RESULT/Trend_ssp_02.csv")
tabla_ind <- read.csv2("B:/A_JORGE/A_VIRTUALES/RESULT/Trend_ssp_02.csv")

## Significance

Tabla_sig <-
  tabla_ind %>%
  # Selecciona las variables
  select(c(Spp, Trend, Variable, Dif_pvalue)) %>%  
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
        Dif_pvalue_TMIN <= 0.01 & Trend_Lat < 0 ~ "TA",
        Dif_pvalue_TMIN <= 0.01 & Trend_Lat > 0 ~ "TT",
        Dif_pvalue_TMAX <= 0.01 & Trend_Lat < 0 ~ "TA",
        Dif_pvalue_TMAX <= 0.01 & Trend_Lat > 0 ~ "TT",
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

Tabla_sig[Tabla_sig == "TA"] <- "TT"
Tabla_sig[Tabla_sig == "SD"] <- "SA"

a <- prop.table(table(Tabla_sig$Thermal_G, Tabla_sig$Thermal))
b <- prop.table(table(Tabla_sig$Spatial_G, Tabla_sig$Spatial))

Thermal_acc <- 0
p <- length(a)/(0.5*length(a))
for (k in 1:p) {
  Thermal_acc <- Thermal_acc + a[k, k]
}
spatial_acc <- 0
for (k in 1:2) {
  spatial_acc <- spatial_acc + b[k, k]
}
d <- cbind(Thermal_acc, spatial_acc)
acc <- rbind(acc, d)
}





spatial_acc=1
writexl::write_xlsx(Tabla_sig, "B:/A_JORGE/A_VIRTUALES/RESULT/Significancia_ssp_02.xlsx")
# Crea una tabla resumen y calcula el porcentaje de las estrategias
Tabla_res <- Tabla_sig %>% 
  group_by(Spatial,Thermal) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(Frecuency = (n *100) / sum(n))


spp_used <- plyr::count(Data, "Especie")

spp_sin_analisis <- as.data.frame(setdiff(spp,unique(tabla_ind[,1])))
names(spp_sin_analisis) <-  "Especie"
spp_sin_analisis <- left_join(spp_sin_analisis, spp_used)


sheets <- list(
  "General_Results" = tabla_general,
  "Spp_Results" = tabla_ind,
  "Significance_Results" = Tabla_sig,
  "Estrategies_Results" = Tabla_res,
  "Species_used" = spp_used,
  "Species_with_insuficient_data" = spp_sin_analisis
)

write_xlsx(sheets, "Resultados_R.xlsx")

##################################################################################
##################################################################################
#################################################################################

Tabla_sig <- 
  tabla_ind %>%
  na.omit() %>% 
  select(c(Spp, Variable, Dif_1_pvalue)) %>% 
  pivot_wider(
    names_from = Variable,
    values_from = Dif_1_pvalue) %>%
  mutate(Geographical =
           case_when(Lat <= 0.01 | Long <= 0.01 ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(Lat =
           case_when(Lat <= 0.01 ~ 1,
                     TRUE ~ 0)) %>%
  mutate(Long =
           case_when(Long <= 0.01 ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(Elevation =
           case_when(Elevation <= 0.01 ~ 1,
                     TRUE ~ 0)) %>%
  mutate(TMAX =
           case_when(TMAX <= 0.01 ~ 1,
                     TRUE ~ 0)) %>%
  mutate(TMIN =
           case_when(TMIN <= 0.01 ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(Spatial =
           case_when(Geographical == 1 | Elevation == 1  ~ "Adaptation",
                     TRUE ~ "Permanence")) %>% 
  mutate(Thermal =
           case_when(TMAX == 1 | TMIN == 1  ~ "Tolerance",
                     TRUE ~ "Adjust")) 


Tabla_sig <- 
  tabla_ind %>%
  na.omit() %>% 
  select(c(Spp, Variable, Dif_1_pvalue)) %>% 
  pivot_wider(
    names_from = Variable,
    values_from = Dif_1_pvalue) %>%
  mutate(Spatial =
           case_when(Lat <= 0.01 | Long <= 0.01 | Elevation <= 0.01 ~ "Adaptation",
                     TRUE ~ "Permanence")) %>% 
  mutate(Thermal =
           case_when(TMIN <= 0.01 | TMAX <= 0.01 ~ "Tolerance",
                     TRUE ~ "Adjust")) 
