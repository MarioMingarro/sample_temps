packages.to.use <- c("readxl",
                     "tidyverse",
                     "writexl",
                     "tictoc",
                     "jtools",
                     "viridisLite",
                     "rnaturalearth",
                     "rnaturalearthdata")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) & package == "VoCC" ) { devtools::install_github("JorGarMol/VoCC", dependencies = TRUE) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}
rm(packages.to.use,package)
# Funciones -----

# general_trend ----
general_trend <- function(Data, y) {
  tabla_general <- data.frame(
    "Variable" = character(),
    "Trend" = numeric(),
    "t" = numeric(),
    "p" = numeric(),
    "P95_max" = numeric(),
    "P95_min" = numeric()
  )
  for (i in 1:length(y)) {
    # Bucle para calcular las estadísticas de todas las variables independientes
    tabla <-data.frame(
      "Variable" = NA,
      "Trend" = NA,
      "t" = NA,
      "p" = NA,
      "P95_max" = NA,
      "P95_min" = NA)
    tabla$Variable <- y[i]  # Rellena primera columna con el nombre de la variable
    model_g <-
      lm(formula(paste(y[i],  # Crea formula utilizando la variable del bucle
                       paste(x, collapse = "+"),
                       sep = " ~ ")),
         data = Data)
    tabla$Trend <- model_g$coefficients[[2]] # Tendencia
    tabla$t <- summary(model_g)$coefficients[2, 3] # t del modelo
    tabla$p <- summary(model_g)$coefficients[2, 4] # p del modelo
    tabla$P95_max <- confint(model_g, "Año_Mes", level = .95)[, 2] # Intervalo de confianza max del 95%
    tabla$P95_min <- confint(model_g, "Año_Mes", level = .95)[, 1] # Intervalo de confianza min del 95%
    tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una de las tablas individuales
  }
  return(tabla_general)
}

# spp_trend ----
spp_trend <- function(Data, spp, y, n_min = 50) {
  tabla_ind <- data.frame(
    "Spp" = NA,
    "Variable" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA,
    "Dif_t" = NA,
    "Dif_pvalue" = NA)
  
  tabla_ind <- tabla_ind[-1,]
  
  # Bucle para calcular las tendencias de cada una de las especies
  for (n in 1:length(spp)){
    # Bucle para actuar sobre cada una de las especies
    # Filtra la especie  
    ind <- Data %>%
      filter(species == spp[n]) %>%
      mutate(group = "i")
    
    gen <- Data %>%
      mutate(group = "g")
    
    dat <- rbind(gen, ind)
    
    if (nrow(ind) > n_min) {
      # Condicional "SI" para seleccionar aquellas especies con mas de 10 registros
      for (i in 1:length(y)) {
        # Bucle para cada una de las variables independientes
        tryCatch({
          # Implementa código que debe ejecutarse cuando se produce la condición de error
          tabla <-
            data.frame(
              # Crea tabla vacía para después unificar a tabla de resultados
              "Spp" = NA,
              "Variable" = NA,
              "Trend" = NA,
              "t" = NA,
              "p" = NA,
              "P95_max" = NA,
              "P95_min" = NA,
              "Dif_t" = NA,
              "Dif_pvalue" = NA
            )
          
          # Crea de nuevo el modelo general utilizando todos los datos
          model_g <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = gen)
          tabla$Spp <- unique(ind$species)
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
          
          
          tabla$Dif_t <- summary(model_int)$coefficients[4, 3]
          tabla$Dif_pvalue <- summary(model_int)$coefficients[4, 4]
          
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
  return(tabla_ind)
}

# spp_trend_percentil ----
spp_trend_percentil <- function(Data, spp, y, n_min =50, percentil, variable) {
  tabla_ind <- data.frame(
    "Percentil" = NA,
    "Spp" = NA,
    "Variable" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA,
    "Dif_t" = NA,
    "Dif_pvalue" = NA)
  
  tabla_ind <- tabla_ind[-1,]
  
  # Bucle para calcular las tendencias de cada una de las especies
  for (n in 1:length(spp)){
    # Bucle para actuar sobre cada una de las especies
    # Filtra la especie  
    
    ind <- Data %>%
      filter(species == spp[n]) %>%
      mutate(group = "i")
    pi <- quantile(ind[,variable], percentil)
  
    if (percentil == .50) {
      p55 <- quantile(ind[,variable], .55)
      p45 <- quantile(ind[,variable], .45)
      ind <- ind %>% 
        filter(between(ind[,variable], p45, p55))
      print("P45_P55")
    }else if(percentil < .50) {
      ind <- ind %>% 
        filter(ind[,variable] <= pi)
      print("P0_P10")
    } else {
      ind <- ind %>% 
        filter(ind[,variable] >= pi)
      print("P90_P100")
    }
   
    gen <- Data %>%
      mutate(group = "g")
    
    pg <- quantile(gen[,variable], percentil)
    
    if (percentil == .50) {
      p55 <- quantile(gen[,variable], .55)
      p45 <- quantile(gen[,variable], .45)
      gen <- gen %>% 
        filter(between(gen[,variable], p45, p55))
      print("P45_P55")
    }else if(percentil < .50) {
      gen <- gen %>% 
        filter(gen[,variable] <= pg)
      print("P0_P10")
    } else {
      gen <- gen %>% 
        filter(gen[,variable] >= pg)
      print("P90_P100")
    }
   
    dat <- rbind(gen, ind)
    
    if (nrow(ind) > n_min) {
      # Condicional "SI" para seleccionar aquellas especies con mas de n registros
      for (i in 1:length(y)) {
        # Bucle para cada una de las variables independientes
        tryCatch({
          # Implementa código que debe ejecutarse cuando se produce la condición de error
          tabla <-
            data.frame(
              # Crea tabla vacia para despues unificar a tabla de resultados
              "Percentil" = NA,
              "Spp" = NA,
              "Variable" = NA,
              "Trend" = NA,
              "t" = NA,
              "p" = NA,
              "P95_max" = NA,
              "P95_min" = NA,
              "Dif_t" = NA,
              "Dif_pvalue" = NA
            )
          
          # Crea de nuevo el modelo general utilizando todos los datos
          model_g <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = gen)
          tabla$Percentil <- percentil
          tabla$Spp <- unique(ind$species)
          tabla$Variable <- y[i]
          
          # Crea el modelo de cada especie para la posterior comparación, De aqui servirán los datos de tendencias
          model_i <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = ind)
          
          # Añade resultados en la tabla
          tabla$Trend <- model_i$coefficients[[2]]
          tabla$t <- round(summary(model_i)$coefficients[2, 3],4)
          tabla$p <- round(summary(model_i)$coefficients[2, 4],4)
          tabla$P95_max <- round(confint(model_i, "Año_Mes", level = .95)[, 2],4)
          tabla$P95_min <- round(confint(model_i, "Año_Mes", level = .95)[, 1],4)
          
          # Crea de nuevo el modelo de comparación y ver si existe diferencias acxorde al grupo
          model_int <- lm(formula(paste(y[i], paste(
            x, "*group", collapse = "+"
          ), sep = " ~ ")), data = dat)
          
          
          tabla$Dif_t <- round(summary(model_int)$coefficients[4, 3],4)
          tabla$Dif_pvalue <- round(summary(model_int)$coefficients[4, 4],4)
          
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
  return(tabla_ind)
}

# Map percentiles ----
#spp_plotting <- function(spp, Data, world_map, directorio) {
#for (i in 1:length(spp)){
#  ind <- Data %>%
#    filter(species == spp[i]) 
#  p10 <- quantile(ind$Lat , .1)
#  p45 <- quantile(ind$Lat, .45)
#  p55 <- quantile(ind$Lat, .55)
#  p90 <- quantile(ind$Lat, .9)
#  
#  ind_50 <- ind %>% 
#    filter(between(ind$Lat, p45, p55))
#  ind_10 <- ind %>% 
#    filter(ind$Lat <= p10)
#  ind_90 <- ind %>% 
#    filter(ind$Lat >= p90)
#  
#  ggplot()+
#    geom_sf(data = world_map)+
#    geom_point(data = ind, aes(Long, Lat, col = Año_Mes), alpha =.2)+
#    geom_point(data = ind_90, aes(Long, Lat, col = Año_Mes))+
#    geom_point(data = ind_50, aes(Long, Lat, col = Año_Mes))+
#    geom_point(data = ind_10, aes(Long, Lat, col = Año_Mes))+
#    labs(title = spp[i], subtitle = paste0("0.05%   count = ", nrow(ind)))+
#    scale_colour_viridis_c(option = "D",trans = "sqrt", alpha = .8)+
#    coord_sf(xlim = c(-20,50), ylim = c(35,75), expand = FALSE)+
#    theme(axis.text = element_text(angle = 45, size =8),
#          axis.title = element_blank(),
#          legend.title = element_blank())
#  ggsave(paste0(directorio, spp[i], "_005.jpeg"), width = 20, height = 20, units = "cm",dpi = 300)
#  }
#}

