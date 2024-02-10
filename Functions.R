packages.to.use <- c("readxl",
                     "tidyverse",
                     "writexl",
                     "tictoc",
                     "jtools")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) & package == "VoCC" ) { devtools::install_github("JorGarMol/VoCC", dependencies = TRUE) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}

# Funciones -----

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
    tabla$Trend <- round(model_g$coefficients[[2]], 2) # Tendencia
    tabla$t <- round(summary(model_g)$coefficients[2, 3], 4) # t del modelo
    tabla$p <- round(summary(model_g)$coefficients[2, 4], 4) # p del modelo
    tabla$P95_max <- round(confint(model_g, "Año_Mes", level = .95)[, 2], 2) # Intervalo de confianza max del 95%
    tabla$P95_min <- round(confint(model_g, "Año_Mes", level = .95)[, 1], 2) # Intervalo de confianza min del 95%
    tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una de las tablas individuales
  }
  return(tabla_general)
}

spp_trend <- function(Data, spp, y, umbral = 50) {
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
    
    if (nrow(ind) > umbral) {
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


spp_trend_percentil <- function(Data, spp, y, umbral =50, percentil, x) {
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
    
    pi <- quantile(ind$Lat, percentil)
    
    
    ## SUSTITUIR ELIF
    if (percentil == 50) {
      ind <- ind %>% 
        filter(ind[,x] <= pi+5)%>% 
        filter(ind[,x] >= pi-5)
    }else if(percentil < 50) {
      ind <- ind %>% 
        filter(ind[,x] <= pi)
    } else {
      ind <- ind %>% 
        filter(ind[,x] >= pi)
    }
   
    gen <- Data %>%
      mutate(group = "g")
    
    pg <- quantile(gen$Lat, percentil)
    
    if (percentil == 50) {
      gen <- gen %>% 
        filter(gen[,x] <= pg+5)%>% 
        filter(gen[,x] >= pg-5)
    }else if(percentil < 50) {
      gen <- gen %>% 
        filter(gen[,x] <= pg)
    } else {
      gen <- gen %>% 
        filter(gen[,x] >= pg)
    }
   
    dat <- rbind(gen, ind)
    
    if (nrow(ind) > umbral) {
      # Condicional "SI" para seleccionar aquellas especies con mas de 10 registros
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
