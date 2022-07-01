library(readxl)
library(tidyverse)

Data <- read_excel("Data.xlsx") # Cargamos datos

x <- "Año_Mes" # Variable dependiente
y <- c("Lat", "Long", "ELEVATION", "TMAX", "TMIN") # Variables independiente

# Generamos la tabla con las tendencias y otras medidas de las tendencias para el set global de datos

# All species -----

# Crear tabla vacia para almacenar los resultados
tabla_general <- data.frame(
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "P95_max" = numeric(),
  "P95_min" = numeric(),
  "F" = numeric()
)


for (i in 1:length(y)) {        # Bucle para calcular las estadisticas de todas las variables independientes
  tabla <- data.frame(          # Tabla vacía donde se guardan resultados de cada variable independiente 
    "Variable" = NA,            # para despues unir a la tabla general
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "P95_max" = NA,
    "P95_min" = NA,
    "F" = NA
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
  tabla$F <- summary(model_g)$fstatistic[1] # F del modelo
  tabla_general <- rbind(tabla_general, tabla) # Unimos las filas de la tabla general con cada una d elas tablas individuales
}

# Generamos la tabla con las tendencias y otras medidas de las tendencias para cada una de las especies
# Además comparamos la tendencia de cada una de las especies con la tendencia del conjunto de datos

# Indivudual species ----

spp <- unique(Data$Especie) # Creamos un vector con los nombres de las especies

# Creamos una función para comparar las tendencias
compare.coeff <- function(b_g,se_g,b_i,se_i){
  return((b_g-b_i)/sqrt(se_g^2+se_i^2))
}

# Tabla vacía para guardar los resultados 
tabla_ind <- data.frame(
  "Spp" = character(),
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "95_max" = numeric(),
  "95_min" = numeric(),
  "F" = numeric(),
  "Dif" = numeric()
)

# Bucle para calcular las tendencias de cada una de las especies

for (n in 1:3) {           #length(spp)                  # Bucle para actuar sobre cada una de las especies
  ind <- Data %>% 
    filter( Especie == spp[n]) %>%
    mutate(group = "i")              # Filtra la especie 
  
  if (nrow(ind) > 10) {                               # Condicional "SI" para seleccionar aquellas especies con mas de 10 registros
    for (i in 1:2) {                                  # bucle para cada una de las variables independientes
      tryCatch({                                      # Implementa código que debe ejecutarse cuando se produce la condición de error
        tabla <- data.frame(                          # Crea tabla vacia para despues unificar a tabla de resultados
          "Spp" = NA,                                 
          "Variable" = NA,
          "Trend" = NA,
          "t" = NA,
          "p" = NA,
          "P95_max" = NA,
          "P95_min" = NA,
          "F" = NA,
          "Dif_1_pvalue" = NA,
          "Dif_2_coef" = NA,
          "Dif_2_pvalue" = NA,
          "Dif_2_F" = NA
        )
        # General
        model_g = lm(formula(paste(y[i], paste(  # Crea de nuevo el modelo general para la posterior comparación
          x, collapse = "+"
        ), sep = " ~ ")), data = Data)
        
        tabla$Spp <- unique(ind[[1]])
        tabla$Variable <- y[i]
        model_i <-                             # Crea el modelo de cada especie para la posterior comparación
          lm(formula(paste(y[i], paste(
            x, collapse = "+"
          ), sep = " ~ ")), data = ind)
        
        tabla$Trend <- model_i$coefficients[[2]]
        tabla$t <- summary(model_i)$coefficients[2, 3]
        tabla$p <- summary(model_i)$coefficients[2, 4]
        tabla$P95_max <-  confint(model_i, "Año_Mes", level = .95)[, 2]
        tabla$P95_min <-  confint(model_i, "Año_Mes", level = .95)[, 1]
        tabla$F <- summary(model_i)$fstatistic[1]
        
        
        # Dos formas de ver si las tendencias de las especies difieren de la general
        
        # Método 1
        # Obtener una estadística z al encontrar la diferencia entre los dos coeficientes 
        # y luego dividirla por un error estándar combinado. Ver https://www.jstor.org/stable/2782277?seq=1#page_scan_tab_contents
        
        b_g <- summary(model_g)$coefficients[2,1] # Tendencia general
        se_g <- summary(model_g)$coefficients[2,2] # Desviación estándar general
        b_i <- summary(model_i)$coefficients[2,1] # Tendencia especie del bucle
        se_i <- summary(model_i)$coefficients[2,2] # desciación estándar de especie del bucle
        
        
        tabla$Dif_1_pvalue <- 2*pnorm(-abs(compare.coeff(b_g,se_g,b_i,se_i))) # Fórmula utilizando la función "compare.coeff"
        
        # Metodo 2  
        # Crear un modelo incroporando la interación de la pendiente general con la de la especie Variable independiente*Año_Mes
        # el coeficiente indica que la pendiente d ela especie es mayor o menor que la pendiente del conjunto de datos.
        # Tambien observamos el pvalor para ver si es significativo (inf a 0,05 son pendientes diferentes)
        
        
        gen <- Data %>%
          mutate(group = "g")
        
        dat <- rbind(gen,ind)
        
        
        model_int = lm(formula(paste(y[i], paste(  # Crea de nuevo el modelo general para la posterior comparación
          x,"*group", collapse = "+"
        ), sep = " ~ ")), data = dat)
        
       
        tabla$Dif_2_coef <- summary(model_int)$coefficients[4,1]
        tabla$Dif_2_pvalue <- summary(model_int)$coefficients[4,4]
        tabla$Dif_2_F <- summary(model_int)$fstatistic[1]
       
        tabla_ind <- rbind(tabla_ind, tabla)  # Unimos tablas
        
      }, error = function(e) {                        # Si la función tryChach da error ejecuta esta parte del codigo
        cat(
          paste0("WARNING: Specie ", ind[1, 1], " variable (", y[i], ") has"), # Indica que especie tiene el problema y por qué
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



############################################
spp[45]

ggplot() + 
  geom_smooth(data= Data, aes(x = Año, y = Altitud),col = "black", fill = "black", method = "lm") +
  geom_smooth(data= ind, aes(x = Año, y = Altitud),col = "red", fill = "red", method = "lm")+
  ggtitle(paste0(spp[1]))+
  labs(x= "Year", y = "Elevation")+
  theme_minimal()


###############################

# ind <-  Data %>%
#   subset(Data$Especie == "Platytomus tibialis") %>%
#   select(Lat, Año_Mes) %>%
#   mutate(df = "i")

ggplot(dat,aes(x=Año_Mes,y=Lat,col=df)) + 
  # geom_point() + 
  #   geom_smooth(method="lm")

  # model_int <- lm(Lat~Año_Mes*df,data=dat)
  #

############################################


model_g = lm(Lat ~ Año, data = Data)
model_i = lm(Lat ~ Año, data = subset(Data,Data$Especie == "Volinus sticticus"))

b_g <- summary(model_g)$coefficients[2,1]
se_g <- summary(model_g)$coefficients[2,2]
b_i <- summary(model_i)$coefficients[2,1]
se_i <- summary(model_i)$coefficients[2,2]

p_value = 2*pnorm(-abs(compare.coeff(b_g,se_g,b_i,se_i)))
p_value #inf 0.05