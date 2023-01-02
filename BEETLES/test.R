# Indivudual species

```{r,warning=FALSE}
spp <- unique(Data$Especie)

compare.coeff <- function(b_g,se_g,b_i,se_i){
  return((b_g-b_i)/sqrt(se_g^2+se_i^2))
}

tabla_ind <- data.frame(
  "Spp" = character(),
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "P95_max" = numeric(),
  "P95_min" = numeric(),
  "F" = numeric(),
  "Dif" = numeric()
)

for (n in 1:length(spp)) {
  ind <- filter(Data, Especie == spp[n])
  
  if (nrow(ind) > 10) {
    for (i in 1:5) {
      tryCatch({
        tabla <- data.frame(
          "Spp" = NA,
          "Variable" = NA,
          "Trend" = NA,
          "t" = NA,
          "p" = NA,
          "P95_max" = NA,
          "P95_min" = NA,
          "F" = NA,
          "Dif" = NA
        )
        #General
        model_g = lm(formula(paste(y[i], paste(
          x, collapse = "+"
        ), sep = " ~ ")), data = Data)
        
        tabla$Spp <- unique(ind[[1]])
        tabla$Variable <- y[i]
        model_i <-
          lm(formula(paste(y[i], paste(
            x, collapse = "+"
          ), sep = " ~ ")), data = ind)
        tabla$Trend <- model_i$coefficients[[2]]
        tabla$t <- summary(model_i)$coefficients[2, 3]
        tabla$p <- summary(model_i)$coefficients[2, 4]
        tabla$X95_max <-  confint(model_i, "Año", level = .95)[, 2]
        tabla$X95_min <-  confint(model_i, "Año", level = .95)[, 1]
        tabla$F <- summary(model_i)$fstatistic[1]
        
        
        
        b_g <- summary(model_g)$coefficients[2,1]
        se_g <- summary(model_g)$coefficients[2,2]
        b_i <- summary(model_i)$coefficients[2,1]
        se_i <- summary(model_i)$coefficients[2,2]
        
        
        tabla$Dif <- 2*pnorm(-abs(compare.coeff(b_g,se_g,b_i,se_i)))
        
        tabla_ind <- rbind(tabla_ind, tabla)
        
      }, error = function(e) {
        cat(
          paste0("WARNING: Specie ", ind[1, 1], " variable (", y[i], ") has"),
          conditionMessage(e),
          "\n"
        )
      })
    }
  } else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient"))
  }
}
```
# Variables
# Latitude
## Table

```{r}
tabla_ind_lat <- filter(tabla_ind, Variable == "Lat")
DT::datatable(tabla_ind_lat,
              class = "nowrap",
              filter = 'top',
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(
                               width = "50%",
                               targets = 0))))
```

## Graphs
```{r, warning=FALSE, message=FALSE}
for ( i in 1:length(spp)){
  ind <- filter(Data, Especie == spp[i])
  if (nrow(ind) > 10) {
    print(ggplot() + 
            geom_smooth(data= Data, aes(x = Año, y = Lat),col = "black", fill = "black", method = "lm") +
            geom_smooth(data= ind, aes(x = Año, y = Lat),col = "red", fill = "red", method = "lm")+
            ggtitle(paste0(spp[i]))+
            labs(x= "Year", y = "Latitude")+
            theme_minimal())
  }else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient")) 
  }
}
```

# Longitude

## Table

```{r}
tabla_ind_lon <- filter(tabla_ind, Variable == "Long")
DT::datatable(tabla_ind_lon,
              class = "nowrap",
              filter = 'top',
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(
                               width = "50%",
                               targets = 0))))
```

## Graphs
```{r, warning=FALSE, message=FALSE}
for ( i in 1:length(spp)){
  ind <- filter(Data, Especie == spp[i])
  if (nrow(ind) > 10) {
    print(ggplot() + 
            geom_smooth(data= Data, aes(x = Año, y = Long),col = "black", fill = "black", method = "lm") +
            geom_smooth(data= ind, aes(x = Año, y = Long),col = "red", fill = "red", method = "lm")+
            ggtitle(paste0(spp[i]))+
            labs(x= "Year", y = "Longitude")+
            theme_minimal())
  }else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient")) 
  }
}
```

# Elevation

## Table
```{r}
tabla_ind_elev <- filter(tabla_ind, Variable == "Altitud")
DT::datatable(tabla_ind_elev,
              class = "nowrap",
              filter = 'top',
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(
                               width = "50%",
                               targets = 0))))
```

## Graphs
```{r, warning=FALSE, message=FALSE}
for ( i in 1:length(spp)){
  ind <- filter(Data, Especie == spp[i])
  if (nrow(ind) > 10) {
    print(ggplot() + 
            geom_smooth(data= Data, aes(x = Año, y = Altitud),col = "black", fill = "black", method = "lm") +
            geom_smooth(data= ind, aes(x = Año, y = Altitud),col = "red", fill = "red", method = "lm")+
            ggtitle(paste0(spp[i]))+
            labs(x= "Year", y = "Elevation")+
            theme_minimal())
  }else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient")) 
  }
}
```

# Maximum temperature

## Table
```{r}
tabla_ind_tmax <- filter(tabla_ind, Variable == "TMAX")
DT::datatable(tabla_ind_tmax,
              class = "nowrap",
              filter = 'top',
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(
                               width = "50%",
                               targets = 0))))
```

## Graphs
```{r, warning=FALSE, message=FALSE}
for ( i in 1:length(spp)){
  ind <- filter(Data, Especie == spp[i])
  if (nrow(ind) > 10) {
    print(ggplot() + 
            geom_smooth(data= Data, aes(x = Año, y = TMAX),col = "black", fill = "black", method = "lm") +
            geom_smooth(data= ind, aes(x = Año, y = TMAX),col = "red", fill = "red", method = "lm")+
            ggtitle(paste0(spp[i]))+
            labs(x= "Year", y = "Maximum temperature")+
            theme_minimal())
  }else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient")) 
  }
}
```

# Minimum temperature

## Table
```{r}
tabla_ind_tmin <- filter(tabla_ind, Variable == "TMIN")
DT::datatable(tabla_ind_lat,
              class = "nowrap",
              filter = 'top',
              options = list(autoWidth = TRUE,
                             columnDefs = list(list(
                               width = "50%",
                               targets = 0))))
```

## Graphs
```{r, warning=FALSE, message=FALSE}
for ( i in 1:length(spp)){
  ind <- filter(Data, Especie == spp[i])
  if (nrow(ind) > 10) {
    print(ggplot() + 
            geom_smooth(data= Data, aes(x = Año, y = TMIN),col = "black", fill = "black", method = "lm") +
            geom_smooth(data= ind, aes(x = Año, y = TMIN),col = "red", fill = "red", method = "lm")+
            ggtitle(paste0(spp[i]))+
            labs(x= "Year", y = "Minimum temperature")+
            theme_minimal())
  }else{
    print(paste0("Data for ", ind[1, 1], " specie are insufficient")) 
  }
}
```

















library(readxl)
REDES22 <- read_excel("C:/Users/mario/Downloads/REDES22.xlsx", sheet = "Q22dB2")
