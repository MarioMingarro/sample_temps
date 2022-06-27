library(readxl)
library(tidyverse)
Data <- read_excel("Data.xlsx")

x <- "Año_Mes"
y <- c("Lat", "Long", "ELEVATION", "TMAX", "TMIN")
# All species -----
# Latitude	Longitude	Elevation	Tªmax	Tªmin
tabla_final <- data.frame(
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "P95_max" = numeric(),
  "P95_min" = numeric(),
  "F" = numeric()
)

for (i in 1:5) {
  tabla <- data.frame(
    "Variable" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "95_max" = NA,
    "95_min" = NA,
    "F" = NA
  )
  tabla$Variable <- y[i]
  model_g <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = Data)
  tabla$Trend <- model_g$coefficients[[2]]
  tabla$t <- summary(model_g)$coefficients[2, 3]
  tabla$p <- summary(model_g)$coefficients[2, 4]
  tabla$X95_max <-  confint(model_g, "Año", level = .95)[, 2]
  tabla$X95_min <-  confint(model_g, "Año", level = .95)[, 1]
  tabla$F <- summary(model_g)$fstatistic[1]
  tabla_final <- rbind(tabla_final, tabla)
}

# Indivudual species

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
  "95_max" = numeric(),
  "95_min" = numeric(),
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
          "95_max" = NA,
          "95_min" = NA,
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

############################################


model_g = lm(Lat ~ Año, data = Data)
model_i = lm(Lat ~ Año, data = subset(Data,Data$Especie == "Volinus sticticus"))

b_g <- summary(model_g)$coefficients[2,1]
se_g <- summary(model_g)$coefficients[2,2]
b_i <- summary(model_i)$coefficients[2,1]
se_i <- summary(model_i)$coefficients[2,2]

p_value = 2*pnorm(-abs(compare.coeff(b_g,se_g,b_i,se_i)))
p_value #inf 0.05

############################################
spp[45]

ggplot() + 
  geom_smooth(data= Data, aes(x = Año, y = Altitud),col = "black", fill = "black", method = "lm") +
  geom_smooth(data= ind, aes(x = Año, y = Altitud),col = "red", fill = "red", method = "lm")+
  ggtitle(paste0(spp[1]))+
  labs(x= "Year", y = "Elevation")+
  theme_minimal()


###############################



g <- Data %>% select(Lat, Año_Mes) %>%
  mutate(df = "g")

i <-  Data %>%
  subset(Data$Especie == "Platytomus tibialis") %>%
  select(Lat, Año_Mes) %>%
  mutate(df = "i")

dat <- rbind(g,i)

ggplot(dat,aes(x=Año_Mes,y=Lat,col=df)) + 
  geom_point() + 
  geom_smooth(method="lm")

summary(lm(Lat~Año_Mes*df,data=dat))

