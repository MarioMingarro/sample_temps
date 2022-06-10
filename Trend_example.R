library(readxl)
library(tidyverse)
Data <- read_excel("Data.xlsx")

x <- "Año"
y <- c("Lat", "Long", "Altitud", "TMAX", "TMIN")
# All species -----
# Latitude	Longitude	Elevation	Tªmax	Tªmin
tabla_final <- data.frame(
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "95_max" = numeric(),
  "95_min" = numeric(),
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
  model <- lm(formula(paste(y[i], paste(x, collapse = "+"), sep = " ~ ")), data = Data)
  tabla$Trend <- model$coefficients[[2]]
  tabla$t <- summary(model)$coefficients[2, 3]
  tabla$p <- summary(model)$coefficients[2, 4]
  tabla$X95_max <-  confint(model, "Año", level = .95)[, 2]
  tabla$X95_min <-  confint(model, "Año", level = .95)[, 1]
  tabla$F <- summary(model)$fstatistic[1]
  tabla_final <- rbind(tabla_final, tabla)
}

# Indivudual species

spp <- unique(Data$Especie)

ind <- filter(Data, Especie == spp[n])

tabla_ind <- data.frame(
  "Spp" = character(),
  "Variable"= character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "95_max" = numeric(),
  "95_min" = numeric(),
  "F" = numeric()
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
          "F" = NA
        )
        tabla$Spp <- ind[1, 1]
        tabla$Variable <- y[i]
        model <-
          lm(formula(paste(y[i], paste(
            x, collapse = "+"
          ), sep = " ~ ")), data = ind)
        tabla$Trend <- model$coefficients[[2]]
        tabla$t <- summary(model)$coefficients[2, 3]
        tabla$p <- summary(model)$coefficients[2, 4]
        tabla$X95_max <-  confint(model, "Año", level = .95)[, 2]
        tabla$X95_min <-  confint(model, "Año", level = .95)[, 1]
        tabla$F <- summary(model)$fstatistic[1]
        tabla_ind <-
          rbind(tabla_ind, tabla)
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
