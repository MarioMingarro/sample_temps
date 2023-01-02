library(raster)
library(tidyverse)
library(strucchange)
library(reshape2)

TMX <- raster::stack()
for (i in 1901:2016){
  raster <- raster::stack(list.files("E:/DATA/CHELSA_SPAIN/OLD/TMAX", pattern = paste0(i), full.names = TRUE))/10
  for (j in 1:12){
    raster <- raster::calc(raster::subset(j, raster,names(my_stack), value = T), mean)
    TMX <- raster::stack(TMX, raster)
  }
}

raster::subset(my_stack, grep('_GAM', names(my_stack), value = T))

names(TMX) <- paste0(seq(1901,2016, by = 1))

TMX_df <- as.data.frame(TMX, xy=TRUE)
###↕

kk <- melt(TMX_df[,3:118])
kk <- cbind(kk, substr(kk$variable, 2, 6))
kk <- na.omit(kk[,2:3])
colnames(kk) <- c("TMAX", "YEAR")
ggplot(kk, aes(YEAR, TMAX))+
  geom_point()

####
TMX_df_mean <- colMeans(TMX_df, na.rm = TRUE)

ss <- ts(TMX_df_mean[3:118],
         start = 1901,
         end = 2016,
         frequency = 1)
qlr <- strucchange::Fstats(ss ~ 1, data = ss) #Quandt Likelihood Ratio (QLR)
bp <- strucchange::breakpoints(qlr)
year_break <- strucchange::breakdates(bp)
RSS <- bp$RSS

##pre
pre <- ss[1:bp$breakpoints]
year_pre <- seq(1901, year_break, 1)
lm_pre <- lm(pre ~ year_pre)
P_pre <- round(lm_pre$coefficients[2],4)
AIC_pre <- AIC(lm_pre)
BIC_pre <- BIC(lm_pre)
RSE_pre <- sqrt(deviance(lm_pre)/df.residual(lm_pre))

##post
post <- ss[bp$breakpoints:length(ss)]
year_post <- seq(year_break,2016,1)
lm_post <- lm(post ~ year_post)
P_post <- round(lm_post$coefficients[2],4)
AIC_post <- AIC(lm_post)
BIC_post <- BIC(lm_post)
RSE_post <- sqrt(deviance(lm_post)/df.residual(lm_post))

##general
year_total <- seq(1901,2016,1)
lm_total <- lm(ss ~ year_total)
P_total <- round(lm_total$coefficients[2],4)
AIC_total <- AIC(lm_total)
BIC_total <- BIC(lm_total)
RSE_total <- sqrt(deviance(lm_total)/df.residual(lm_total))

# Test the null hypothesis that the annual temperature remains constant over the years
test <- strucchange::sctest(qlr, type = "supF")
F.sup <- test[1]
p.value <- test[2]

result_TMX <- data.frame(year_break, RSS, F.sup, p.value, 
                         P_total, AIC_total, BIC_total, RSE_total,
                         P_pre, AIC_pre, BIC_pre,RSE_pre,
                         P_post,AIC_post, BIC_post, RSE_post)

######
colnames(TMX_df)

TMX_df_pre <- dplyr::select(TMX_df, c("x", "y", "y_1901","y_1902","y_1903","y_1904","y_1905","y_1906","y_1907","y_1908","y_1909","y_1910",
                                      "y_1911","y_1912","y_1913","y_1914","y_1915","y_1916","y_1917","y_1918","y_1919","y_1920",
                                      "y_1921","y_1922","y_1923","y_1924","y_1925","y_1926","y_1927","y_1928","y_1929","y_1930",
                                      "y_1931","y_1932","y_1933","y_1934","y_1935","y_1936","y_1937","y_1938","y_1939","y_1940",
                                      "y_1941","y_1942","y_1943","y_1944","y_1945","y_1946","y_1947","y_1948","y_1949","y_1950",
                                      "y_1951","y_1952","y_1953","y_1954","y_1955","y_1956","y_1957","y_1958","y_1959","y_1960",
                                      "y_1961","y_1962","y_1963","y_1964","y_1965","y_1966","y_1967","y_1968","y_1969","y_1970",
                                      "y_1971","y_1972","y_1973","y_1974","y_1975","y_1976","y_1977","y_1978","y_1979",))

TMX_df_post <- dplyr::select(TMX_df, c("x", "y","y_1980","y_1981","y_1982","y_1983","y_1984","y_1985","y_1986","y_1987","y_1988","y_1989",
                                       "y_1990","y_1991","y_1992","y_1993","y_1994","y_1995","y_1996","y_1997","y_1998","y_1999","y_2000",
                                       "y_2001","y_2002","y_2003","y_2004","y_2005","y_2006","y_2007","y_2008","y_2009","y_2010","y_2011",
                                       "y_2012","y_2013","y_2014","y_2015","y_2016"))


kk <- dplyr::filter(TMX_df, rowSums(is.na(TMX_df[,3:118])) != ncol(TMX_df[,3:118]))

TMX_df_pre <- as.data.frame(rbind(TMX_df_pre, seq(1901,1979, by = 1)))
library(xlsx)
write_xlsx(TMX_df_pre, "A:/BEETLES_TREND/TMX/TMX_df_pre.xlsx")
write.csv(TMX_df_pre, "A:/BEETLES_TREND/TMX_df.csv")
write_xlsx(kk, "A:/BEETLES_TREND/TMX_df.xlsx")
write_x
library(reshape2)
kk <- melt(TMX_df_pre)
for(i in 3:nrow(TMX_df_pre)){
  kk <- TMX_df_pre[c(i,1528213),3:length(TMX_df_pre)]
  rownames(kk) <- c("TMX", "YEAR")
  kk <- melt(kk)
}
i=3
#####

############################

TMN <- raster::stack()
for (i in 1901:2016){
  raster <- raster::stack(list.files("E:/DATA/CHELSA_SPAIN/OLD/TMIN", pattern = paste0(i), full.names = TRUE))/10
  raster <- calc(raster, mean)
  TMN <- raster::stack(TMN, raster)
}

names(TMN) <- paste0("y_", seq(1901,2016, by = 1))

TMN_df <- as.data.frame(TMN, xy=TRUE)

TMN_df_mean <- colMeans(TMN_df, na.rm = TRUE)

ss <- ts(TMN_df_mean[3:118],
         start = 1901,
         end = 2016,
         frequency = 1)
qlr <- strucchange::Fstats(ss ~ 1, data = ss) #Quandt Likelihood Ratio (QLR)
bp <- strucchange::breakpoints(qlr)
year_break <- strucchange::breakdates(bp)
RSS <- bp$RSS

##pre
pre <- ss[1:bp$breakpoints]
year_pre <- seq(1901, year_break, 1)
lm_pre <- lm(pre ~ year_pre)
P_pre <- round(lm_pre$coefficients[2],4)
AIC_pre <- AIC(lm_pre)
BIC_pre <- BIC(lm_pre)
RSE_pre <- sqrt(deviance(lm_pre)/df.residual(lm_pre))

##post
post <- ss[bp$breakpoints:length(ss)]
year_post <- seq(year_break,2016,1)
lm_post <- lm(post ~ year_post)
P_post <- round(lm_post$coefficients[2],4)
AIC_post <- AIC(lm_post)
BIC_post <- BIC(lm_post)
RSE_post <- sqrt(deviance(lm_post)/df.residual(lm_post))

##general
year_total <- seq(1901,2016,1)
lm_total <- lm(ss ~ year_total)
P_total <- round(lm_total$coefficients[2],4)
AIC_total <- AIC(lm_total)
BIC_total <- BIC(lm_total)
RSE_total <- sqrt(deviance(lm_total)/df.residual(lm_total))

# Test the null hypothesis that the annual temperature remains constant over the years
test <- strucchange::sctest(qlr, type = "supF")
F.sup <- test[1]
p.value <- test[2]

result_TMN <- data.frame(year_break, RSS, F.sup, p.value, 
                         P_total, AIC_total, BIC_total, RSE_total,
                         P_pre, AIC_pre, BIC_pre,RSE_pre,
                         P_post,AIC_post, BIC_post, RSE_post)


kk <- dplyr::filter(TMN_df, rowSums(is.na(TMN_df[,3:118])) != ncol(TMN_df[,3:118]))

kk <- rbind(result_TMN, result_TMX)
write.csv(kk, "D:/MNCN/JORGE/TEMP_SAMPLES_BEETLES/AA/BP_results.csv")

Mario Mingarro López
