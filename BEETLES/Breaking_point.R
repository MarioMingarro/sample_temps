library(raster)
library(tidyverse)
library(strucchange)
library(reshape2)
library(Kendall)

TMX <- raster::stack()
for (i in 1901:2016){
  raster <- raster::stack(list.files("E:/DATA/CHELSA_SPAIN/OLD/TMAX", pattern = paste0(i), full.names = TRUE))/10
  raster <- raster::calc(raster, mean)
  TMX <- raster::stack(TMX, raster)
}

names(TMX) <- paste0("y_", seq(1901,2016, by = 1))


#
TMX_df <- as.data.frame(TMX, xy=TRUE)
TMX_df_mean <- as.data.frame(colMeans(TMX_df, na.rm = TRUE))
TMX_df_mean[c('a', 'year')] <- str_split_fixed(rownames(TMX_df_mean), '_', 2)

TMX_df_mean <- TMX_df_mean[-c(1,2),-2]
colnames(TMX_df_mean) <- c("y", "x")
#

TMX_df <- as.data.frame(TMX, xy=TRUE)
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
Mann_pre <- MannKendall(pre)
RSE_pre <- sqrt(deviance(lm_pre)/df.residual(lm_pre))

##post
post <- ss[bp$breakpoints:length(ss)]
year_post <- seq(year_break,2016,1)
lm_post <- lm(post ~ year_post)
P_post <- round(lm_post$coefficients[2],4)
Mann_post <- MannKendall(post)
RSE_post <- sqrt(deviance(lm_post)/df.residual(lm_post))

##general
year_total <- seq(1901,2016,1)
lm_total <- lm(ss ~ year_total)
P_total <- round(lm_total$coefficients[2],4)
Mann_gen <- MannKendall(year_total)
RSE_total <- sqrt(deviance(lm_total)/df.residual(lm_total))

# Test the null hypothesis that the annual temperature remains constant over the years
test <- strucchange::sctest(qlr, type = "supF")
F.sup <- test[1]
p.value <- test[2]

result_TMX <- data.frame("year" = year_break, 
                         "RSS" = RSS, 
                         "F" = F.sup, 
                         "p" = p.value, 
                         "General trend"= P_total, 
                         "Mann general" = Mann_gen[2], 
                         "RSE general" = RSE_total,
                         "Before BP trend" = P_pre, 
                         "Mann Before BP" = Mann_pre[2], 
                         "RSE Before BP" = RSE_pre,
                         "After BP trend" = P_post, 
                         "Mann After BP" = Mann_post[2], 
                         "RSE After BP" = RSE_post)
colnames(result_TMX) <-  c(
  "year",
  "RSS",
  "F",
  "p",
  "General trend",
  "Mann general",
  "RSE general",
  "Before BP trend",
  "Mann Before BP",
  "RSE Before BP",
  "After BP trend",
  "Mann After BP",
  "RSE After BP"
)


library(writexl)
write_xlsx(result_TMX, "D:/MNCN/JORGE/TEMP_SAMPLES_BEETLES/BP_results/tmax_result.xlsx")



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
Mann_pre <- MannKendall(pre)
RSE_pre <- sqrt(deviance(lm_pre)/df.residual(lm_pre))

##post
post <- ss[bp$breakpoints:length(ss)]
year_post <- seq(year_break,2016,1)
lm_post <- lm(post ~ year_post)
P_post <- round(lm_post$coefficients[2],4)
Mann_post <- MannKendall(post)
RSE_post <- sqrt(deviance(lm_post)/df.residual(lm_post))

##general
year_total <- seq(1901,2016,1)
lm_total <- lm(ss ~ year_total)
P_total <- round(lm_total$coefficients[2],4)
Mann_gen <- MannKendall(year_total)
RSE_total <- sqrt(deviance(lm_total)/df.residual(lm_total))

# Test the null hypothesis that the annual temperature remains constant over the years
test <- strucchange::sctest(qlr, type = "supF")
F.sup <- test[1]
p.value <- test[2]

result_TMN <- data.frame("year" = year_break, 
                         "RSS" = RSS, 
                         "F" = F.sup, 
                         "p" = p.value, 
                         "General trend"= P_total, 
                         "Mann general" = Mann_gen[2], 
                         "RSE general" = RSE_total,
                         "Before BP trend" = P_pre, 
                         "Mann Before BP" = Mann_pre[2], 
                         "RSE Before BP" = RSE_pre,
                         "After BP trend" = P_post, 
                         "Mann After BP" = Mann_post[2], 
                         "RSE After BP" = RSE_post)
colnames(result_TMN) <-  c(
  "year",
  "RSS",
  "F",
  "p",
  "General trend",
  "Mann general",
  "RSE general",
  "Before BP trend",
  "Mann Before BP",
  "RSE Before BP",
  "After BP trend",
  "Mann After BP",
  "RSE After BP"
)


library(writexl)
write_xlsx(result_TMN, "D:/MNCN/JORGE/TEMP_SAMPLES_BEETLES/BP_results/tmin_result.xlsx")
