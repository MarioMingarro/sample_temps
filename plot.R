library(reshape2)
library(stringr)
library(tidyverse)


TMX_df <- TMX_df[-1,3:118]
TMX_df <-  as.data.frame(colMeans(TMX_df, na.rm = TRUE))

TMX_df[c('a', 'year')] <- str_split_fixed(rownames(TMX_df), '_', 2)
TMX_df <- TMX_df[-c(1,2),-2]
colnames(TMX_df) <- c("temp", "year")

TMX_df$year <- as.numeric(TMX_df$year)

TMX_pre <- TMX_df %>% 
  filter(TMX_df$year<=1986)
TMX_post <- TMX_df %>% 
  filter(TMX_df$year>=1986)
ggplot() + 
  geom_point(data= TMX_df, aes(x = year, y = temp), alpha = .2) +
  geom_smooth(data= TMX_df, aes(x = year, y = temp),col = "orange", se = FALSE, method = "lm")+
  geom_smooth(data= TMX_pre, aes(x = year, y = temp),col = "darkcyan", se = FALSE, method = "lm")+
  geom_smooth(data= TMX_post, aes(x = year, y = temp),col = "darkblue", se = FALSE, method = "lm")+
  geom_vline(xintercept = 1980, linetype="dotted", 
             color = "black", size=1, alpha = .5)+
  labs(x= "Year", y = "Maximum temperature (ºC)")+
  theme_minimal()

####♣


TMN_df <- TMN_df[-1,3:118]
TMN_df <-  as.data.frame(colMeans(TMN_df, na.rm = TRUE))

TMN_df[c('a', 'year')] <- str_split_fixed(rownames(TMN_df), '_', 2)
TMN_df <- TMN_df[-c(1,2),-2]
colnames(TMN_df) <- c("temp", "year")

TMN_df$year <- as.numeric(TMN_df$year)

TMN_pre <- TMN_df %>% 
  filter(TMN_df$year<=1980)
TMN_post <- TMN_df %>% 
  filter(TMN_df$year>=1980)
ggplot() + 
  geom_point(data= TMN_df, aes(x = year, y = temp), alpha = .2) +
  geom_smooth(data= TMN_df, aes(x = year, y = temp),col = "orange", se = FALSE, method = "lm")+
  geom_smooth(data= TMN_pre, aes(x = year, y = temp),col = "darkcyan", se = FALSE, method = "lm")+
  geom_smooth(data= TMN_post, aes(x = year, y = temp),col = "darkblue", se = FALSE, method = "lm")+
  geom_vline(xintercept = 1986, linetype="dotted", 
             color = "black", size=1, alpha = .5)+
  labs(x= "Year", y = "Minimum temperature (ºC)")+
  theme_minimal()
