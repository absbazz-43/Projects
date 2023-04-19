# input the data

## library

library(tidyverse)
library(caret)
b = na.omit(b)

b = countries.of.the.world
b$Agriculture = as.numeric(gsub(",",".", b$Agriculture))
b$Literacy....= as.numeric(gsub(",",".", b$Literacy....))
suma_region = b %>% 
  group_by(Region) %>% 
  summarise(median_literacy = median(Literacy....,na.rm = TRUE), median_agro = median(Agriculture, na.rm = TRUE), median_gdp_capita = median(GDP....per.capita., na.rm = TRUE))

suma_region

bg <- b %>% 
  select(Region, Literacy....,Agriculture, GDP....per.capita.)

str(bg)
after_imp = bg %>% 
  gather(key = key, value = value, 2:4 ) %>% 
  group_by(key, Region) %>% 
  mutate(value = ifelse(is.na(value),median(value,na.rm = TRUE), value)) %>% 
  ungroup() 

GDP  <- after_imp %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  unnest()

GDP

table(is.na(GDP))

skim(GDP)
ncol(b)


#########  pre analysis

for(i in 1:ncol(b)){
  b[,i]= gsub(",",".",b[,i])

}

bi <- as.data.frame(sapply(b[,3:20],as.numeric))

b <-  data.frame(Country = b$Country, Region = b$Region, bi)
preana <- b %>%
  gather(key = key , value = value, -c(1,2)) %>% 
  group_by(Region) %>% 
  mutate(value = ifelse(is.na(value), median(value,na.rm = TRUE), value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  unnest()

prm = data.frame(preana)

colMeans(prm[,-c(1,2)])
heatmap(cor(prm[,-c(1,2)]))
gdp_num = prm[,-c(1,2)]
corrplot::corrplot(cor(gdp_num))
library(corrplot)
head(cp,2)
cp = melt(cor(gdp_num))
ggplot(cp,aes(x=Var1, y=Var2, fill=round(value,2))) +geom_tile()+geom_text(aes(Var1,Var2,label = round(value,2)))

head(prm,2)
ggplot(prm %>% head(10), aes( x = Country,y = sort(GDP....per.capita.,decreasing = FALSE ))) +geom_bar(stat = "identity")

pairs(gdp_num)


