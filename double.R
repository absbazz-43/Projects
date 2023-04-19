library(tidyverse)
library(TTR)
library(forecast)
ad = iris[,4:5] %>%
  group_by(Species) %>%
  nest() %>%
  mutate(m = map(data, ~SMA(., n =2))) %>%
  mutate(fd = map(m, forecast,h=5))
ad$fd


ad = iris[,4:5] %>%
  group_by(Species) %>%
  nest() %>%
  mutate(j = map(data, ts, frequency=1))%>%
  mutate(m = map(j, ~SMA(., n =2))) %>%
  mutate(fd = map(m, forecast,h=5))
ad$fd

ad$m

a = str_remove(c("65+","_","56-"),"[^[:alnum:]]")
a


jk <- iris[,4:5] %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(m = map(data,ts)) %>% 
  mutate(k = map(m, auto.arima)) %>% 
  mutate(mj = list(map2(m,k,~forecast(.x,.y,h=1)))) %>% 
  unnest(mj)
jk$mj
jk$k
jk <- iris[,4:5] %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(m = map(data,ts)) %>% 
  mutate(k = map(m, auto.arima)) %>% 
  mutate(mj = list(map2(m,k,~forecast(.,.,h=1)))) %>% 
  unnest(mj)

jk$mj[[1]]
a <- list(1,2,3)
b <- list(2,3,4)
map2(a,b,~.x+.y)







##############  


a = matrix(1:9, ncol =3, nrow =3)
a

b = matrix(0, ncol = 2, nrow =2)
for ( i in 1:2){
  for(j in i:(i+1)){
    for(k in i:(i+1)){
      b[j,k]=max(a[i:(i+1),i:(i+1)])
    }
  }
}


v= c()  
for(i in 1:2){
  for(j in i:(i+1)){
    v = append(v,max(a[j:(i+1),j:(i+1)]))
  }
}

v
    

