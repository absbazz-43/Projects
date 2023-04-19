## create a longituidinal data 

library(tidyverse)
da = data.frame(id = 1:5, month1= c(2.1,3.1,3.4,5.6,5), month2 = c(3.5,3.4,5.6,7,5),month3 = c(4.2,5.7,6.7,6,5.7))

long = da %>% 
  pivot_longer(names_to = 'v',values_to = 'value',2:4)
### another way  
long_data = gather(da, key = 'id', value= 'value', 2:4)
long_data  
##
boxplot( value ~ v,data = long )
library(lattice)
lattice::xyplot(value ~ id | v, data = long)

corrplot::corrplot.mixed(cor(da[,2:4]))
cor(da[,2:4])
