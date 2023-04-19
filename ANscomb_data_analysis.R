library(tidyverse)
anscomb <- ANscomb_datset %>% 
  rename(
    "data.1.x" = "x" ,
    "data.1.y" =  "y",
     "data.2.x" = "x.1",
     "data.2.y" = "y.1",
     "data.3.x" = "x.2",
     "data.3.y" = "y.2",
     "data.4.x" = "x.3",
     "data.4.y" = "y.3"
  )
  
#head(ANscomb_datset)
head(anscomb)


library(ggplot2)
library(ggpubr)

p1 <- ggplot(anscomb, aes(x = data.1.x, y = data.1.y)) + geom_point() +geom_smooth(method = "lm",se = FALSE)
p1  


p2 <- ggplot(anscomb, aes(x = data.2.x, y = data.2.y)) + geom_point() +geom_smooth(method = "lm",se = FALSE)
p2

p3 <- ggplot(anscomb, aes(x = data.3.x, y = data.3.y)) + geom_point() +geom_smooth(method = "lm",se = FALSE)
p3 

p4 <- ggplot(anscomb, aes(x = data.4.x, y = data.4.y)) + geom_point() +geom_smooth(method = "lm",se = FALSE)
p4 


ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2)


## DFescriptive analysis 


skimr::skim(anscomb)
