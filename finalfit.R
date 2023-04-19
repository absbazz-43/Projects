library(finalfit)

colon_s
names(colon_s)
expa <- c("age","age.factor","sex.factor","obstruct.factor")
dependent <-  "perfor.factor"



colon_s %>% 
  summary_factorlist(dependent = dependent, explanatory = expa)
colnames(colon_s)




