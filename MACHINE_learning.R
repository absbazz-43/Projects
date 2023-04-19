library(tidyverse)
library(caret)
library(MASS)
library(caTools)
library(nnet)

## Data 

t = sample.split(iris$Species, SplitRatio = .75)
train_data = iris[t==TRUE,]
train_data = train_data %>% relocate(Species,.before = Sepal.Length)
test_data = iris[t==FALSE,]
test_data = test_data %>% relocate(Species,.before = Sepal.Length)
#######  For categorical variable and classification p[roblem

## multinomial Logistic Regression
mod_log = multinom( Species~., data = train_data,family = binomial(link = "logit"))
pred_log = predict(mod_log, test_data[,-1],type = "class")

log_accuracy = mean(test_data[,1]!=pred_log) 

################random forest 

fg <- train(Species ~., data = train_data, method = "xgbTree")  
hj <- predict(fg, test_data[,-1])  
mean(hj != test_data[,1])



##################


dim(table(mtcars$am))



###  function 

ML_model <- function(train, test, response, type = "categorical"){
  
    ## Checking factor 
  if( type== "categorical"){
    if(dim(table(response)==2)){
      
      ### Logistics regression
      
     logit <- glm(train[,1] ~ ., data = train) 
     pred_log <- predict(logit, test[,-1])
     recat <- ifelse(pred_log>.5,1,0)
     log_accuracy <- mean(recat == test[,1])
     
     ###  LDA 
     
     LDA <- lda(train[,1] ~ ., data = train)
     pred_Lda <- predict(LDA, test[,-1], type ="class")
     Lda_accuracy <- mean(pred_Lda == test[,1])
     
     ### QDA
     
     QDA <- qda(train[,1] ~ ., data = train)
     pred_Qda <- predict(QDA, test[,-1], type ="class")
     Qda_accuracy <- mean(pred_Qda == test[,1])
     #
     
     
    }
  }
  

}





glm(Species ~ ., data = train_data, family = binomial(link ="logit"))
