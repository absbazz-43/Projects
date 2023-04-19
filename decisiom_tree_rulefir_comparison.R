library(randomForest)
#devtools::install_github('araastat/reprtree')

library(reprtree)


model <- randomForest(Species ~ ., data=iris, importance=TRUE, ntree=500, mtry = 2, do.trace=100)

reprtree:::plot.getTree(model)
plot(model)

reprtree::ReprTree(model)
?ReprTree
reptree <- ReprTree(mod, iris, metric='d2')
reptree
plot(reptree)
varImpPlot(model)

mod <- rpart::rpart(Species ~., data = iris)
reprtree::plot.getTree(mod)
fg <- predict(model, iris[,-5])
fg
table(fg, iris$Species)

tr <- predict(mod, iris[,-5], type="class")
tr
table(tr, iris$Species)



#########       Rule fir model 

library("pre")
data("carrillo", package = "pre")

dim(carrillo)
head(carrillo)
mno <- pre(Species ~., data = iris)
mno
hj <- predict(mno, iris[,-5],"class")
table(hj, iris$Species)
coef(mno)
importance(mno, standardize = TRUE, round = 4L)
carrillo.ens <- pre(bdi ~ ., data = carrillo)
carrillo.ens
plot(mno,nterms = 6, plot.dim = c(2, 3), standardize = TRUE,
     cex = 0.7)

library(tree)
plot.reptree(mod)

coef(carrillo.ens)
imps <- importance(carrillo.ens, standardize = TRUE, round = 4L)
plot(carrillo.ens, nterms = 6, plot.dim = c(2, 3), standardize = TRUE,
      cex = 0.7)
j <- predict(mno, )

singleplot(carrillo.ens, varname = "ntot")
cv.carrillo <- cvpre(carrillo.ens)
cv.carrillo


##########  cubist model

#install.packages("Cubist")
library(Cubist)
cub <- cubist( mtcars[,!names(mtcars) %in% "mpg"],mtcars$mpg)
cub
plot(cub)

summary(cub)
