
#####  Model slection 

library(leaps)
# best subset

model  = regsubsets(mpg~., data = mtcars)
model
d=summary(model)
which.max(d$rsq)
d$rsq

all = lm(mpg ~1 ,data = mtcars)
av  = lm(mpg ~., data = mtcars)
model_step = step(all, direction = "forward", scope = formula(av))
model_step$anova

all <- lm(mpg ~ ., data=mtcars)

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)

#view results of backward stepwise regression
backward$anova
backward$coefficients



intercept = lm(mpg ~1, data = mtcars)
full = lm(mpg ~., data = mtcars)
both = step(intercept, direction = 'both', scope = formula(all), steps = 1000)
both$anova
both$coefficients

?step







intercept_only <- lm(mpg ~ 1, data=mtcars)

#define model with all predictors
all <- lm(mpg ~ ., data=mtcars)

#perform backward stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of backward stepwise regression
both$anova



# K fold cross validation 

library(caret)
model  = train(
  as.factor(am) ~ drat , mtcars, method = "glm",family = "binomial",
trControl=  trainControl(method = "LOOCV", number = 5)
)
model


## Lda vs KNN

library(MASS)

mod = qda(iris$Sepal.Length ~ iris$Sepal.Width, data = iris[,-5])
mod
summary(mod)


library(caTools)

train = sample.split(iris$Species, SplitRatio = .75)
train

trin = iris[train ==TRUE,]
tst = iris[train==FALSE,][,-5]
x = trin[,-5]
y = trin[,5]
library(class)
knnMo = knn(x,tst,y, k =1)
knnMo
yt = iris[train==FALSE,][,5]
table(yt, knnMo)
mean(knnMo !=yt)


lid = qda(Species ~ . , trin)
pr = predict(lid,tst)
table(pr$class, yt)

mean(pr$class != yt)


















ggplot(iris, aes(Sepal.Length, Petal.Length))+ geom_point()+geom_label(label = iris$Species)


require(utils)

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))

expand.grid(a= c("m","f"),b = c(2,3))

grid.e


####################################################################
#
# Example of joint, marginal, and conditional distributions (sample)
# Using the Obama voting data.
#
####################################################################


#Load data

dat      <- read.csv("http://www4.stat.ncsu.edu/~reich/ST590/assignments/Obama2012.csv")
pctObama <- 100*dat[,2]
pctUnEmp <- dat[,18]

############################################
# Convert to discrete variables
############################################

X        <- ifelse(pctObama>50,1,0)
Y        <- ifelse(pctUnEmp>10,1,0)+
  ifelse(pctUnEmp>15,1,0)
plot(Y,pctUnEmp)

# Compute the sample joint distribution

table(X,Y)/100
# Compute the sample marginal distributions

table(X)/100
table(Y)/100

# Compute the conditional probabilities

mean(X[Y==0])
mean(X[Y==1])
mean(X[Y==2])



############################################
# Plot for continuous variables
############################################

X <- pctObama
Y <- pctUnEmp

#Joint

plot(X,Y,main="Joint distribution")

# Marginals

hist(X,main="Marginal distribution of X")
hist(Y,main="Marginal distribution of Y")

# Probability in a set

inA <- (X>50) & (Y>10) & (Y<15)  
plot(X,Y,col=ifelse(inA,2,1),main="Prob in set A")
polygon(c(50,50,100,100,50),c(10,15,15,10,10))

# Approximate conditional pdf

Y10 <- Y>9.5 & Y<10.5
plot(X,Y,col=ifelse(Y10,2,1))
abline(9.5,0)
abline(10.5,0)

X10 <- X[Y10]
hist(X10,main="f(x|Y=10)",xlim=range(X),prob=TRUE)







# Bayesian posterior
 ## Beta binomial mode
n = 20
Y = 4
a = 3
b =1

grid = seq(0,1,.1)

prior = dbeta(grid, a,b)
prior = prior/ sum(prior) # standarized

like = dbinom(4, 20, grid)
like = like/sum(like)

post = like*prior
post = post/sum(post)

plot(grid, like)

# Exact calculation 

post = dbeta(grid, (Y+a), (n+b-Y) )
post
plot(grid, post)

man = (Y +a)/(n- Y +b)

va = (Y + a)/(n - Y+b)^2
 
qbeta(c(.05,.95),Y+a, n-Y+b)



### Gamma poisson

N = 20
y =11
a = .5
b = .5
l = seq(.1,2,.01)
like = dpois(y, N*l)
like = like/sum(like)

prior = dgamma(l, .5,.5)
prior = prior/sum(prior)

post = like * prior
post = post / sum(post)

plot(l, post)

li = ( y + a)/(N + b)
sq = (y + a)/ (N+b)^2
sqrt(sq)


## Normal normal]

y = .1
sigma = .005

m = .05
s = .025
gri = seq(0,.15, .001)
like = dnorm(y, gri, sigma)
like = like /sum(like)

pr = dnorm(gri, m, s)
pr =  post/sum(post)

post = pr * like
post = post/sum(post)
plot(gri, post)

su  = 1/sigma^2 + 1/s^2
ms = (y/sigma^2 + m/s^2 )
mu = ms/su
mu

a <- "man in the east"

?str_c


library(stringr)
mtcars %>% 
  select(str_extract(colnames(mtcars),"am"))

str_extract(colnames(mtcars),"am")
  
str_match(colnames(mtcars), "am")
colnames(mtcars)

str_detect()
a <- mtcars %>% 
  select(matches(c("am","vs")))
a


iris %>% 
  select(matches("Sepal"))


iris %>% 
  select(starts_with("Sepal",ignore.case = TRUE))
