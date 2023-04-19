dat   <- read.csv(file.choose())
team  <- dat[,1]
Y2012 <- dat[,2]
Y2013 <- dat[,3]
Ytot  <- Y2012+Y2013
dat

team  <- team[order(Ytot)] # order the teams by total concussions
Y2012 <- Y2012[order(Ytot)]
Y2013 <- Y2013[order(Ytot)]
Ytot  <- Ytot[order(Ytot)]

N     <- 512
a     <- 0.01 
b     <- 0.01

a2012 <- sum(Y2012)+a
b2012 <- N+b




library(tidyverse)
iris %>% 
  gather(key = key, value = value, c("Sepal.Length","Sepal.Width"))


a2013 <- sum(Y2013)+a
b2013 <- N+b

grid  <- seq(0.2,0.45,.001)

p2012 <- dgamma(grid,a2012,b2012)
p2013 <- dgamma(grid,a2013,b2013)

plot(grid,p2013,type="l",xlab="Concussion rate (per game)",ylab="Posterior density")
lines(grid,p2012,lty=2)

legend("topright",c("2012","2013"),lty=2:1,inset=0.05)


a    <- Ytot  + .01
b    <- 32 + .01
low  <- qgamma(0.05,a,b)
high <- qgamma(0.95,a,b)

par(mar=c(5, 4, 4, 2))
plot(NA,xlim=c(0,1),ylim=c(1,32),axes=FALSE,
     ylab="",xlab="Posterior 90% interval",
     main="Concusions per game in the NFL in 2012-2013")

for(j in 1:32){
  lines(c(low[j],high[j]),c(j,j))
  lines(c(low[j],low[j]),c(j-.2,j+.2))
  lines(c(high[j],high[j]),c(j-.2,j+.2))
}
axis(1)
axis(2,at=1:32,labels=team,las=2,cex.axis=.75)
?par(mar)

































#### 
rbinom(100,10,.5)
?dbinom
den = c()
men = c()
for( i in 1:1000){
  ran = rbinom(2,1,.65)
  rz = rbinom(2,1,.75)
  brz = ran
  arz = rz
  den[i] = 37/75*sum(dbinom(brz,2,.5))
  men[i] = (38/75)*sum(dbinom(arz,2,.5))
}

men
az =mean(den)
bz = mean(men)

round(az/sum(az,bz),2)
round(bz/sum(az,bz),2)


############## my suggested model 

v = rbinom(10,1,.5)
v
pv = sum(v[v==1])/length(v)
pv

p1 = (.7)

prob_model = function (simulation, ma_be_AB, Aw, Bw,ML4A, ML4B,Aw4, Bw4){
  A = c()
  B = c()
  pA = Aw4/ML4A
  pB = Bw4/ML4B
  poA = Aw/ ma_be_AB
  poB = Bw / ma_be_AB
  for( i in 1:simulation){
    rA = rbinom(simulation,1,pA)
    rB = rbinom(simulation,1,pB)
    wA = sum(rA[rA==1])/length(rA)
    wB = sum(rB[rB==1])/length(rB)
    A[i] = poA * wA 
    B[i] = poB * wB
  }
  meA = mean(A)
  meB = mean(B)
  cat("Win probablity of team A", round(meA/sum(meA,meB)*100,2),"%","\n")
  cat("Win probablity of team B", round(meB/sum(meA,meB)*100,2),"%","\n")
}


prob_model(simulation = 10000, ma_be_AB = 75, Aw = 30, Bw = 45, ML4A = 36, ML4B = 30, Aw4 = 30, Bw4 = 28)
















mo <- iris[,4:5] %>% 
  group_by(Species) %>% 
  do(model=auto.arima(ts(Petal.Width)))

d <- mtcars %>% 
  group_by(am) %>% 
  do(model = lm(mpg ~.,.)) %>% 
  predict(model, mtcars[13:15,-1])
predict(d[0,], mtcars[,-1])
d[0,]




library(forecast)

a <- ts(rnorm(100))
model =auto.arima(a)
forecast(model,10)


data <- read.

da = iris[,4:5]
FG <- da %>% 
  group_by(Species) %>% 
  mutate(hj = ts(Petal.Width)) %>% 
  mutate(m = map(hj,auto.arima)) %>% 
  summarise(fort = map(m, forecast,h = 1))
FG$fort
#Install the required package
#Load the required library 
library(googlesheets4)
data= read_sheet("https://docs.google.com/spreadsheets/d/1pMzgsLtYSVbvZIKWiTDNTk7zzTuCVG8r/edit#gid=982982405")
data
View(data)



