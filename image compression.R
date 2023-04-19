library(jpeg)
tiger <- readJPEG(file.choose())

imager::

r <- tiger[,,1]
g <- tiger[,,2]
b <- tiger[,,3]
tiger_r <- prcomp(r, center = FALSE)
tiger_g <- prcomp(g, center = FALSE)
tiger_b <- prcomp(b, center = FALSE)

rgb <- list(tiger_r, tiger_g, tiger_b)
pca.img <- sapply(rgb, function(j) {
  compressed.img <- j$x[,1:90] %*% t(j$rotation[,1:90])
}, simplify = 'array')


writeJPEG(pca.img, paste0('Compressed_image_with_',10, '_components.jpg'))



dim(bn$rotation)

dim(bn$x)
################

pcaimg <- sapply(rgb, function(j) {
  compressed.img <- j$x[,1:90] %*% t(j$rotation[,1:90])
}, simplify = 'array')



writeJPEG(pcaimg, "pca.jpg")
################

getwd()
plot(tiger)
image(tiger)
magick::image_read(file.choose())

pca.img

for( i in seq.int(c(10,20))){
  print(i)
}





library(raster)
im <- raster(file.choose())
im
image(im)
img.flip<-flip(im, direction = "y")
rasterimg<-t(as.matrix(img.flip))
dim(rasterimg)
svdimg<-svd(rasterimg)

U<-svdimg$u
d<-diag(svdimg$d)
V<-svdimg$v
U1 <- as.matrix(U[, 1:118])
d1 <- as.matrix(d[1:118, 1:118])
V1 <- as.matrix(V[, 1:118])






BiocManager::install("EBImage")

if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

library(EBImage)
install.packages("EBImage")

r = channel(orig,"r")
g = channel(orig,"g")
b = channel(orig,"b")

gray1 = 0.21*r+0.71*g+0.07*b

display(gray1) 
image(gray1)
img1 <- U1 %*% d1 %*% t(V1)
image(img1, col = grey(seq(0, 1, length = 256)))






#### Indepedndent compondent analysis 


library(jpeg)
im <- readJPEG(file.choose())
r <- im[,,1]
g <- im[,,2]
b <- im[,,3]

library(fastICA)
ric <- fastICA(r,n =20)
gic <- fastICA(g,n = 20)
bic <- fastICA(b,n =20)

ic <- list(ric,gic,bic)

imt <- sapply(ic, function(i){
  m <- i$S %*% i$A
}, simplify = "array")

writeJPEG(imt, "tiger.jpg")
getwd()



r <- im[,,1]
g <- im[,,2]
b <- im[,,3]

library(fastICA)
ric <- fastICA(r,n =20)
gic <- fastICA(g,n = 20)
bic <- fastICA(b,n =20)

ic <- list(ric,gic,bic)

imt <- sapply(ic, function(i){
  m <- i$S %*% i$A
}, simplify = "array")

writeJPEG(imt, "tiger.jpg")


library(fastICA)
ric <- fastICA(r,n =20)
gic <- fastICA(g,n = 20)
bic <- fastICA(b,n =20)

ic <- list(ric,gic,bic)

imt <- sapply(ic, function(i){
  m <- i$S[1:600,] %*% i$A[,1:600]
}, simplify = "array")

writeJPEG(imt, "tiger.jpg")

###### Kmeans
library(class)
rica <- kmeans(r,centers =500)
gica <-  kmeans(g,centers = 500)

bica <-  kmeans(b,centers =500)




ick <- list(rica,gica,bica)



imt <- sapply(ick, function(i){
  m <- i$centers[i$cluster,]
}, simplify = "array")

writeJPEG(imt, "tigerkk.jpg")

points(rica$centers,col="red")
points(rica$centers[rica$cluster,],col="green")



as = mtcars$mpg
k = kmeans(as, centers = 2)
k$cluster
k$centers
plot(as)
points(k$centers, col = 'red')
k$centers[k$cluster,]



