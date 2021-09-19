d <- density(rexp(500000,1))
plot(d, main = "Exp(1)", xlab = "Soporte", ylab = "Densidad", ylim = c(0,1), xlim = c(0,10))
polygon(d, col="lightblue2", border="blue")
points(0:10/10 ~ rep(1,11), type = "l", col = "red")

rm(d)

d <- density(runif(1000000,0,2))
plot(d, main = "Unif(0,2)", xlab = "Soporte", ylab = "Densidad",  ylim = c(0,1), xlim = c(0,10))
polygon(d, col="lightblue2", border="blue")
points(0:10/10 ~ rep(1,11), type = "l", col = "red")

rm(d)

G=function(x,n){if(x<(1/n)){n}else{0}}

n = 1
X = 0:200/100
Y = sapply(X, function(x){G(x,n)})
plot(Y ~ X, type = "l", ylim = c(0,10), main = "Lim_n G_n(x)")
points(Y ~ X, type = "h", ylim = c(0,10), col = "skyblue")
positivos = which(Y>0)
points(Y[positivos] ~ X[positivos], type = "l")

n = 2
X = 0:300/100
Y = sapply(X, function(x){G(x,n)})
points(Y ~ X, type = "l", ylim = c(0,10), col = "skyblue1")
points(Y ~ X, type = "h", ylim = c(0,10), col = "skyblue1")
positivos = which(Y>0)
points(Y[positivos] ~ X[positivos], type = "l")

n = 4
X = 0:300/100
Y = sapply(X, function(x){G(x,n)})
points(Y ~ X, type = "l", ylim = c(0,10), col = "skyblue3")
points(Y ~ X, type = "h", ylim = c(0,10), col = "skyblue3")
positivos = which(Y>0)
points(Y[positivos] ~ X[positivos], type = "l")

n = 7
X = 0:300/100
Y = sapply(X, function(x){G(x,n)})
points(Y ~ X, type = "l", ylim = c(0,10), col = "skyblue4")
points(Y ~ X, type = "h", ylim = c(0,10), col = "skyblue4")
positivos = which(Y>0)
points(Y[positivos] ~ X[positivos], type = "l")

n = 10
X = 0:300/100
Y = sapply(X, function(x){G(x,n)})
points(Y ~ X, type = "l", ylim = c(0,10), col = "slateblue4")
points(Y ~ X, type = "h", ylim = c(0,10), col = "slateblue4")
positivos = which(Y>0)
points(Y[positivos] ~ X[positivos], type = "l")


