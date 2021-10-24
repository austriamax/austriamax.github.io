
n = 3
Fs= function(x){
  f = (floor((2^n)*(x))+1)/(2^n)
  if(x<0){f = 0}
  if(x>1){f = 1}
  return(f)
}

x = runif(1000,-.1,1.1)
x = x[order(x)]

plot(punif(x) ~x, main = "Fn vs F", ylim = c(0,1.1), type = "l", col = "blue",
     xlab = "x", ylab = "Prob(X <= x)")
for (i in 0:(2^n+1)) {
  z = x[intersect(which(x<(i/2^n)),which(x>((i-1)/2^n)))]
  points(sapply(z, Fs)~z, col = "red", type = "l")
}