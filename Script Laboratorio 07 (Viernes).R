#### Actividad Laboratorio 7

set.seed(1113)
y.10=vector("numeric")
y.100=vector("numeric")
y.1000=vector("numeric")
y.10000=vector("numeric")

rep = 10000
size = 30
prob = 0.3

for(i in 1:rep){
  x=rbinom(n=10,size = size,prob = prob)
  y.10[i]=sum(x)
  x=rbinom(n=100,size = size,prob = prob)
  y.100[i]=sum(x)
  x=rbinom(n=1000,size = size,prob = prob)
  y.1000[i]=sum(x)
  x=rbinom(n=10000,size = size,prob = prob)
  y.10000[i]=sum(x)
}


#par(mfrow=c(2,2),cex=0.7)
n=10
hist(y.10,freq=F,main=expression(n==10),
     col="darkblue")

#BIN(n=30,p=0.3) = sumaron 30 BIN(n=1,p=0.3)
curve(dbinom(x,size = n*size,prob = prob),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*size*prob,sd=sqrt(n*size*prob*(1-prob))),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=100
hist(y.100,freq=F,main=expression(n==100),
     col="darkblue")
curve(dbinom(x,size = n*size,prob = prob),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*size*prob,sd=sqrt(n*size*prob*(1-prob))),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=1000
hist(y.1000,freq=F,main=expression(n==1000),
     col="darkblue")
curve(dbinom(x,size = n*size,prob = prob),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*size*prob,sd=sqrt(n*size*prob*(1-prob))),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=10000
hist(y.10000,freq=F,main=expression(n==10000),
     col="darkblue")
curve(dbinom(x,size = n*size,prob = prob),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*size*prob,sd=sqrt(n*size*prob*(1-prob))),lwd=3,
      col="magenta",lty=2,add=TRUE)
########



# X_1,...,X_n ~ Uniforme(a,b)
# f(x) = 1/(b-a)
# F(x) = (x-a)/(b-a)
# Y_1=X_(1)=min(X_1,...,X_n)=n*(1-(b-y)/(b-a))^{n-1}(1/(b-a))
# Y_n = max(X_1,...,X_n)=n/(b-a)*((x-a)/(b-a))^(n-1)

a=-5
b=5
n=100
y1=vector("numeric")
yn=vector("numeric")

rep = 1000
for(i in 1:rep){
  x=runif(n=100,min = a, max = b)
  y1[i]=min(x)
  yn[i]=max(x)
}

#par(mfrow=c(1,2))
hist(y1,freq=FALSE,col="darkblue")
eq = function(x){n/(b-a)*((b-x)/(b-a))^(n-1)}
curve(eq,from = a,to=b,col="orange",lty=2,lwd=3,add=TRUE)

hist(yn,freq=FALSE,col="darkblue")
eq = function(x){n/(b-a)*((x-a)/(b-a))^(n-1)}
curve(eq,from = a,to=b,col="orange",lty=2,lwd=3,add=TRUE)
