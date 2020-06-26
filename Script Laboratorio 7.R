 ###########################
###                     ###
###   Laboratorio 07    ###
###                     ###
###########################

# Teorema del l?mite central #

# Ejemplo 1 #
# X_1,...,X_n ~ Exponencial(nu)

set.seed(1113)
y.10=vector("numeric")
y.50=vector("numeric")
y.100=vector("numeric")
y.500=vector("numeric")

rep=10000
nu=5
for(i in 1:rep){
  x=rexp(n=10,rate=nu)
  y.10[i]=sum(x)
  x=rexp(n=50,rate=nu)
  y.50[i]=sum(x)
  x=rexp(n=100,rate=nu)
  y.100[i]=sum(x)
  x=rexp(n=500,rate=nu)
  y.500[i]=sum(x)
  
}


par(mfrow=c(2,2),cex=0.7)
n=10
hist(y.10,freq=F,main=expression(n==10),
     col="darkblue")
curve(dgamma(x,shape=n,rate=nu),lwd=3,
      col="orange",lty=2,add=TRUE)
curve(dnorm(x,mean=n/nu,sd=sqrt(n)/nu),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=50
hist(y.50,freq=F,main=expression(n==50),
     col="darkblue")
curve(dgamma(x,shape=n,rate=nu),lwd=3,
      col="orange",lty=2,add=TRUE)
curve(dnorm(x,mean=n/nu,sd=sqrt(n)/nu),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=100
hist(y.100,freq=F,main=expression(n==100),
     col="darkblue")
curve(dgamma(x,shape=n,rate=nu),lwd=3,
      col="orange",lty=2,add=TRUE)
curve(dnorm(x,mean=n/nu,sd=sqrt(n)/nu),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=500
hist(y.500,freq=F,main=expression(n==500),
     col="darkblue")
curve(dgamma(x,shape=n,rate=nu),lwd=3,
      col="orange",lty=2,add=TRUE)
curve(dnorm(x,mean=n/nu,sd=sqrt(n)/nu),lwd=3,
      col="magenta",lty=2,add=TRUE)


# Ejemplo 1 #
# X_1,...,X_n ~ Poisson(lambda)

set.seed(1113)
y.10=vector("numeric")
y.50=vector("numeric")
y.100=vector("numeric")
y.500=vector("numeric")

rep=10000
lambda=10
for(i in 1:rep){
  x=rpois(n=10,lambda = lambda)
  y.10[i]=sum(x)
  x=rpois(n=50,lambda = lambda)
  y.50[i]=sum(x)
  x=rpois(n=100,lambda = lambda)
  y.100[i]=sum(x)
  x=rpois(n=500,lambda = lambda)
  y.500[i]=sum(x)
}


par(mfrow=c(2,2),cex=0.7)
n=10
hist(y.10,freq=F,main=expression(n==10),
     col="darkblue")
curve(dpois(x,lambda=n*lambda),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*lambda,sd=sqrt(n*lambda)),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=50
hist(y.50,freq=F,main=expression(n==50),
     col="darkblue")
curve(dpois(x,lambda=n*lambda),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*lambda,sd=sqrt(n*lambda)),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=100
hist(y.100,freq=F,main=expression(n==100),
     col="darkblue")
curve(dpois(x,lambda=n*lambda),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*lambda,sd=sqrt(n*lambda)),lwd=3,
      col="magenta",lty=2,add=TRUE)

n=500
hist(y.500,freq=F,main=expression(n==500),
     col="darkblue")
curve(dpois(x,lambda=n*lambda),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*lambda,sd=sqrt(n*lambda)),lwd=3,
      col="magenta",lty=2,add=TRUE)


# Estad?sticos de orden # 
# X_1,...,X_n ~ Exponencial(nu)
# Y_1=X_(1)=min(X_1,...,X_n)~Exponencial(n*nu)

set.seed(1113)
n=100
nu=3
y=vector("numeric")

rep=10000
for(i in 1:rep){
  x=rexp(n=100,rate=nu)
  y[i]=min(x)
}

par(mfrow=c(1,1))
hist(y,freq=FALSE,col="darkblue")
curve(dexp(x,rate=n*nu),col="orange",lty=2,lwd=3,add=TRUE)



## ACTIVIDAD hecha por mi
'[{{{'
y.10 = vector("numeric")
y.100 = vector("numeric")
y.1000 = vector("numeric")

size =30
prob = 0.3
p= 0.3
n=10

rate = 1000
for (i in 1:rate){
      x = rbinom(10,size= size, prob= prob)
      y.10[i]=sum(x)
    
}


hist(y.10,freq=F,main=expression(n==10),
     col="darkblue")
curve(dnorm(x, mean=n*p*N, sd = sqrt(n*p*(1-p)*N)), col="red", add=TRUE)


hist(y.10,freq=F,main=expression(n==10),
     col="darkblue")
#BIN(n=30,p=0.3) = sumaron 30 BIN(n=1,p=0.3)
curve(dbinom(x,size = n*size,prob = prob),lwd=3,
      col="orange",lty=1,add=TRUE)
curve(dnorm(x,mean=n*size*prob,sd=sqrt(n*size*prob*(1-prob))),lwd=3,
      col="magenta",lty=2,add=TRUE)


5+6


