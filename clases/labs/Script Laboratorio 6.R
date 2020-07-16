###########################
###                     ###
###   Laboratorio 06    ###
###                     ###
###########################

# 1 #

y=rnorm(200,mean=10,sd=1.5)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
p=c(0.01,0.1,1,2,5,10,20,30,50,50,60,70,80,90,95,99,99.9,99.99)/100
?qnorm
plot(qnorm(x),y,xaxt="n",ylab="Valores de Y",xlab="Probabilidad Acumulada",
     bty="n",lwd=2,las=1)
axis(1,at=qnorm(p),label=p,las=1)
abline(lm(y~qnorm(x)),lwd=2,col="blue")
modelo=lm(y~qnorm(x))
modelo$coefficients
fit=modelo$coefficients;fit
hat.mu=fit[1]
hat.sigma=fit[2]
hat.mu;hat.sigma
hist(y,freq=FALSE,col="darkred",border="white",main="",las=1)
curve(dnorm(x,mean=hat.mu,sd=hat.sigma),add=TRUE,lwd=2,lty=2,col="blue")


# 2 # 
lambda=1
zeta=0.2
y=rlnorm(200,meanlog=lambda,sdlog=zeta)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
p
plot(qnorm(x),log(y),xaxt="n",ylab="Valores de Y",xlab="Probabilidad Acumulada",
     bty="n",lwd=2,las=1)
axis(1,at=qnorm(p),label=p,las=1)
abline(lm(log(y)~qnorm(x)),lwd=2,col="blue")
modelo=lm(log(y)~qnorm(x))
modelo$coefficients
fit=modelo$coefficients;fit
hat.lambda=fit[1]
hat.zeta=fit[2]
hat.lambda;hat.zeta
hist(y,freq=FALSE,col="darkred",border="white",main="",
     las=1)
curve(dlnorm(x,meanlog=hat.lambda,sdlog = hat.zeta),add=TRUE,col="blue",
      lwd=2,lty=2)

# 3 # 
k=10
nu=0.1
y=rgamma(200,rate=nu,shape=k)
hist(y,freq=FALSE,col="darkred",border="white",main="",las=1)


# qqplot Normal #
par(mfrow=c(1,2),cex=0.8)
y=sort(y)
N=length(y)
m=1:N
x=m/(N+1)
plot(qnorm(x),y,xaxt="n",xlab="Probabilidad Acumulada",
     ylab="Valores de Y",bty="n",lwd=2,las=1,main="Distribución Normal")
axis(1,at=qnorm(p),label=p,las=1)
abline(lm(y~qnorm(x)),lwd=2,col="blue")
modelo=lm(y~qnorm(x))
fit=modelo$coefficients
hat.mu=fit[1]
hat.sigma=fit[2]

# qqplot Log-Normal #
plot(qnorm(x),log(y),xaxt="n",xlab="Probabilidad Acumulada",
     ylab="Valores de Y",bty="n",lwd=2,las=1,main="Distribución Log-Normal")
axis(1,at=qnorm(p),label=p,las=1)
abline(lm(log(y)~qnorm(x)),lwd=2,col="blue")
modelo=lm(log(y)~qnorm(x))
fit=modelo$coefficients
hat.lambda=fit[1]
hat.zeta=fit[2]

# Histograma #
par(mfrow=c(1,1),cex=0.8)
hist(y,freq=FALSE,col="darkred",border="white",main="",las=1)
curve(dnorm(x,mean=hat.mu,sd=hat.sigma),
      lwd=2,col="darkblue",add=TRUE)
curve(dlnorm(x,meanlog=hat.lambda,sdlog = hat.zeta),
      lwd=2,col="darkgreen",add=TRUE)
legend("topright",col=c("darkblue","darkgreen"),
       lwd=2,c("Normal","Log-Normal"),cex=2,bty="n")


### Weibull ###
set.seed(10052020)
n=200
beta=0.8
eta=0.2
X=rweibull(n,shape=beta,scale=eta)
QQ.Weibull = function(y){
  x=sort(y)
  N=length(y)
  p=(1:N)/(N+1)
  plot(log(x)~log(-log(1-p)),
       pch = 20, col = "darkblue", bty = "n", las = 1,
       main = expression("QQ-Weibull"),
       ylab = expression(log(x[p])),
       xlab = expression(log(-log(1-p))))
  abline(lm(log(x) ~ log(-log(1-p))), lwd = 3, col = "darkorange")
  aux = lm(log(x) ~ log(-log(1-p)))
  aux
}
hat.eta = as.numeric(exp(QQ.Weibull(X)$coef[1]))
hat.beta = as.numeric(1/QQ.Weibull(X)$coef[2])


hist(X,freq=FALSE,nclass=30,col="darkred",border="white",main="",las=1)
curve(dweibull(x,shape=hat.beta,scale=hat.eta),lwd=3,col="darkblue",
      add=TRUE)
