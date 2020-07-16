
##################
# Laboratorio 09 #
##################

# Ejemplo 1 #
f <- function(x,a) (x-a)**2

xmin <- optimize(f=f, interval=c(0,1), tol=0.0001,
                 a=1/3)
xmin

xmax <- optimize(f=f, interval=c(0,1), tol=0.0001, 
                 a=1/3, maximum=TRUE)
xmax

a=1/3
curve(f(x,a),c(0,1))
abline(v=xmin$minimum,col="red",lty=2,lwd=2)
abline(v=xmax$maximum,col="blue",lty=2,lwd=2)


# Ejemplo 2 #
fr <- function(x){ 
x1 <- x[1]; x2 <- x[2]
100 * (x2 - x1 * x1)**2 + (1 - x1)**2
}
optim(c(-1.2,1), fr)


# Ejemplo 3 #
library(MASS)
x2 <- rnorm(250, mean=10,sd=4)
fitdistr(x=x2, densfun="normal")


# Ejercicio #
lambda=1.2
zeta=0.3
Y=rlnorm(n=500,meanlog=lambda,sdlog=zeta)
hist(Y)

# Estimaci�n QQ
x=sort(Y)
N=length(x)
p=1:N/(N+1)
fit=lm(log(x)~qnorm(p))
lambda.qq=fit$coef[1]
zeta.qq=fit$coef[2]

# Estimaci�n de momentos
zeta.em=sqrt(log(mean(Y^2))-2*log(mean(Y)))
lambda.em=log(mean(Y))-0.5*zeta.em^2

# Estimaci�n de m�xima verosimilitud
fit=fitdistr(Y,"lognormal")
fit
lambda.emv=fit$estimate[1]
zeta.emv=fit$estimate[2]

hist(Y,freq=F,nclass=5,las=1,
     ylim=c(0,0.5),col="gray",border="white",main="")
curve(dlnorm(x,meanlog=lambda.qq,sdlog=zeta.qq),
      col="darkred",lwd=3,lty=2,add=TRUE)
curve(dlnorm(x,meanlog=lambda.em,sdlog=zeta.em),
      col="darkorange",lwd=3,lty=2,add=TRUE)
curve(dlnorm(x,meanlog=lambda.emv,sdlog=zeta.emv),
      col="darkblue",lwd=3,lty=2,add=TRUE)
lambda.qq;zeta.qq
lambda.em;zeta.em
lambda.emv;zeta.emv
legend("topright",c("QQ","EM","EMV"),lwd=3,
       col=c("darkred","darkorange","darkblue"),
       lty=2,bty="n",cex=1)


## actividad hecha por mi 
library(foreign)
data = read.table("tiempos.txt")
a = attach(data)
names(data) = NULL
data = data[,c(1)]
Y = data
n = length( Y)
#momentos
momento_nu = (mean(Y)/((sum(Y^2)/n-(mean(Y))^{2})))
momento_nu
momento_k = ((mean(Y))^2 /(mean(Y^2)-(mean(Y))^2))


hist(data, freq=F, nclass=15)
curve(dgamma(x, shape=momento_k, rate=momento_nu), col='blue',add=T)

[]
library(MASS)
fit = fitdistr(Y, 'gamma')

curve(dgamma(x,shape =fit$estimate[1], rate = fit$estimate[2]), col = 'red',add = T)



