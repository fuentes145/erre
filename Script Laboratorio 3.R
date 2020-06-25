#####################
### Laboratorio 3 ###
#####################

### Distribuciones ###

# Exponencial #
# nu=3
# f_x(1)
dexp(1,rate=3)
# P(X<=1.5)=F_x(1.5)
pexp(1.5,rate=3)
# P(X<=k)=0.5, k=?
qexp(0.5,rate=3)
# Generar una muestra de tama?o n=1000
rexp(1000,rate=3)
muestraexp=rexp(1000,rate=3)
hist(muestraexp,freq=FALSE)

# Uniforme #
# min=50, max=100
# f_x(60)
dunif(x=60,min=50,max=100)
# P(X<=60)=F_x(60)
punif(q=60,min=50,max=100)
# P(X<k)=0.2, k=?
qunif(p=0.2,min=50,max=100)
# Muestra de tama?o 100
runif(n=100,min=50,max=100)
muestraunif=runif(n=100,min=50,max=100)
hist(muestraunif,freq=FALSE)

# set.seed() para establecer una semilla y que las simulaciones no varien


# Normal #
# mu=100, sd=20
mu=12
sigma=7.627758
# f_x(80)
dnorm(x=8,mean=mu,sd=sigma)
# P(X<80)=F_x(80)
1-pnorm(q=24,mean=mu,sd=sigma)
# P(X<k)=0.1586553, k=?
qnorm(p=0.3,mean=mu,sd=sigma)
# muestra de tama?o n
n=10000
muestranormal=rnorm(n=n,mean=mu,sd=sigma)
hist(muestranormal,freq=FALSE)


# Log-Normal #
# lambda=1, zeta=2
delta = 0.33
lambda= log(5) - (1/2)* delta^2
zeta=sqrt(log(1+delta^2))
plnorm(8, meanlog = lambda, sdlog = zeta)

# f_x(80)
dlnorm(x=80,meanlog=lambda,sdlog=zeta)
# P(X<=80)=F_x(80)
a
# P(X<k)=0.9545829
qlnorm(p=0.9545829,meanlog=lambda,sdlog=zeta)
# muestra tama?o n
n=10000
muestralognormal=rlnorm(n=n,meanlog=lambda,sdlog=zeta)
hist(muestralognormal,freq=FALSE)


# Gamma #
# k=3, nu=4
k=6
nu=0.15
# f_x(10)
1-dgamma(x=30,shape=k,rate=nu)
# P(X<=10)=F_x(10)
p = 1-pgamma(q=30,shape=k,rate=nu)
p^3
# P(X<=k)=0.9999995
qgamma(p=0.9999995,shape=k,rate=nu)
# muestra tama?o n
n=10000
muestragamma=rgamma(n=n,shape=k,rate=nu)
hist(muestragamma,freq=FALSE)

# Chi cuadrado #
# n=10
n2=10
# f_x(5)
dchisq(x=5,df=n2)
# P(X<=5)=F_x(5)
pchisq(q=5,df=n2)
# P(X<=k)=0.108822, k=?
qchisq(p=0.108822,df=n2)
# muestra tama?o n
n=10000
muestrachisq=rchisq(n=n,df=n2)
hist(muestrachisq,freq = FALSE)


# Gr?ficos con curve #
curve(dnorm(x,mean=6,sd=6),from=-6,to=16)
curve(pnorm(x,mean=6,sd=6),from=-6,to=16)


# Integrales #
integrate(dnorm, -1.96, Inf)
integrand <- function(x){1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)


# Ejercicios #
# 1 #
nu=2
n=100
muestra1=rexp(n=n,rate=nu)
hist(muestra1,freq=FALSE)
curve(dexp(x,rate=nu),add=TRUE,lwd=2,col="blue",lty=2)

# 2 # 
lambda=0.5
zeta=0.02
n=100
muestra2=rlnorm(n=n,meanlog=lambda,sdlog=zeta)
hist(muestra2,freq=FALSE)
curve(dlnorm(x,meanlog=lambda,sdlog=zeta),add=TRUE,lwd=2,col="red",lty=2)

# 3 #
# X~Normal(24,10.2)
mu=24
sigma=10.2
# a #
# P(X>32)
1-pnorm(q=32,mean=mu,sd=sigma)
pnorm(q=32,mean=mu,sd=sigma,lower.tail = FALSE)
# b # 
# P(20<X<32/X>18)=P(20<X<32 ^ X>18)/P(X>18)=P(20<X<32)/P(X>18)=(P(X<32)-P(X<20))/P(X>18)
(pnorm(q=32,mean=mu,sd=sigma)-pnorm(q=20,mean=mu,sd=sigma))/(pnorm(q=18,mean=mu,sd=sigma,lower.tail = FALSE))
# c # 
# exito="Cliente mantiene cuenta por menos de 24 meses"
# P(exito)=P(X<24)
p=pnorm(q=24,mean=mu,sd=sigma)
p
# Y = cantidad de exitos de un total de n experimentos
n=5
# Y~Binomial(size=n,prob=p)
# P(Y<=1)
pbinom(q=1,size=n,prob=p)
# P(Y=0) + P(Y=1)
dbinom(x=0,size=n,prob=p)+dbinom(x=1,size=n,prob=p)


# Programaci?n b?sica #

# input
a<- 1; b<-4; c<- 2
# calculos
raiz1 <- (-b-sqrt(b^2-4*a*c))/(2*a)
raiz2 <- (-b+sqrt(b^2-4*a*c))/(2*a)
# output
c(raiz1, raiz2)


a<- 1; b<- 4; c<- 2
a<- 1; b<- 2; c<- 1
a<- 2; b<- 2; c<- 1
discriminante <- b^2-4*a*c
discriminante
if(discriminante > 0){
  raiz <- c((-b-sqrt(b^2-4*a*c))/(2*a),
            (-b+sqrt(b^2-4*a*c))/(2*a))
} else {
  if(discriminante == 0){
    raiz <- -b/(2*a)
  } else{
    raiz <- c()
  }
}
raiz


x.seq <- seq(1, 19, 3)
x.seq
producto <- 1
for(x in x.seq){
  producto <- producto*x
  print(producto)
}
producto
# otra forma de hacer lo mismo
for(i in 1:length(x.seq)){
  producto <- prod(x.seq[1:i])
  print(producto)
}


x.seq <- seq(1, 19, 3)
producto <- 1
i <- 1
while(producto < 4000){
  producto <- producto*x.seq[i]
  print(c(i, producto))
  i <- i+1
}


suma2 <- 0
for(i in 1:100){
suma2 <- suma2 + i^2
}
suma2
sum((1:100)^2)


x <- c(-2, -1, 1, 2)
ifelse(x > 0, "Positivo", "Negativo")


AED <- function(x) {
  if(is.numeric(x)==TRUE){
    prom <- mean(x); prom.rec <- mean(x, trim=0.1)
    med <- median(x); cuar <- quantile(x)
    mi <- min(x); ma <- max(x)
    va <- var(x); RIC <- IQR(x)
    par(mfrow=c(2, 1))
    hist(x, freq=F)
    boxplot(x,horizontal=TRUE)
    list(promedio=prom, promedio.recortado=prom.rec, mediana=med, cuartiles=cuar,
         minimo=mi, maximo=ma, varianza=va, RIC=RIC)
  }
  else{
    conteo <- table(x)
  par(mfrow=c(1,2))
  barplot(conteo)
  pie(conteo)
  return(Tabla.conteo=conteo)
  }
}

muestra3=rnorm(n=1000,mean=100,sd=20)
AED(muestra3)
muestra4=c("a","a","a","b","b")
AED(muestra4)



# sapply #
x1 <- rnorm(10); x1
sapply(x1, round)
sapply(x1, round, digits=3)

# apply #
X <- matrix(rnorm(30, 20, 3), ncol=3,
            nrow=10); X
apply(X, MARGIN=2, mean) 
# Repite lo anterior pero calculando la media recortada al 20% para cada columna.
apply(X, 2, mean, trim=0.1)


# tapply #
edad <- rnorm(30, mean=40, sd=sqrt(10));edad
genero <- rbinom(30, size=1, prob=0.3); genero
tapply(edad, genero, mean)
# Repetir lo anterior, pero calculando la media recortada al 20%.
tapply(edad, genero, mean, trim=0.1)



p = 0.116
tot =dgeom(5,prob = p)
tot
tat = 1- pbinom(0, 6, p)
tat
3/4
0.75^3
1-0.421875

40/24
1-ppois(2, 40/24)
ppois(3, 40/24, lower.tail = F)


dbinom(7,13,1)

beta(1,1)

cov = 0.4*0.7*0.7
varv= 2*0.7+cov*2
1-pnorm(3.9,3.9 + (0.3)*(3.8-3.9), 0.7* sqrt(1--0.3^2))
cov=200*-0.3
cov = cov*2 + 30

mu = 3.9 + (0.4)*(3.8-3.9)
var = 0.7* sqrt(1-(0.4^2))
1-pnorm(3.9, mu, var)
pnorm(4, mu, var, lower.tail = FALSE)


dbeta(1, shape1 = 1, shape2 = 1, 0)

pnorm(3.9, mu, des, lower.tail = FALSE)
