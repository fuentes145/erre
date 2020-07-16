
##################
# Laboratorio 10 #
##################


# Máximo de Uniforme 
n=30
N=2000
b=5
a=0
muestras=matrix(runif(n*N,min=a,max=b),ncol=N,nrow=n)
estadisticos=apply(muestras,2,max)
hist(estadisticos,freq=FALSE,
     main="Distribución muestral del máximo 
     \nde variables aleatorias uniformes",
     xlab="Estimadores de b",ylab="Densidad")
curve(n*x**(n-1)/(b**n),add=TRUE,col="darkred",
      lty=2,lwd=3)


# p de Bernoulli
n=50
N=7000
p=0.3
muestras=matrix(rbinom(n*N,size=1,prob=p),ncol=N,nrow=n)
estadisticos=apply(muestras,2,mean)
hist(estadisticos,freq=FALSE,
     main="Distribución muestral del promedio\n
     de variables aleatorias Bernoulli",
     xlab="Estimadores de p",ylab="Densidad")
curve(dnorm(x,mean=p,sd=sqrt(p*(1-p)/n)),add=TRUE,col="darkred",
      lty=2,lwd=3)

# lambda de Poisson
n=70
N=1000
lambda=15
muestras=matrix(rpois(n*N,lambda=lambda),ncol=N,nrow=n)
estadisticos=apply(muestras,2,mean)
hist(estadisticos,freq=FALSE,
     main="Distribución muestral del promedio\n
     de variables aleatorias Poisson",
     xlab="Estimadores de lambda",ylab="Densidad",
     ylim=c(0,1),breaks=20)
curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="darkred",
      lty=2,lwd=3)


# Test de Hipótesis #

# Para mu con sigma conocido #
install.packages("TeachingDemos")
library(TeachingDemos)

N=1000
mu=55
sigma=11
X=rnorm(n=N,mean=mu,sd=sigma)
summary(muestra)
sd(muestra)
mu0=60

# H0: mu=mu0 vs H1 mu!=mu0
z.test(X,mu=mu0,sd=sigma,alternative="two.sided")
mu0=55
z.test(X,mu=mu0,sd=sigma,alternative="two.sided")
# H0: mu<=mu0 vs H1 mu>mu0
mu0=60
z.test(X,mu=mu0,sd=sigma,alternative="greater")
# H0: mu>=mu0 vs H1 mu<mu0
z.test(X,mu=mu0,sd=sigma,alternative="less")

# Para mu con sigma desconocido #
mu0
# H0: mu=mu0 vs H1 mu!=mu0
t.test(X,mu=mu0,alternative="two.sided")
# H0: mu<=mu0 vs H1 mu>mu0
t.test(X,mu=mu0,alternative="greater")
# H0: mu>=mu0 vs H1 mu<mu0
t.test(X,mu=mu0,alternative="less")


# Para sigma con mu desconocido #
sd(X)
sigma0=12
# H0: sigma=sigma0 vs H1 sigma!=sigma0
sigma.test(X,sigma=sigma0,alternative="two.sided")
sigma0=11
sigma.test(X,sigma=sigma0,alternative="two.sided")
# H0: sigma<=sigma0 vs H1 sigma>sigma0
sigma=12
sigma.test(X,sigma=sigma0,alternative="greater")
# H0: sigma>=sigma0 vs H1 sigma<sigma0
sigma.test(X,sigma=sigma0,alternative="less")

