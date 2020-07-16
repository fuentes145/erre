#####################################
## Estimacio Puntual y Propiedades ##
#####################################

## theta: Parametro desconocido
## X1,...,Xn muestra aleatoria (iid)
## theta.hat = T(X1,...,Xn): Estimador de theta
## Como la muestra es aleatoria --> T(X1,...,Xn) tambien es una v.a.

## Simulamos un caracteristica de una población
## X ~ Normal(mu, sigma)
mu = 10
sigma = 1
X = rnorm(10000000, mean = mu, sd = sigma)
hist(X, freq = F, col = "gray", border = "white")

## Tomemos una muestra de tamaño n
n = 100
x = sample(X, n) ## esta función por defecto realiza un muestreo aleatorio sin reemplazo
x

## Bajo el supuesto que X ~ Normal, queremos estimar el parametro mu (que es desconocido, ya lo olvidamos)

## Se proponen 4 estimadores para mu
mu1 = (min(x)+max(x))/2
mu2 = (quantile(x, 0.25) + quantile(x, 0.75))/2
mu3 = median(x)
mu4 = mean(x)
cbind(mu1,mu2,mu3,mu4)
## Cual de estos cuatro es mejor?

## Realicemos un experimento de simulación

mu1 = c()
mu2 = c()
mu3 = c()
mu4 = c()
for(i in 1:1000){
n = 100
x = sample(X, n)
mu1[i] = (min(x)+max(x))/2
mu2[i] = (quantile(x, 0.25) + quantile(x, 0.75))/2
mu3[i] = median(x)
mu4[i] = mean(x)
}

M = cbind(mu1,mu2,mu3,mu4)

## ¿Son insesgados? La esperaza (el promedio) debiera acertar, es decir, estar cercano a 10
apply(M, 2, mean)
## estas cuatro propuestas se espera que le apunten al parametro
apply(M, 2, var)
apply(M, 2, sd)

boxplot(M)
abline(h = mu, lty = 2, col = 2, lwd = 2)

## En este como los cuatro son insesgados (empiricamente), escogemos el mas eficiemnte a partir de la varianza
## El de menor varianza seria mu4 (promedio)

## ¿Si alguno fuese sesgado, como comparamos? Con ECM = Var + Sesgo^2, el mas eficiente es el que tiene menor ECM

## ¿Cuál es el efecto del tamaño de la muestra?

mu10 = c()
mu100 = c()
mu1000 = c()
mu10000 = c()
for(i in 1:500){
n = 10
x = sample(X, n)
mu10[i] = mean(x)
n = 100
x = sample(X, n)
mu100[i] = mean(x)
n = 1000
x = sample(X, n)
mu1000[i] = mean(x)
n = 10000
x = sample(X, n)
mu10000[i] = mean(x)
}

M = cbind(mu10,mu100,mu1000,mu10000)
boxplot(M)

abline(h = mu, lty = 2, col = "red", lwd = 3)
## A medida que n crece la credibilidad del estimador aumenta, es decir, consistencia.

##########################
## Estimador de Momento ##
##########################

Data = read.table("ENS2009.txt", dec = ",")
head(Data)

## ¿Como distribuye el HDL?
X = Data$HDL
hist(X, freq = F, col = "gray", border = "white", nclass = 10)

## Dado que presentan una asimetria posiva una Gamma y Log-Normal podrían ajustar bien

## Gamma(k,nu)
k = mean(X)^2/(mean(X^2)-mean(X)^2)
nu = mean(X)/(mean(X^2)-mean(X)^2)
cbind(k,nu)
x = seq(0,200,0.01)
lines(dgamma(x, shape = k, rate = nu)~x, lwd = 2, col = "darkred")

## Log-Normal(lambda, zeta)
zeta = sqrt(log(mean(X^2))-2*log(mean(X)))
lambda = log(mean(X))-0.5*zeta^2
cbind(lambda,zeta)
lines(dlnorm(x, meanlog = lambda, sdlog = zeta)~x, lwd = 2, col = "darkorange")

## Cual ajusta mejor? Cap7

## Normal(mu, sigma)
mu= mean(X)
sigma = sqrt(mean(X^2)-mean(X)^2)
cbind(mu, sigma)
lines(dnorm(x, mean = mu, sd = sigma)~x, lwd = 2, col = "steelblue")

 


