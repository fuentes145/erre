##############################
## Estimación y Propiedades ##
##############################

mu = 10
sigma = 1
Poblacion = rnorm(10000000, mean = mu, sd = sigma)
hist(Poblacion, freq = F, col = "gray", border = "white")
n = 10
x = sample(Poblacion, n) ## Esta función realiza un muestreo aleatorio, por defecto sin reemplazo

## Que valor le damos a mu? 
## mu1 = (min+max)/2
## mu2 = (P25+75)/2
## mu3 = P50
## mu4 = Promedio

mu1 = c()
mu2 = c()
mu3 = c()
mu4 = c()
for(i in 1:2000){
x = sample(Poblacion, n)
mu1[i] = (min(x)+max(x))/2
mu2[i] = (quantile(x, 0.25)+quantile(x, 0.75))/2
mu3[i] = median(x)
mu4[i] = mean(x)
}

apply(cbind(mu1,mu2,mu3,mu4),2,mean) ## Las 4 propuestas empiriucamente son estimadores insesgados
## Insesgamiento E(mu.hat) = mu

apply(cbind(mu1,mu2,mu3,mu4),2,var)

boxplot(cbind(mu1,mu3,mu2,mu4))
abline(h = mu, col = "red")

## más eficiente seria el primedio

## ¿Cuál es el efecto n?

mu10 = c()
mu100 = c()
mu1000 = c()
mu10000 = c()

for(i in 1:500){
n = 10
x = sample(Poblacion, n)
mu10[i] = mean(x)
n = 100
x = sample(Poblacion, n)
mu100[i] = mean(x)
n = 1000
x = sample(Poblacion, n)
mu1000[i] = mean(x)
n = 10000
x = sample(Poblacion, n)
mu10000[i] = mean(x)
}

boxplot(cbind(mu10,mu100,mu1000,mu10000))

#############################
### Estimacion de Momento ###
#############################

Data = read.table("ENS2009.txt", dec = ",")
head(Data)

X = Data$HDL
hist(X, freq = F)
## Como tenemos asimetria positiva ajustaremos una Log-Normal, Gamma y también Normal para comparar

zeta = sqrt(log(mean(X^2))-2*log(mean(X)))
lambda = log(mean(X))-0.5*zeta^2
x = seq(0,200,0.01)
lines(dlnorm(x, meanlog = lambda, sdlog = zeta)~x, col = "red")

k = mean(X)^2/(mean(X^2)-mean(X)^2)
nu= mean(X)/(mean(X^2)-mean(X)^2)
lines(dgamma(x, shape = k, rate = nu)~x, col = "blue")

mu = mean(X)
sigma = sqrt(mean(X^2)-mean(X)^2)
lines(dnorm(x, mean = mu, sd = sigma)~x, col = "orange")
