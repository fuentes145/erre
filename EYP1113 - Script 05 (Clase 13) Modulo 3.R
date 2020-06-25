#############################
## Script Modelos Disretos ##
#############################

## Ejercicio 9 ##

## T: Tiempo operacional de una niveladora
## T ~ Log-Normal(lambda, zeta)
muT = 1500
deltaT = 0.30
zeta = sqrt(log(1+deltaT^2))
lambda = log(muT)-0.5*zeta^2 ## despejar lambda desde E(T)
cbind(lambda,zeta)

## p = P(T <= 900): plnorm()
## {T <= 900} = exito
p = plnorm(900, meanlog = lambda, sdlog = zeta)
p

## Y: # niveladoras que fallan antes de las 900 horas de funcionamiento.
## Y ~ Binomial(n = 5, p = 0.05554377)
n = 5

## (a) P(Y = 2) = dbinom(2, size = n, prob = p)
dbinom(2, size = n, prob = p)

## (b) P(Y >= 2) = 1 - P(Y < 2) = 1 - P(Y <= 1) = P(Y > 1)
## 1 - P(Y <= 1)
1-pbinom(1, size = n, prob = p )
## P(Y > 1)
pbinom(1, size = n, prob = p, lower.tail = F)

## (c) P(Y <= 2)
pbinom(2, size = n, prob = p)

## Modelo Geométrico(p)
## N: # experimentos Bernoulli hasta la ocurencia del 1er exito
## En R, la definen como el # de fracasos hasta el 1er exito

## Ejemplo 10
p = 0.05

## P(N <= 20) = pgeom(20-1, prob = p) = 1 - (1-p)^trunc(20), [.] = trunc(.)
1 - (1-p)^trunc(20)   ## Por definción
pgeom(20-1, prob = p) ## Utilizando función pgeom() de R

## P(N > 3) = 1 - P(N <= 3)
1- pgeom(3-1, prob = p)              ## 1 - P(N <= 3)
pgeom(3-1, prob = p, lower.tail = F) ## P(N > 3)

## P(N = 5 | N > 3) = P({N = 5} int {N > 3})/P(N > 3) = P(N = 5)/P(N > 3) = P(N = 2)
dgeom(5-1, prob = p)/pgeom(3-1, prob = p, lower.tail = F)
dgeom(2-1, prob = p) ## Propiedad de Carencia de Memoria

