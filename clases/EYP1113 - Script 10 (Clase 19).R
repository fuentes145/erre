#################################################
## Funciones de Múltiples Variables Aleatorias ##
#################################################

## Ejemplo 4.13
lw = log(2000) ## log(): logaritmo natural 
lf = log(20)
le = log(1.6)
zw = sqrt(log(1+0.20^2))
zf = sqrt(log(1+0.15^2))
ze = sqrt(log(1+0.125^2))
lc = lw + lf - 0.5 * le
zc = sqrt(zw^2 + zf^2 + (-0.5)^2 * ze^2)
## C ~ Log-Normal(lc, zc)
1-plnorm(35000, meanlog = lc, sdlog = zc)
plnorm(35000, meanlog = lc, sdlog = zc, lower.tail = F)

################################
## Teorema del Limite Central ##
################################

## X1,X2,...,Xn iid Bernoulli(p) --> E(Xi) = p, Var(Xi) = p * (1-p)
## X1+X2+...+Xn exacta Binomial(n, p) aprox Normal(n * p, sqrt(n) * sqrt(p*(1-p)))

n = 30
p = 1/3
x = 0:n
px = dbinom(x, size = n, prob = p)
plot(px~x, type = "h", bty = "n", las = 1)
x = seq(0,n,0.01)
fx = dnorm(x, mean = n * p, sd = sqrt(n * p * (1-p)))
lines(fx ~ x, col = "red", lwd =2)
axis(1, c(13, 14))

## P(X <= 13)
pbinom(13, size = n, prob = p)
x = seq(0,13,0.01)
fx = dnorm(x, mean = n * p, sd = sqrt(n * p * (1-p)))
lines(fx ~ x, col = "red", lwd =2, type = "h")
pnorm(13, mean = n*p, sd = sqrt(n*p*(1-p)))

x = seq(13,14,0.001)
fx = dnorm(x, mean = n * p, sd = sqrt(n * p * (1-p)))
lines(fx ~ x, col = "orange", lwd =2, type = "h")

## P(X < 14) = P(X <= 13)
pnorm(14, mean = n*p, sd = sqrt(n*p*(1-p)))

abline(v = 13.5, lty = 2)
## P(X < 14) = P(X <= 13) = P(X < 13.5)
pnorm(13.5, mean = n*p, sd = sqrt(n*p*(1-p))) ## Corrección por continuidad()

## Error de aproximación 
abs(1-pnorm(13.5, mean = n*p, sd = sqrt(n*p*(1-p)))/pbinom(13, size = n, prob = p))*100

## X1,X2,...,Xn iid Exp(lambda) = Gamma(1,lambda) --> E(Xi) = 1/lambda, Var(Xi) = 1/lambda^2
## X1+X2+...+Xn exacta Gamma(n, lambda) aprox Normal(n / lambda, sqrt(n) / lambda))
n = 60
lambda = 0.4
## P(X1+...+Xn <= 135)
pgamma(135, shape = n, rate = lambda)
pnorm(135, mean = n/lambda, sd = sqrt(n)/lambda)

## Error de aproximación
abs((1-pnorm(135, mean = n/lambda, sd = sqrt(n)/lambda)/pgamma(135, shape = n, rate = lambda)))*100

## Graficamente se observa un comportamiento muy similar
x = seq(0,(5*n),0.01)
plot(dgamma(x, shape = n, rate = lambda)~x, type = "l", bty = "n", las = 1)
lines(dnorm(x, mean = n/lambda, sd = sqrt(n)/lambda) ~ x, col = "red", lwd =2)





