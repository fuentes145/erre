## LOG-NORMAL
mu= 6
sd=
delta=0.31
zeta=sqrt(log(1+delta**2))
lambda=log(mu)-(1/2 * (zeta**2))
plnorm(7 , meanlog=lambda, sdlog=zeta)


## 1. X~Poisson(lamnda) e Y ~ Poisson(nu)
## E(X+Y) = lambda + nu
## Var(X+Y) = lambda + nu + 2 * sqrt(lambda * nu) * rho
lambda = 4
nu = 3
rho = -0.6
mu = lambda + nu
sigma = sqrt(lambda + nu + 2 * sqrt(lambda * nu) * rho)
delta = sigma/mu
delta

pgeom(5,0.116)
1-ppois(3,40/24)

sqrt((m-0)**2*0.4+ (m-1)**2*0.3+ ((m-2))**2*0.2+ ((m-3))**2*0.1 )
m = 0.4*0+0.3*1+0.2*2+0.1*3


## normal 

pnorm(4)
(4.15-4)/0.2



0.2*qnorm(0.8413)+4

((968**4)*log(1-log(0.5)))**(1/4)

sd =sqrt( 5**2 + 15**2 - 2* 45 )
1-pnorm(90, 75, sd )

pbinom(85, 39, prob= 2.1)

40*2.1
pnorm(83, 84, 0.8)


m=1
n=7
N=33
dhyper(1, m = m, n = N-m, k = n)

