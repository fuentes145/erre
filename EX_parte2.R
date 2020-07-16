library(TeachingDemos)
rm(list = ls())

#########################
##ESTIMADOR DE MOMENTOS##
#########################
lambda = 1.2 
zeta = 0.3
Y = rlnorm(, meanlog=lambda, sdlog=zeta)
zeta.em = sqrt(log(mean(Y^2)-2*log(mean(Y))))
lambda.em=log(mean(Y))-(1/2)*zeta.em^2



#####################
##TEST DE HIPOTESIS##
#####################

X=
mu= 
sd=
n=
S=
###distribuciones normales

Z = (X - mu) / (sd/sqrt(n))
#pnorm(Z,0,1)

T = (X-mu) / (S/sqrt(n))
#pt(T, n-1)

C = ((n-1)*(S**2)) / (sd**2)
#pgamma((n-1)/2, 1/2)


###distribuciones no normales
#Bernulli
n1 = 22
p1 = 7/n1
n2 = 16
p2 = 8/n2
p = (7+8)/(n1+n2)
Z0 = (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Z0## A mano con Rpnorm(Z0) ## No rechaza Ho## Con z.test
z.test(x = (p1-p2), mu = 0, sd = sqrt(p*(1-p)*(1/n1+1/n2)), alternative = "less")$p.value

#exponencial
v_=
v=
Z = (v_-v)/(v/sqrt(n))

#poisson
l_=
l=
Z = (l_-l)/(sqrt(l/n))

#pnorm(Z)

## Como cabia el calculo del valor.p para distintas Ha
valor.p = pnorm(Zo)             ## Ha: <
valor.p = 1-pnorm(Zo)           ## Ha: >
valor.p = 2*(1-pnorm(abs(Zo)))  ## Ha: !=


###########################
##INTERVALOS DE CONFIANZA##
###########################


### intervalos de confianza 


# Bernulli
confianza=0.9
p_=46/(46+32+31+30+15+6)
n=46+32+31+30+15+6

alfa = 1-confianza
k_1= qnorm(1-alfa/2)
w = k_1*sqrt(p_*(1-p_)/n)

lim_sup = p_ + w
lim_inf = p_ - w
lim_sup
lim_inf


### buscar tmaño de muestra
confianza=0.9
p=32/(46+32+31+30+15+6)
n=46+32+31+30+15+6
w=0.05

sd= sqrt(p*(1-p))
alfa = (1-confianza)/2
k_1= qnorm(1-alfa/2)

## se puede omitir el sd en la formula
tamaño= ((k_1*sd)/w)**2
tamaño
####en el ensayo N
confianza = 0.99
alpha = 1-confianza
w = 0.05
n=(qnorm(1-alpha/2)/(2*w))^2
n
## en el ensayo w
n = 300
confianza = 0.95
alpha = 1-confianza
w = qnorm(1-alpha/2)/(2*sqrt(n))
w




##############################
##COMPARACION DE POBLACIONES##
##############################

#### antes hacer un test para ver si las varianzas son iguales. test F

## Comaparemos las medias. Las varianzas son desconocidas: ¿iguales o distintas? 
sx =
sy=
n1=
n2=
F0 = (sx/sy)^2
alpha = 0.05
qf(alpha/2, df1 = n1-1, df2 = n2-1) < F0 
F0 < qf(1-alpha/2, df1 = n1-1, df2 = n2-1)


###DISTRIBUCIONES NORMALES iid
X=
Y=
mu.x = 4.5
sx = 0.7
n1 = 16
sy = 0.5
mu.y = 4.2
n2 = 22

## Test de comparación de medias con sigma.x y sigma.y CONOCIDOS: z.test()
Z = ((X-Y)-(mu.x-mu.y))/sqrt(sigma.x^2/n1+sigma.y^2/n2) 
#pnorm(Z)

## Test de comparación de medias con sigma.x = sigma.y (DESCONOCIDOS): t.test()
## Ho: mu.x = mu.y vs Ha: mu.x < mu.y
Sp = sqrt(((n1-1)*sx**2+(n2-1)*sy**2)/(n1+n2-2)) 
T0 = (X-Y)*(mu.x-mu.y)/(Sp*sqrt(1/n1+1/n2))
1-pt(T0, df = n1+n2-2)

## Test de comparación de medias con sigma.x != sigma.y (DESCONOCIDOS): t.test()
T0 = (X-Y)*(mu.x-mu.y)/sqrt(sx**2/n1+sy**2/n2))
nu = (sx**2/n1+sy**2/n2)**2 / ( (sx**2/n1)**2/(n-1) + (sy**2/n2)**2/(n2-1) )
pt(T0, nu)


###DISTRIBUCION BERNULLI
n1 = 22
p1 = 7/n1
n2 = 16
p2 = 8/n2
p = (7+8)/(n1+n2)
Z0 = (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Z0## A mano con Rpnorm(Z0) ## No rechaza Ho## Con z.test
z.test(x = (p1-p2), mu = 0, sd = sqrt(p*(1-p)*(1/n1+1/n2)), alternative = "less")$p.value
#pnorm(Z,0,1)

###DISTRIBUCION POISSON
X= 
Y= 
l.x = 
l.y = 
n=
m=
## para parametro lamnda l
Z = ((X-Y)-(l.x-l.y))/sqrt(l.x/n+l.y/m) 
#pnorm(Z,0,1)

###DISTRIBUCION ESPONENCIAL
X= 
Y= 
nu.x = 
nu.y = 
n=
m=
## para parametro lamnda l
Z = ((X-Y)-(nu.x-nu.y))/sqrt(1/(n*nu.x**2)+1/(m*nu.y**2)) 
#pnorm(Z,0,1)



###########################7
