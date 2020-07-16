rm(list = ls())

## posibles librerias
library(dplyr)
library(foreign)
library(TeachingDemos)
# library(rio) Función import():

### comandos utiles 
data = read.table("ENS2009.txt", header = T, dec = ",")
typeof(data)
summary(data)
A = filter(data, JAULA == "A" )$PESO
head(data)
X = filter(Base, SEXO == "MASCULINO")$COLESTEROL


aux = t.test(x = X, y = Y, var.equal = T, alternative = "two.sided")
aux
aux$statistic
aux$p.value

## ¿Fue valido el supuesto de igualdad de varianzas? Necesitamos hacer un nuevo test

## H0: sigma.x = sigma.y. vs  Ha: sigma.x != sigma.y 
aux = var.test(x = X, y = Y, alternative = "two.sided")
## F0 = sx^2/sy^2 ~ F(n-1, m-1)
aux$statistic
## valor-p
aux$p.value
## Si nuestro nivel de significancia fuese 10%, entonces existe suficiente evidencia para rechazar H0 (varianzas iguales), es decir, el teste anterior no sería valido

## Opción (2)
aux = t.test(x = X, y = Y, var.equal = F, alternative = "two.sided")
aux
## T0
aux$statistic
## valor-p
aux$p.value
## No existe suficiente evidencia para rechazar H0 (igualdad de medias)

#####################################################
## Colesterol: H0: mu.X = mu.Y vs Ha: mu.X != mu.Y ##
#####################################################

## X: Hipertensos
## Y: NO Hipertensos
X = filter(Base, HTA == "SI")$COLESTEROL
Y = filter(Base, HTA == "NO")$COLESTEROL
## Existe evidencia para afirmar que los hipertensos presentan un colesterol mayor a los no hipertensos?
aggregate(COLESTEROL ~ HTA, data = Base, FUN = mean)
aggregate(COLESTEROL ~ HTA, data = Base, FUN = sd)
## Se observa una pequeña diferencia entre desviaciones estandar... Vamos a realizar un test F de igualdad de varianzas
aux = var.test(x = X, y = Y, alternative = "two.sided")
aux$p.value ## No se rechaza igualdad de varianzas (Se rechaza H0 si valor-p es pequeño menor a alpha = 1%, 5%, 10%)
aux = t.test(x = X, y = Y, alternative = "greater", var.equal = T) ## Ha: mu.x > mu.y
aux$p.value #






#####################
##TEST DE HIPOTESIS##
#####################
library(TeachingDemos)

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


###distribuciones no normales
#Bernulli p_ es p.gorro
n=
p_=
p0=
table(Data$HTA)
prop.test(x = 39, n = n , p = p0, alternative = "less", correct = F)$p.value
z.test(x = p_, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$statistic
z.test(x = p_, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$p.value

#exponencial
# ojo q puse mu = v0 y x = v_ q puede estar mal por puede ser 1/v
n=
v_=
v0=
z.test(x = 1/v_, mu = 1/v0, sd = v0/sqrt(n), alternative = "less")$p.value

#poisson
n=
l_=
l0=
z.test(x = l_, mu = l0, sd = sqrt(l0/n), alternative = "less")$p.value


##########################
##INTERVALO DE CONFIANZA##
##########################

## bernulli

z.test(x = p.gorro, sd = sqrt(p.gorro*(1-p.gorro)/n), conf.level = 0.95)$conf.int


##############################
##COMPARACION DE POBLACIONES##
##############################
install.packages("TeachingDemos")
library("TeachingDemos")
library("BSDA")

data = read.table("ENS2009.txt", header = T, dec = ",")
summary(data)
library(dplyr)
X = filter(data, JAULA == "A" )$PESO
###DISTRIBUCIONES NORMALES iid
X= 
Y= 


## Test de comparación de medias con sigma.x y sigma.y CONOCIDOS: z.test()
sigma.x =
sigma.y = 
z.test(x= X, y= Y, alternative = "t", mu = 0, sigma.x = sigma.x , sigma.y = sigma.y)

#### antes hacer un test para ver si las varianzas son iguales. test F
## Opción 2: var.test
var.test(HDL~HTA, data = Data, alternative = "two.sided")$statistic ## Fo = s.y^2/s.x^2 ~ F(n.y - 1, n.x - 1)
var.test(HDL~HTA, data = Data, alternative = "two.sided")$p.value

## Opción 3: var.test
library(dplyr)
X = filter(Data, HTA == "SI")$HDL
Y = filter(Data, HTA == "NO")$HDL

## Fo = s.x^2/s.y^2 ~ F(n.x - 1, n.y - 1)
var.test(x = X, y = Y, alternative = "two.sided")$statistic 
var.test(x = X, y = Y, alternative = "two.sided")$p.value 



T_critico = qt(0.9, df= length(X) + length(Y) - 2)
$statistic > T_critico
## Test de comparación de medias con sigma.x = sigma.y (DESCONOCIDOS): t.test()
t.test(x = X, y = Y, var.equal = T, alternative = "g")$p.value

## Test de comparación de medias con sigma.x != sigma.y (DESCONOCIDOS): t.test()
t.test(x = X, y = Y, var.equal = F, alternative = "g")$p.value



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





####################
##REGRESION LINEAL##
####################
## ¿El colesterol aumenta con la edad? 
plot(COLESTEROL ~ EDAD, data = Data, pch = 20, bty = "n", las = 1)
abline(h = mean(Data$COLESTEROL), col = "red", lwd = 3, lty = 2)

## Función lm() entrega recta de regresión y muchas otras cosas
recta = lm(COLESTEROL ~ EDAD, data = Data)
abline(recta, col = "blue", lwd = 3)

## Será la recta distinta al promedio
summary(recta)
## t.value asociado a la pendiente
summary(recta)$coefficients
summary(recta)$coefficients[2,3] ## To ~ t-Student(n-2)
summary(recta)$coefficients[2,4] 
## se rechaza  ho por que la p value era muy chico 
##  lo que significa q la pendiente era lo suficiente mente significativa para que exista regrecion 
## en la salida del sumari esta el test.t asociado al beta de la edad.
## esta el test F que me dice si hay o no regrecion
## para cuantificar el grado de asociasion usamos el R^2  o el r² ajustado

# # Interpretación:
## Por cada año de envejecimiento el colesterol aumente en 0.6651 md/dL
## Por cada decada de envejecimiento el colesterol aumente en 6.651 md/dL

## En el caso de regresión simple t.value^2 = F-statistic ---> identicos valores-p
To = summary(recta)$coefficients[2,3]
To^2

## Coeficientes de Determinación
## R2: Multiple R-squared (% de variabilidad explicada por el modelo)
## r2: Adjusted R-squared (% de variabilidad explicada por el modelo) Penaliza la complejidad del modelo

plano = lm(COLESTEROL ~ EDAD + SEXO, data = Data)
summary( plano)

## F-statistic: 5.342 (Ho: b.EDAD = b.SEXO = 0. vs Ha: Al menos un b es distinto de 0)
## Ho: No hay regresion vs Ha: Hay Regresión
## Valor.p < 5% --> Hay regresión 


View(iris)
# 1 #
modelo1=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris)

data=iris[,c(1:4)]
modelo2=lm(Sepal.Length~.,data=data)

modelo1
modelo2


# 2 # intervalo de confianza para el petal.whith que es el cuarta fila de la matriz lm.coeficients
summary(modelo1)
aux=summary(modelo1)
aux$coefficients[4,1]
beta3=aux$coefficients[4,1];beta3
se3=aux$coefficients[4,2]
alpha=0.05
n=dim(data)[1]
k=dim(data)[2]-1
t3=qt(1-alpha/2,n-(k+1));t3
c(beta3-se3*t3,beta3+se3*t3)

# 3 # para clacular el estadistico F necesito anova que me da lo scr y sce
anova(modelo1)
aux=anova(modelo1)
SCR=sum(aux$`Sum Sq`[1:k])
SCE=aux$`Sum Sq`[k+1]
SCT=SCR+SCE
F=(SCR/k)/(SCE/(n-(k+1)));F
qF=qf(1-alpha,k,n-(k+1));qF
summary(modelo1)

# 4 # r ² y r² ajustado con el sumary se ssacan altoque pero aca esta manual
r2=SCR/SCT;r2
r2a=1-(1-r2)*(n-1)/(n-(k+1));r2a




##################
##TEST DE AJUSTE##
##################


#test chi cuadrado
# h0: proporcion real = 4,2,1   ha: la muestra no tiene prop 4.2.1

k = 3
v = # la cantidad de parametros
real = c(2000, 1000, 500)
muestra = c(2010, 1090, 400)

chic = sum(((muestra-real)**2)/real) 

# chi critico al 95%
chi_critico = qchisq(0.95, k-1-v)
#si es false no rechaso 
chic > chi_critico

p = 1 - pchisq(chic, k-1-v)

####  con qchisq.test
proporciones = diff(plnorm(c(-Inf,20,25,30,35,Inf))
X2 = chisq.test(x = muestra, p = proporciones)$statistic
1- pchisq(X2, len(muestra)-1-v) 

proporciones = c(0.0577,0.2875,0.368, 0.2023,0.0845)
muestra = c(7,36,45,23,11)
X2 = chisq.test(muestra , p = proporciones)$statistic
pchisq(X2, 5-1-2) 

x_=(3350.035/122)**2
x=995516.78**2/122
sd=sqrt(x-x_)


pnorm(25, x_, sd)


