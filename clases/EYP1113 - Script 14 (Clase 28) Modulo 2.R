################################
## Comparación de Poblaciones ##
################################

install.packages("TeachingDemos")
library(TeachingDemos)

## X1,...,Xn iid N(mu.x, sigma.x)
## Y1,...,Ym iid N(mu.y, sigma.y)
## Xi indep Yj

## Test de comparación de medias con sigma.x y sigma.y CONOCIDOS: z.test()
## Z = ((mean(X)-mean(Y))-(mu.x-mu.y))/sqrt(sigma.x^2/n+sigma.y^2/m) ~ N(0,1)

## Test de comparación de medias con sigma.x = sigma.y (DESCONOCIDOS): t.test()
## T = ((mean(X)-mean(Y))-(mu.x-mu.y))/(Sp*sqrt(1/n+1/m) ~ t-Student(n+m-2)

## Test de comparación de medias con sigma.x != sigma.y (DESCONOCIDOS): t.test()
## T = ((mean(X)-mean(Y))-(mu.x-mu.y))/sqrt(s.x^2/n+s.y^2/m) ~ t-Student(nu)

Base = read.table("ENS2009.txt", header = T, dec = ",")
head(Base)
summary(Base)

## Colesterol: H0: mu.H = mu.M vs Ha: mu.H != mu.M
install.packages("dplyr")
library(dplyr)
X = filter(Base, SEXO == "MASCULINO")$COLESTEROL
Y = filter(Base, SEXO == "FEMENINO")$COLESTEROL

## Tenemos dos opciones: (1) asumir varianzas iguales, (2) asumir varianza distintas

## Opción (1)
aux = t.test(x = X, y = Y, var.equal = T, alternative = "two.sided")
aux
## T0
aux$statistic
## valor-p
aux$p.value
## Si nuestro nivel de significancoa fuese de un 10%, no existe suficiente evidencia para rechazar H0 (igualdad de medias) 

## ¿Fue valido el supuesto de igualdad de varianzas? Necesitamos hacer un nuevo test

## H0: sigma.x = sigma.y. vs  Ha: sigma.x != sigma.y 
aux = var.test(x = X, y = Y, alternative = "two.sided")
## F0 = sx^2/sy^2 ~ F(n-1, m-1)
aux$statistic
## valor-p
aux$p.value
## Si nuestro nivel de significancia fuese 10%, entonces existe suficiente evidencia para rechazar H0 (varianzas iguales), es decir, el teste anterior no sería valido

## Opción (1)
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
## Se observa evidencia para apoyar la afirmación, ¿será suficiente estadísticamente?
aggregate(COLESTEROL ~ HTA, data = Base, FUN = sd)
## Se observa una pequeña diferencia entre desviaciones estandar... Vamos a realizar un test F de igualdad de varianzas
aux = var.test(x = X, y = Y, alternative = "two.sided")
aux$p.value ## No se rechaza igualdad de varianzas (Se rechaza H0 si valor-p es pequeño menor a alpha = 1%, 5%, 10%)

aux = t.test(x = X, y = Y, alternative = "greater", var.equal = T) ## Ha: mu.x > mu.y
aux$p.value ## Tenemos un valor-p muuuuuy pequeño ---> Se rechaza H0 y se apoya la afirmación que decia: "los hipertensos tienen nivel de colesterol mayor a los NO hipertensos"

## Como hascer el z.test a mano, ya que la funcion z.test() solo esta una muestra
sigma.x = 42
sigma.y = 41
n = length(X)
m = length(Y)
Z0 = (mean(X)-mean(Y))/sqrt(sigma.x^2/n+sigma.y^2/m)
Z0
## valor.p para Ha: mu.x != mu.y
2*(1-pnorm(abs(Z0)))
## valor.p para Ha: mu.x < mu.y
pnorm(Z0)
## valor.p para Ha: mu.x > mu.y
1-pnorm(Z0)

## install.packages("BSDA")
library("BSDA")
z.test(x = X, y = Y, sigma.x = sigma.x, sigma.y = sigma.y, alternative = "two.sided")$statistic
z.test(x = X, y = Y, sigma.x = sigma.x, sigma.y = sigma.y, alternative = "two.sided")$p.value
z.test(x = X, y = Y, sigma.x = sigma.x, sigma.y = sigma.y, alternative = "greater")$p.value

