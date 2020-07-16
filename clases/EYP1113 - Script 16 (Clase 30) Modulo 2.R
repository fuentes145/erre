##########################################
## EYP1113 - Probabilidad y Estadistica ##
##########################################

## Preparación R - Examen ##

## Carpeta de Trabajo
getwd()
dir()

## Cambiar carpeta de trabajo
setwd("/Users/raolea")
getwd()

## Leer datos
## Función read.table(): Lee archivos TXT
## library(rio); Función import(): Lee archivos de todo
Data = read.table("ENS_Mod2.txt", header = T)
head(Data)
summary(Data)


########################
## Test de Proporción ##
########################

## Nivel de significancia igual al 5%
alpha = 0.05

################
## Pregunta 1 ##
################

## Menos de 1/3 de la población son hipertensos?
## Ho: p = 1/3 vs Ha: p < 1/3
p0 = 1/3
head(Data)
n = dim(Data)[1]
aux = table(Data$HTA)/n ## Se observa evidencia que apoya ha, ¿será significativa?
p.gorro = aux[2]
p.gorro

## Vamos a realizar una prueba de hipotesis aproximada (test Z)
Zo = (p.gorro - p0)/sqrt(p0*(1-p0)/n)
Zo
## Valor-p? 
valor.p = pnorm(Zo) ## Por que Ha: <
valor.p ## valor-p = 37.44% > 5% = alpha ---> No hay evidencia para rechazar Ho, es decir, no podemos apoyar la afirmación que dice que menos de 1/3 son Hipertensos

## Como cabia el calculo del valor.p para distintas Ha
valor.p = pnorm(Zo)             ## Ha: <
valor.p = 1-pnorm(Zo)           ## Ha: >
valor.p = 2*(1-pnorm(abs(Zo)))  ## Ha: !=


## Utilizando funciones de R
table(Data$HTA)
prop.test(x = 39, n = n, p = p0, alternative = "less", correct = F)$p.value
library(TeachingDemos)
z.test(x = p.gorro, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$statistic
z.test(x = p.gorro, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$p.value


################
## Pregunta 2 ##
################

## Existe evidencia para afirmar que las proporciones de HTA en hombres y mujeres difieren?
## Ho: p.H = p.M vs Ha: p.H != p.M
## Test de comparación de proporciones

M = table(Data[,c("SEXO", "HTA")])
p.gorro.H = 18/(38+18)
p.gorro.M = 21/(45+21)
n.H = 38+18
n.M = 45+21
p = (n.H*p.gorro.H+n.M*p.gorro.M)/(n.H+n.M) ## Estimación de p bajo Ho: p.H = p.M = p
Zo = (p.gorro.H - p.gorro.M)/sqrt(p*(1-p)/n.H + p*(1-p)/n.M)
Zo
valor.p = 2 * (1-pnorm(abs(Zo)))
valor.p ## valor-p = 96.94% > 5% = alpha ---> No existe sificiente evidencia para rechazar la igualde de proporcion (Ho)

## Utilizando función prop.test()
prop.test(x = c(18,21), n = c(n.H, n.M), correct = F, alternative = "two.sided")$p.value
## Utilizando función z.test()
z.test(x = p.gorro.H - p.gorro.M, mu = 0, sd = sqrt(p*(1-p)/n.H + p*(1-p)/n.M), alternative = "two.sided")$statistic
z.test(x = p.gorro.H - p.gorro.M, mu = 0, sd = sqrt(p*(1-p)/n.H + p*(1-p)/n.M), alternative = "two.sided")$p.value

################
## Pregunta 3 ##
################

alpha = 0.10 ## Nivel de significancia del 10%

## Los hombres tendrán mayor IMC que las mujeres? 
## Comparemos medias, con varianzas desconocidas: ¿iguales o distintas? Asumiendo Normalidad

## Ho: sigma.H = sigma.M vs Ha: sigma.H != sigma.M
var.test(Data$IMC ~ Data$SEXO, alternative = "two.sided")$p.value ## valor-p = 37.5% > 10% = alpha --> No rechazamos la igualdad de varianzas

library(dplyr)
X = filter(Data, SEXO == "MASCULINO")$IMC
Y = filter(Data, SEXO == "FEMENINO")$IMC
var.test(x = X, y = Y, alternative = "two.sided")$p.value

## A mano
aux = aggregate(IMC ~ SEXO, data = Data, FUN = var)
aux

## var.M/var.H
Fo = aux[1,2]/aux[2,2] ## Fo ~ F(n.M-1, n.H-1) y como Fo > 1
valor.p = 2*(1-pf(Fo, n.M-1, n.H-1))
cbind(Fo, valor.p)

## var.H/var.M
Fo = aux[2,2]/aux[1,2] ## Fo ~ F(n.H-1, n.M-1) y como Fo < 1
valor.p = 2*pf(Fo, n.H-1, n.M-1)
cbind(Fo, valor.p)


## Dado que las varianzas desconocidas puedenlas podemos considerar iguales
## Vamos a usar t.test
t.test(Data$IMC ~ Data$SEXO, alternative = "less", var.equal = T)$p.value ## Se puso "less" por que por abecedario Femenino esta antes de Masculino
t.test(x = X, y = Y, alternative = "great", var.equal = T)$p.value 
t.test(x = X, y = Y, alternative = "great", var.equal = T)$statistic 

## A mano
aux = aggregate(IMC ~ SEXO, data = Data, FUN = mean)
aux
mean.X = aux[2,2]
mean.Y = aux[1,2]
aux = aggregate(IMC ~ SEXO, data = Data, FUN = sd)
s.x = aux[2,2]
s.y = aux[1,2]
sp = sqrt(((n.H-1)*s.x^2+ (n.M-1)*s.y^2)/(n.H+n.M-2))
To = (mean.X-mean.Y)/(sp*sqrt(1/n.H+1/n.M)) 
To
valor.p = 1-pt(To, df = n.H+n.M-2)
valor.p

################
## Pregunta 4 ##
################

## ¿El colesterol aumenta con la edad? REGRESION LINEAL
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
## valor-p = 0.84% < 5% = alpha ---> Se rechaza Ho: b1 = 0 y apoyamos Ha: b1 != 0 --> La edad explica positivamente el Colesterol

## Interpretación:
## Por cada año de envejecimiento el colesterol aumente en 0.6651 md/dL
## Por cada decada de envejecimiento el colesterol aumente en 6.651 md/dL

## En el caso de regresión simple t.value^2 = F-statistic ---> identicos valores-p
To = summary(recta)$coefficients[2,3]
To^2

## Coeficientes de Determinación
## R2: Multiple R-squared (% de variabilidad explicada por el modelo)
## r2: Adjusted R-squared (% de variabilidad explicada por el modelo) Penaliza la complejidad del modelo

plano = lm(COLESTEROL ~ EDAD + SEXO, data = Data)
summary(plano)

## F-statistic: 5.342 (Ho: b.EDAD = b.SEXO = 0. vs Ha: Al menos un b es distinto de 0)
## Ho: No hay regresion vs Ha: Hay Regresión
## Valor.p < 5% --> Hay regresión




