##########################################
## EYP1113 - Probabilidad y Estadistica ##
##########################################

## Preparación R - Examen ##

## Carpeta de Trabajo
getwd()
dir()

## Cambiar Carpeta de Trabajo
setwd("/Users/raolea")
getwd()

## Leer Datos ##
## TXT: read.table() esta fiunción ya viene en R.
## XLSX, XLS, CSV: import() de la library(rio).

Data = read.table("ENS_Mod2.txt", header = T)
head(Data)
summary(Data)

################
## Pregunta 1 ##
################

## Utilizando un nivel de significancia del 5%
alpha = 0.05

## "Menos del 40% de la población es Hipertensa"
## Realicemos un test de proporción

## Ho: p = p0 vs Ha: p < p0
p0 = 0.40
aux = table(Data$HTA) ## Guarde como objeto la tabla
aux
n = sum(aux)
p.gorro = aux[2]/n
p.gorro ## Normal(p, sqrt(p*(1-p)/n)) ---> Zo = (p.gorro-p0)/sqrt(p0*(1-p0)/n) ~ Normal(0,1) [Aproximadamente]

## Opción 1: "a mano"
Zo = (p.gorro-p0)/sqrt(p0*(1-p0)/n)
Zo
valor.p = pnorm(Zo)            ## Ha:  <
#valor.p = 1-pnorm(Zo)          ## Ha:  >
#valor.p = 2*(1-pnorm(abs(Zo))) ## Ha: !=

valor.p ## valor.p = 3.506% < 5% = alpha ---> Se rechazas Ho y apoyamos la afimación "Menos del 40% de la población es Hipertensa." 

## Opción 2: prop.test()
prop.test(x = aux[2], n = n, p = p0, alternative = "less", correct = F)$p.value

## Opción 3: z.test()
library(TeachingDemos)
z.test(x = p.gorro, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$statistic
z.test(x = p.gorro, mu = p0, sd = sqrt(p0*(1-p0)/n), alternative = "less")$p.value

###################################
## Intervalo de Confianza para p ##
###################################

## Opción 1: "a mano"
confianza = 0.95
alpha = 1-confianza
LI = p.gorro-qnorm(1-alpha/2)*sqrt(p.gorro*(1-p.gorro)/n)
LS = p.gorro+qnorm(1-alpha/2)*sqrt(p.gorro*(1-p.gorro)/n)
cbind(LI,LS)

## Opción 2: z.test()
z.test(x = p.gorro, sd = sqrt(p.gorro*(1-p.gorro)/n), conf.level = 0.95)$conf.int

################
## Pregunta 2 ##
################

## Utilizando un nivel de significancia del 5%
alpha = 0.05

## "Los hombres son más hipertensos que las mujeres"
## Busquemos evidencia que apoye lo afirmado anteriormente comparando proporciones
## Ho: p.H = p.M = p vs Ha: p.H > p.M

## Opcion 1: "a mano"
aux = table(Data$HTA, Data$SEXO)
aux
n.M = sum(aux[,1])
n.H = sum(aux[,2])
p.gorro.H = aux[2,2]/n.H
p.gorro.M = aux[2,1]/n.M 
## Bajo Ho tenemos que Zo = (p.gorro.H-p.gorro.M)/sqrt(p*(1-p)/n.H+p*(1-p)/n.M))
p = (p.gorro.H*n.H + p.gorro.M*n.M)/(n.H+n.M)
## p = sum(aux[2,])/sum(aux) ## Alternativa
Zo = (p.gorro.H-p.gorro.M)/sqrt(p*(1-p)/n.H+p*(1-p)/n.M)
valor.p = 1 - pnorm(Zo) ## Ha: >
cbind(Zo, valor.p)
## Como el valor-p = 48.5% > 5% = alpha ---> No existe evidencia para rechazar H0 y poder apoyar lo afirmado (H > M)

## Opcion 2: prop.test()
aux = table(Data$HTA, Data$SEXO)
aux
n.M = sum(aux[,1])
n.H = sum(aux[,2])
prop.test(x = c(18, 21), n = c(n.H, n.M), correct = F, alternative = "great")$p.value

## Opcion 3: z.test()
aux = table(Data$HTA, Data$SEXO)
aux
n.M = sum(aux[,1])
n.H = sum(aux[,2])
p.gorro.H = aux[2,2]/n.H
p.gorro.M = aux[2,1]/n.M 
p = (p.gorro.H*n.H + p.gorro.M*n.M)/(n.H+n.M)
z.test(x = p.gorro.H-p.gorro.M, sd = sqrt(p*(1-p)/n.H+p*(1-p)/n.M), alternative = "great")$statistic
z.test(x = p.gorro.H-p.gorro.M, sd = sqrt(p*(1-p)/n.H+p*(1-p)/n.M), alternative = "great")$p.value

################
## Pregunta 3 ##
################

## "Los hipertensos tiene menor HDL"
hist(Data$HDL)
## Asumamos un comportamiento Normal para HDL

## Comaparemos las medias. Las varianzas son desconocidas: ¿iguales o distintas? 

## Previamente vamos a relaizar un test F. 
## X: HTA = SI
## Y: HTA = NO
## Ho: sigma.x = sigma.y vs Ha: sigma.x != sigma.y

## Opción 1: "a mano"
aux = aggregate(HDL ~ HTA, data = Data, FUN = var)
Fo = aux[1,2]/aux[2,2] ## Fo > 1 --> Nos vamos a la cola superior
## Fo = s.y^2/s.x^2 ~ F(n.y - 1, n.x - 1)
n = aggregate(HDL ~ HTA, data = Data, FUN = length)
n.x = n[2,2]
n.y = n[1,2]
valor.p = 2 * (1 - pf(Fo, df1 = n.y-1, df2 = n.x-1))
cbind(Fo, valor.p)

## Opción 2: var.test
var.test(HDL~HTA, data = Data, alternative = "two.sided")$statistic ## Fo = s.y^2/s.x^2 ~ F(n.y - 1, n.x - 1)
var.test(HDL~HTA, data = Data, alternative = "two.sided")$p.value

## Opción 3: var.test
library(dplyr)
X = filter(Data, HTA == "SI")$HDL
Y = filter(Data, HTA == "NO")$HDL

## Fo = s.y^2/s.x^2 ~ F(n.y - 1, n.x - 1)
var.test(x = Y, y = X, alternative = "two.sided")$statistic 
var.test(x = Y, y = X, alternative = "two.sided")$p.value 

## Fo = s.x^2/s.y^2 ~ F(n.x - 1, n.y - 1)
var.test(x = X, y = Y, alternative = "two.sided")$statistic 
var.test(x = X, y = Y, alternative = "two.sided")$p.value 

## Conclusion: No existe evidencia para rechazar la hipotesis que las varianzas desconocidas son iguales

## Test t para comparar mediad con varianzas desconocidas iguales
## Ho: mu.x = mu.y vs Ha: mu.x < mu.y
t.test(x = X, y = Y, var.equal = T, alternative = "less")$statistic
t.test(x = X, y = Y, var.equal = T, alternative = "less")$p.value
## Como el valor-p = 18.4% > 5% = alpha ---> No es posible rechazar Ho y apoyar la afirmación que los hipertensios tiene menor HDL que los NO hipertensos 

################
## Pregunta 4 ##
################

## A mayor edad ---> mayor colesterol

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