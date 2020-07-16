###########################
### Prueba de Hipotesis ###
###########################

## Aplicación Encuesta Nacional de Salud 2009 ##

## Supongamos que esta data es la Población
Data  = read.table("ENS2009.TXT", dec = ",", header = T)
n = dim(Data)[1] ## Cantidad de Datos 

## Obtenganmos una muestra aleatoria correspondiente al 10%
Muestra = Data[sample(1:n, 0.1*n),] 
n = dim(Muestra)[1]

## Pregunta:
## ¿Los mayores de 15 tienen un colesterol menor a 200 mg/dL? 
X = Muestra$COLESTEROL
## 1. Supongamos que X ~ Normal(mu, sigma), con sigma conocido e igual a 42.64 (Data Población)
## 2. Hipótesis ---> H0: mu = mu0 vs Ha: mu < mu0, con mu0 = 200
## 3. Pivote bajo H0: Z0 = (promedio-mu0)/(sigma/sqrt(n)) ~ N(0,1)
## 4. Nivel de significancia de prueba: alpha = 5%
mu0 = 200
sigma = 42.64 ## No es usual conocer sigma, pero para efectos de este ejercicio lo conocemos
n = length(X)
Z0 = (mean(X)-mu0)/(sigma/sqrt(n))

## Graficquemos la distribución bajo H0 del pivote
z = seq(-4,4,.001)
plot(dnorm(z)~z, type = "l", bty = "n", las = 1, main = expression("Distribución del Estadistico de Prueba Bajo H0"), xlab = "", ylab = "")
abline(v = Z0, lty = 2)
axis(1, Z0, label = expression(Z[0])) ## Rechazamos H0?
## ¿Cual es la zona de rechazo al alpha x 100%?
## Necesitamos el percentil: Percentil 5%
z = seq(-4,qnorm(0.05),.001)
lines(dnorm(z)~z, type = "h", col = "steelblue")
axis(1, qnorm(0.05), label = expression(k[0.05]))
## Vemos que el Z0 se encuentra en la zona de rechazo 
## Por lo tanto, existe suficiente evidencia para rechazar H0 en favor de Ha, 
## es decir, podemos apoyar la hipotesis que el colesterol de los mayores 
## de 15 años es inferior a 200 mg\dl

## El valor-p? 
z = seq(-4,Z0,.001)
lines(dnorm(z)~z, type = "h", col = "gray") ## área gris corresponde al valor-p
z = seq(-4,4,.001)
lines(dnorm(z)~z, lwd = 2)
abline(v = Z0, lty = 2)
## como se aprecia en este caso el valor-p < alpha ---> Rechazamos H0

###########################################
## Calculo del Valor-p para distintos Ha ##
###########################################

## Valor-p cuando Ha: < ("less")
pnorm(Z0) 
## Valor-p cuando Ha: > ("greater")
1-pnorm(Z0) 
## Valor-p cuando Ha: != ("two.sided")
2*(1-pnorm(abs(Z0)))

#############################
## Test de Hiótesios con R ##
#############################

library(TeachingDemos)

## Test utilizando datos, mu0 y sigma conocido
z.test(x = X, mu = mu0, sd = sigma, alternative = "less")$statistic
z.test(x = X, mu = mu0, sd = sigma, alternative = "less")$p.value
z.test(x = X, mu = mu0, sd = sigma, alternative = "less")
## Opciones para alternative: "less" (<), "greater" (>), "two.sided" (!=)

## Test utilizando resumen de los datos (n y promedio), mu0 y sigma conocido
promedio = mean(X)
z.test(x = promedio, n = n, mu = mu0, sd = sigma, alternative = "less")$statistic
z.test(x = promedio, n = n, mu = mu0, sd = sigma, alternative = "less")$p.value