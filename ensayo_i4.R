###DISTRIBUCION BERNULLI


n1 = 22
p1 = 7/n1
n2 = 16
p2 = 8/n2
p = (7+8)/(n1+n2)
Z0 = (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Z0## A mano con Rpnorm(Z0) ## No rechaza Ho## Con z.test
z.test(x = (p1-p2), mu = 0, sd = sqrt(p*(1-p)*(1/n1+1/n2)), alternative = "less")$p.value
pnorm(Z0)
X=1
Y=1

mu.x = 4.5
sx = 0.7
n1 = 16
sy = 0.5
mu.y = 4.2
n2 = 22
Sp = sqrt(((n1-1)*sx**2+(n2-1)*sy**2)/(n1+n2-2)) 
T0 = (mu.x-mu.y)/(Sp*sqrt(1/n1+1/n2))
1-pt(T0, df = n+m-2)




Z = (X-Y)-(p.x-p.y))/ sqrt( ((p.x*(1-p.x))/n)+((p.y*(1-p.y))/m) 
1-pnorm(abs(Z))

F0 = (0.7/0.5)^2
alpha = 0.05
qf(1-alpha/2, df1 = 22, df2 = 16)






install.packages("rio")
library("rio")
X = import("Radiata.xlsx")
Y = lm(Branches_area_cm2 ~Diameter_basal_cm, data= X)
Z = lm(Branches_area_cm2~., data= X)
summary(Y)
summary(Z)

anova(Y,Z)






M = rbind(c(6,18,32,30,14))
colnames(M) = c("[0 - 15)", "[15 - 25)", "[25 - 40)", "[40 - 60)", "[60, Inf)")
rownames(M) = "Tiempos"
## Valores Observados
O = c(M)
## Estimador de Momento k y nu
mu1 = 36
mu2 = 99*(20^2)/100+mu1^2
nu = mu1/(mu2-mu1^2)
k = mu1^2/(mu2-mu1^2)
p = diff(pgamma(c(0,15,25,40,60,Inf), shape = k, rate = nu))
X2 = chisq.test(x = O, p = p)$statistic
valor.p = 1-pchisq(X2, df = 5-1-2)
M = cbind(X2, valor.p)
## Estimador a partir de c.o.v. y mean de k y nu
k = (20/36)^-2
nu = k/36
p = diff(pgamma(c(0,15,25,40,60,Inf), shape = k, rate = nu))
X2 = chisq.test(x = O, p = p)$statistic
valor.p = 1-pchisq(X2, df = 5-1-2)
M = rbind(M,cbind(X2, valor.p))
M

x5=pgamma(Inf, shape= 36*(3/10)**2, rate= (3/10)**2)-pgamma(60, shape= 36*(3/10)**2, rate= (3/10)**2)


proporciones = c(x1,x2,x3,x4,x5)
muestra = c(6,18,32,30,14)
chisq.test(muestra , p = proporciones)$statistic

k = 5
real = c(x1,x2,x3,x4,x5)*100
muestra = c(6,18,32,30,14)

chic = sum(((muestra-real)**2)/real)
nu = k - 1

# chi critico al 95%
chi_critico = qchisq(1-0.0599, nu)
#si es false no rechaso 
chic > chi_critico

p = pchisq(5.76931 , 2)
1-p





library(MASS)
library(dplyr)
View(anorexia)
Xpr = filter(anorexia,  Treat== "FT")$Prewt
Xpo = filter(anorexia, Treat == "FT")$Postwt
Ypr = filter(anorexia,  Treat== "Cont")$Prewt
Ypo = filter(anorexia,  Treat== "Cont")$Postwt
X = Xpo -Xpr
Y = Ypo - Ypr
sd(X)
var.test(X~Y, alternative = "two.sided")$p.value
t.test(x = X, y = Y, var.equal = T, alternative = "g")$p.value

x_ = mean(anorexia$Prewt)
xsd = sd(anorexia$Prewt)
dnorm(, x_, sd)
ks.test(x = X, y = pnorm(x_, xsd))





confianza=0.99
p=14600000/18000000
w=0.05

sd= sqrt(p*(1-p))
alfa = (1-confianza)/2
k_1= qnorm(1-alfa/2)

tamaño= ((k_1*sd)/w)**2
tamaño

library(MASS)
fitdistr(x= 
, densfun= "normal")





library(rio)
data = import("VIENTO.xlsx")

mean(data$VELOCIDAD)
fitdistr(x = data$VELOCIDAD, densfun = "Weibull")$estimate

qweibull(0.5,3.693466,9.511309)
qlnorm(0.5,2.0837975,0.4009483)

head(muscle)
sd(muscle$Length)
var.test(x = X, y = Y, alternative = "two.sided")$p.value 
c= (length(muscle$Length)-1)*sd(muscle$Length)**2 / 60
pgamma(c, (length(muscle$Length)-1)/2, 1/2)

X=38.5
mu= 40
sd=
n=12
S=2.5
###distribuciones normales

Z = (X - mu) / (sd/sqrt(n))
#pnorm(Z,0,1)

T = (X-mu) / (S/sqrt(n))
pt(T, n-1)

sd(rt(2000000, 5.202111))
sd(qt(x,5.239538)

X=4.0
mu= 4.2
sd= sqrt(1.2)
n=22
S=
###distribuciones normales

Z = (X - mu) / (sd/sqrt(n))
1-pnorm(abs(Z),0,1)

confianza=0.95
p_=300/14600000
n=300

alfa = 1-confianza
k_1= qnorm(1-alfa/2)
w = k_1*sqrt(p_*(1-p_)/n) *100
w

