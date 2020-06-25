###########################
###                     ###
###   Laboratorio 04    ###
###                     ###
###########################

# Hipergeom?trica 
# En un lote de tama?o N tengo m objetos defectuosos y N-m que no son defectuosos
# obtengo una muestra aleatoria de tama?o n y luego 
# la probabilidad de que x objetos sean defectuosos est? dada por 
# la funci?n de probabilidad de la distribuci?n hipergeom?trica

# X: cantidad de objetos defectuosos de la muestra
# X={0,1,...,min(m,k)}

# En R se define como una urna con m bolas blancas y n bolas negras
# Se realiza una extracci?n de tama?o k y
# x representa el n?mero de bolas blancas extra?das

# dhyper(x,m,n,k)
# phyper(q,m,n,k)
# qhyper(p,m,n,k)
# rhyper(nmuestra,m,n,k)

# Media teorica = k*p, con p=m/(m+n)
# Varianza teorica = k*p*(1-p)*(m+n-k)/(m+n-1)

# Hay una urna con 17 bolas blancas y 23 negras,
# si se extraen 15 bolas al azar,
# ?Cu?l es la distribuci?n de las bolas blancas extra?das?

# X~Hipergeometrica(m=17,n=23,k=15)

# Vamos a simular una muestra aleatoria de tama?o n=120

set.seed(1113)
tam=120
m=17
n=23
k=15
X=rhyper(tam,m,n,k)

barplot(table(X))

# Medidas centrales muestrales 

# Media muestral
mean(X)
# Media te?rica 
p=m/(m+n)
k*p

# Moda muestral
filtro=(table(X)==max(table(X)))
table(X)[filtro]

# Mediana muestral
median(X)
# Mediana te?rica
qhyper(0.5,m,n,k)

# Esperanza de g(X)=X^2
g=function(X){X^2}
mean(g(X))

# Percentiles muestrales
quantile(X,seq(from=0.1,to=0.9,by=0.1))
# Percentiles te?ricos 
qhyper(seq(from=0.1,to=0.9,by=0.1),m,n,k)

# Varianza muestral
var(X)
# Varianza te?rica 
k*p*(1-p)*(m+n-k)/(m+n-1)

# Rango muestral
Rango=max(X)-min(X)
Rango
Rango=function(X){max(X)-min(X)}
Rango(X)
range(X)[2]-range(X)[1]

# Rango intercuart?lico muestral
IQR=function(X){quantile(X,0.75)-quantile(X,0.25)}
IQR(X)
# Rango intercuart?lico te?rico
qhyper(0.75,m,n,k)-qhyper(0.25,m,n,k)

# Desviaci?n est?ndar muestral
sd(X)
# Desviaci?n est?ndar te?rica
sqrt(k*p*(1-p)*(m+n-k)/(m+n-1))

# Coeficiente de variaci?n muestral
sd(X,na.rm=TRUE)/mean(X,na.rm=TRUE)
# Coeficiente de variaci?n te?rico
sqrt(k*p*(1-p)*(m+n-k)/(m+n-1))/(k*p)

# Skewness muestral
Skewness=function(x){
  n=length(x)
  sum((x-mean(x))^3/n)/sd(x)^3
}
Skewness(X)

# Kurtosis muestral
Kurtosis=function(x){
  n=length(x)
  sum((x-mean(x))^4/n)/sd(x)^4-3
}
Kurtosis(X)


# Para lo ?ltimo necesitamos dos variables
# Y ~ Binomial(k,p)

set.seed(1113)
Y=rbinom(120,k,p)
barplot(table(Y))
plot(X,Y)

# Covarianza muestral
cov(X,Y)

# Correlaci?n muestral
cor(X,Y)
cov(X,Y)/(sd(X)*sd(Y))
