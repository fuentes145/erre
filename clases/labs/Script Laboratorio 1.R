### Laboratorio 1 ###

# Ejercicio 1
11+8+2016
3-(2-(1-(15/5*3))-3**2)

# Ejercicio 2
sqrt(1082016)
log(235)
exp(0)
factorial(85)
sin(pi)/cos(2*pi)

# Ejercicio 3
a <- "Laboratorio EYP1113"
b <- 3*6+9/7
d <- sqrt(9)/log(10)
a+b
a*d
b/d
d-b

ls()
objects()

rm("c")
ls()

# Ejercicio 4 
ls()
rm("a")
ls()

getwd()
# setwd("")
save.image("objetos")
rm(list=ls())
ls()
load("objetos")
ls()

z <- seq(from=-3,to=81,by=2)
z
largo.z <- length(z)
elemento2 <- z[2]
1:8

# Ejercicio 5 
1:3
z[1:3]

# Ejercicio 6
c(1,2,3)+c(4,5,6,7)

9<=6
c(2,5,8)>c(9,8,7)


# Ejercicio 7
m1 <- matrix(c(1,2,3,4),ncol=2,byrow=FALSE)
m2 <- matrix(c(1,2,3,4),ncol=2,byrow=TRUE)
m1
m2
diag(m1)
diag(m2)
m1*m2
m1%*%m2
dim(m1)
dim(m2)
ncol(m1)
nrow(m1)
t(m1)
det(m1)
solve(m1)

getOption("defaultPackages")

# Para instalar paquetes install.packages("Nombre")
# Para cargar paquetes library("Nombre")


datos <- read.table(file.choose(),
                    header = TRUE)
datos
head(datos)
tail(datos)
dim(datos)
colnames(datos)
names(datos)
attach(datos)

datos <- read.table(file.choose(),
                    header = TRUE, dec=",")
attach(datos)
class(datos$Temperatura_Maxima)
class(datos$Juega_Tenis)
as.factor(datos$Juega_Tenis)
class(datos$Juega_Tenis)
datos$Juega_Tenis <- as.factor(datos$Juega_Tenis)
class(datos$Juega_Tenis)

# Ejercicio 8
colnames(datos)
summary(datos$Pronostico)
table(datos$Pronostico)
min(datos$Temperatura_Minima)
summary(datos$Temperatura_Maxima)
summary(datos$Temperatura_Minima)


