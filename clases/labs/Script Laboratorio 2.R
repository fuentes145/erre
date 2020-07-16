######################
### Laboratorio 02 ###
######################

getwd()
# install.packages("foreign")
library(foreign)
data <- read.dta("RentasMunich.dta")
# data <- read.dta(file.choose())
colors()

par(mar=c(5.1,5.1,4.1,2.1))
curve(100*(x^3-x^2)+15, 0, 1,
      xlab=expression(alpha),
      ylab=expression(100%*%(alpha^3-alpha^2)+15),
      main=expression(
        paste("Función: ",
              f(alpha)==100%*%(alpha^3-alpha^2)+15)))
sigma1=1.2
text(0.1,3,bquote(sigma[alpha]==.(sigma1)))
text(0.8,11,expression(sigma[alpha]==0.25))


plot(data$rent~data$area,
     xlab=expression("Tamaño del departamento [m"^2*"]"),
     ylab="Precio de renta [???]",
     main="Precio de renta por tamaño, Munich 1999")
#plot(x=data$area,y=data$rent)


# 1 
boxplot(data$rentsqm)
max(data$rentsqm)
min(data$rentsqm)
summary(data$rentsqm)
boxplot(data$rentsqm,horizontal = TRUE)
boxplot(data$rentsqm,main="Boxplot de Renta por metro cuadrado")

hist(data$rentsqm,
     main="Histograma de Renta por metro cuadrado, \nMunich 1999",
     xlab="Renta por metro cuadrado",
     ylab="Frecuencia",col="blue")

# 2 
data$Localización <- ifelse(data$location == 1, "Promedio",
                            ifelse(data$location == 2,"Bueno",
                                   "Alta"))

boxplot(data$rent ~ data$Localización,
        xlab="Localización",
        ylab="Renta")
title("Valor de la renta según la localización")

# 3
data$Baño <- ifelse(data$bath==0,"Estándar","Premium")

table(data$Baño)
barplot(table(data$Baño))

bañoloc=table(data$Baño,data$Localización)
barplot(bañoloc)
barplot(bañoloc,beside=TRUE,ylim=c(0,2500),
        main="Distribución de Baños según Localización",
        legend=TRUE)

# 4
data$Cocina <- ifelse(data$kitchen==0,"Estándar","Premium")

table(data$Cocina)
barplot(table(data$Cocina))

cocinaloc=table(data$Cocina,data$Localización)
cocinaloc
barplot(cocinaloc)
barplot(cocinaloc,beside=TRUE,ylim=c(0,2000),
        main="Distribución de Cocina según Localización",
        legend=FALSE)
# 5
class(data$cheating)
data$Calefacción <- ifelse(data$cheating==0,"No tiene","Tiene")
table(data$Calefacción)
boxplot(data$rentsqm~data$Calefacción,
        xlab="Calefacción",
        ylab="Renta por metro cuadrado",
        main="Renta por metro cuadrado vs \npresencia de calefacción")

