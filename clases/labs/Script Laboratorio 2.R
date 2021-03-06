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
        paste("Funci�n: ",
              f(alpha)==100%*%(alpha^3-alpha^2)+15)))
sigma1=1.2
text(0.1,3,bquote(sigma[alpha]==.(sigma1)))
text(0.8,11,expression(sigma[alpha]==0.25))


plot(data$rent~data$area,
     xlab=expression("Tama�o del departamento [m"^2*"]"),
     ylab="Precio de renta [???]",
     main="Precio de renta por tama�o, Munich 1999")
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
data$Localizaci�n <- ifelse(data$location == 1, "Promedio",
                            ifelse(data$location == 2,"Bueno",
                                   "Alta"))

boxplot(data$rent ~ data$Localizaci�n,
        xlab="Localizaci�n",
        ylab="Renta")
title("Valor de la renta seg�n la localizaci�n")

# 3
data$Ba�o <- ifelse(data$bath==0,"Est�ndar","Premium")

table(data$Ba�o)
barplot(table(data$Ba�o))

ba�oloc=table(data$Ba�o,data$Localizaci�n)
barplot(ba�oloc)
barplot(ba�oloc,beside=TRUE,ylim=c(0,2500),
        main="Distribuci�n de Ba�os seg�n Localizaci�n",
        legend=TRUE)

# 4
data$Cocina <- ifelse(data$kitchen==0,"Est�ndar","Premium")

table(data$Cocina)
barplot(table(data$Cocina))

cocinaloc=table(data$Cocina,data$Localizaci�n)
cocinaloc
barplot(cocinaloc)
barplot(cocinaloc,beside=TRUE,ylim=c(0,2000),
        main="Distribuci�n de Cocina seg�n Localizaci�n",
        legend=FALSE)
# 5
class(data$cheating)
data$Calefacci�n <- ifelse(data$cheating==0,"No tiene","Tiene")
table(data$Calefacci�n)
boxplot(data$rentsqm~data$Calefacci�n,
        xlab="Calefacci�n",
        ylab="Renta por metro cuadrado",
        main="Renta por metro cuadrado vs \npresencia de calefacci�n")

