


library(dplyr)
library(foreign)
library(TeachingDemos)
# library(rio) Función import():

### comandos utiles 
data = read.table("PCR.txt", header = T, dec = ",")
head(data)
summary(data)
X = filter(data, PCR == "1" )$EDAD
Y = filter(data, PCR == "0" )$EDAD
t.test(x = X, y = Y, var.equal = T, alternative = "g")$p.value





n=length(X)+length(Y)
p_ = length(X)/n
#Bernulli p_ es p.gorro
p0=1/3
z= (p_-p0)/sqrt(p0*(1-p0)/n)
1-pnorm(z)



















































