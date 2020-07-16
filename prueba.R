lambda = 1.2 
zeta = 0.3
Y = rlnorm(n=10, meanlog=lambda ,sdlog=zeta)
Y^2
zeta.em = sqrt(log(mean(Y^2)-2*log(mean(Y))))
lambda.em=log(mean(Y))-(1/2)*zeta.em^2
lambda.em

N=580
n= 8
m= 0.15*580
dhyper(1, m=m, n=N-m, k=n)


des =6
qnorm(1-0.1216725, 33,des)

0.70*0.25+0.3*0.7

dpois(4, 8/3)

(1/20-(1/20)^3)*20^3
1-pnorm(2/(5/sqrt(10)), 0, 1)


mu = 60
ed = 15 
delta = ed/mu
zeta=sqrt(log(1+delta^2))
lambda= log(mu) - (1/2)* zeta^2

qlnorm(0.75, meanlog = lambda, sdlog = zeta)-qlnorm(0.25, meanlog = lambda, sdlog = zeta)



library(foreign)
data = read.table("SALMONES.txt", header = T)
head(data)
names(data) =NULL
typeof(data)
 data$JAULA 
dat=attach(data)
dat
summary(data)
c=0
for(i in data$JAULA ) {if (i== "A"){c= c+1} }
c
rm(list = ls())

install.packages("dplyr")
library("dplyr")
A = filter(data, JAULA == "A" )$PESO
summary(A)
ka = (sd(A) /mean(A))^-2
ka
B = filter(data, JAULA == "B" )$PESO
kb = (sd(B) /mean(B))^-2
kb
nub = (mean(B)/kb)^-1



