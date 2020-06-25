###########################
###                     ###
###   Laboratorio 05    ###
###                     ###
###########################

# Múltilpes variables aleatorias #
Sexo=c(rep(0,1+4+2+1+1),rep(1,1+3+1+1+3+2))
Sexo
Edad=c(rep(9,1),rep(11,4),rep(12,2),rep(13,1),rep(14,1),
       rep(9,1),rep(10,3),rep(11,1),rep(12,1),rep(13,3),rep(14,2))
Edad
data=data.frame(Sexo=Sexo,Edad=Edad)
data
tabla=table(data)
tabla

# 1
?prop.table
Funcion.Prob.Conjunta=prop.table(tabla)
Funcion.Prob.Conjunta

# 2
P.B=sum(Funcion.Prob.Conjunta["1",c("10","12","14")])
sum(Funcion.Prob.Conjunta[2,c(2,4,6)])
mean(data$Edad%%2==0 & data$Sexo==1)
P.B

# 3
Funcion.X=apply(Funcion.Prob.Conjunta,1,sum)
Funcion.X

# 4
Funcion.Y=apply(Funcion.Prob.Conjunta,2,sum)
Funcion.Y

# 5
Funcion.Y.dado.X=prop.table(tabla,1)
Funcion.Y.dado.X

# 6
# Podemos probar si P(X=0,Y=9)=P(X=0)*P(Y=9)
Funcion.Prob.Conjunta["0","9"]==Funcion.X["0"]*Funcion.Y["9"]
# No son independientes

# 7
Costo=function(x,y){
  costo=ifelse(x==1,3000+150*y,2500+120*y)
  return(costo)
}
Costo(data$Sexo,data$Edad)
mean(Costo(data$Sexo,data$Edad))


# Poisson Binomial
p=0.6
lambda=15
p.x.y=function(x,y){
  p.conjunta=ifelse(x<=y,dbinom(x,y,p)*dpois(y,lambda),0)
  return(p.conjunta)
}

x=seq(0,30)
y=seq(0,30)
z=outer(x,y,p.x.y)
X=rep(x,times=length(y))
Y=rep(y,each=length(x))
Z=c(z)

library(scatterplot3d)
scatterplot3d(X,Y,Z,type="h",lwd=2,pch=" ",xlab="X",ylab="Y",zlab=expression(P(X==x,Y==y)),
              highlight.3d = TRUE,angle=45)


# Funcion conjunta de dos variables continuas
alpha=5
beta=2
f.x.y=function(x,y){
  densidad=alpha*beta*exp(-alpha*x-beta*y)
  return(densidad)
}
x=seq(0.01,5,0.01)
y=seq(0.01,2,0.01)
z=outer(x,y,f.x.y)

library(rgl)

rgl.surface(x=x,y=z,z=y,color="red",back="lines")


X=rep(x,times=length(y))
Y=rep(y,each=length(x))
Z=c(z)

scatterplot3d(X,Y,Z,type="h",lwd=2,pch=" ",xlab="X",ylab="Y",zlab=expression(F(X==x,Y==y)),
              highlight.3d = TRUE,angle=45)


# Análisis de regresión #
panel.cor=function(x, y, ...){
  par(usr = c(0, 1, 0, 1))
  txt=as.character(format(cor(x, y), digits=2))
  text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}
?iris
pairs(iris[1:4], upper.panel=panel.cor)


# Normal bivariada #

f.xy = function(x, y, mu.x = 0, mu.y = 0, s.x = 1, s.y = 1,
                rho = 0){
  n.r = length(x)
  n.c = length(y)
  M = matrix(NA, ncol = n.c, nrow = n.r)
  for(i in 1:n.r){
    M[i,] = dnorm(x[i], mean = mu.x, sd = s.x) * dnorm(y, mean = mu.y
                                                       + rho*s.y*(x[i]-mu.x)/s.x, sd = s.y*sqrt(1-rho^2))
  }
  M
}
x = seq(-5,5,0.1)
y = seq(-5,5,0.1)
z = f.xy(x, y, rho = 0)
rgl.surface(x = x, y = z*10, z = y, color = "red", back="lines")


# Aplicación #
# 1
data=iris[,c(1:4)]
data
pairs(data,upper.panel = panel.cor)


# 2
colnames(data)
X=data[,4]
Y=data[,3]

beta=sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)
beta
alpha=mean(Y)-beta*mean(X)
alpha
cbind(alpha,beta)


# 3
plot(Y~X,pch=20,bty="n",lwd=5,las=1,xlab="Ancho del pétalo",ylab="Largo del pétalo")
abline(a=alpha,b=beta,lwd=3,col="red",lty=2)


# 4 
n=length(X)
S.y.x=sqrt((sum((Y-mean(Y))^2)-beta^2*sum((X-mean(X))^2))/(n-2))
S.y.x
S.y=sqrt(var(Y))
S.y
cbind(S.y.x,S.y)
r2=1-S.y.x^2/S.y^2
r2

# 5
lm(Y~X)
summary(lm(Y~X))



