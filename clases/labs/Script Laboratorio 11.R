
##################
# Laboratorio 11 #
##################

View(iris)


# 1 #
modelo1=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris)

data=iris[,c(1:4)]
modelo2=lm(Sepal.Length~.,data=data)

modelo1
modelo2


# 2 # intervalo de confianza para el petal.whith que es el cuarta fila de la matriz lm.coeficients
summary(modelo1)
aux=summary(modelo1)
aux$coefficients[4,1]
beta3=aux$coefficients[4,1];beta3
se3=aux$coefficients[4,2]
alpha=0.05
n=dim(data)[1]
k=dim(data)[2]-1
t3=qt(1-alpha/2,n-(k+1));t3
c(beta3-se3*t3,beta3+se3*t3)

# 3 # para clacular el estadistico F necesito anova que me da lo scr y sce
anova(modelo1)
aux=anova(modelo1)
SCR=sum(aux$`Sum Sq`[1:k])
SCE=aux$`Sum Sq`[k+1]
SCT=SCR+SCE
F=(SCR/k)/(SCE/(n-(k+1)));F
qF=qf(1-alpha,k,n-(k+1));qF
summary(modelo1)

# 4 # r ² y r² ajustado con el sumary se ssacan altoque pero aca esta manual
r2=SCR/SCT;r2
r2a=1-(1-r2)*(n-1)/(n-(k+1));r2a
