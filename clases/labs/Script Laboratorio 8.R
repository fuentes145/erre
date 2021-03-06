
##################
# Laboratorio 08 #
##################

# Package rio
install.packages("rio")
library(rio)
data=import(file.choose())
head(data)
?import

# Package ggplot2
install.packages("ggplot2")
library(ggplot2)
?mpg

names(mpg)

# manufacturer = marca/fabricante
table(mpg$manufacturer)
length(cbind(table(mpg$manufacturer)))
# model = modelo
table(mpg$model)
length(cbind(table(mpg$model)))
# displ = desplazamiento del motor en litros
summary(mpg$displ)
boxplot(mpg$displ)
hist(mpg$displ)
# year = a�o de fabricaci�n 
table(mpg$year)
# cyl = n�mero de cilindros
table(mpg$cyl)
# trans = tipo de transmisi�n
table(mpg$trans)
length(cbind(table(mpg$trans)))
# drv = tracci�n (f=delantera,r=trasera,4=cuatro ruedas)
table(mpg$drv)
# cty = millas por gal�n en la ciudad 
summary(mpg$cty)
# hwy = millas por gal�n en la carretera
summary(mpg$hwy)
# fl= tipo de combustible
table(mpg$fl)
# class = tipo del veh�culo
table(mpg$class)


# Gr�ficos de barra 
barplot(table(mpg$manufacturer))
qplot(manufacturer,data=mpg,geom="bar",fill=manufacturer)

barplot(table(mpg$trans))
qplot(trans,data=mpg,fill=trans,geom="bar")

# Gr�ficos de puntos
plot(mpg$cty~mpg$displ)
ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy,size=hwy,
                 shape=drv))+
  labs(x="Desplazamiento",y="Millas por gal�n en la ciudad",
       color="Millas por gal�n \nen la carretera",
       size="Millas por gal�n \nen la carretera",
       shape="Tracci�n")

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy),size=8,alpha=0.5)+
  labs(x="Desplazamiento",y="Millas por gal�n en la ciudad",
       color="Millas por gal�n \nen la carretera")+
  theme_minimal()

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(size=8,alpha=0.5,col="blue")+
  labs(x="Desplazamiento",y="Millas por gal�n en la ciudad",
       color="Millas por gal�n \nen la carretera")+
  theme_minimal()

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy,size=hwy,
                 shape=drv))+
  labs(x="Desplazamiento",y="Millas por gal�n en la ciudad",
       color="Millas por gal�n \nen la carretera",
       size="Millas por gal�n \nen la carretera",
       shape="Tracci�n")+
  guides(colour=guide_colourbar(order=1),
         shape=guide_legend(order=2),
         size=guide_legend(order=3))

colours <- list(~class,~drv,~fl)
for(colour in colours){
  print(ggplot(mpg,aes_(~displ,~cty,colour=colour))+
          geom_point())
}


ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=class),show.legend = FALSE,
             alpha=0.5)+
  theme_bw()+
  facet_wrap(~class)

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=class),show.legend = FALSE,
             alpha=0.5)+
  theme_bw()+
  geom_smooth(aes(linetype=drv,color=drv),method="lm")
?geom_smooth

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=class),show.legend = FALSE,
             alpha=0.5)+
  theme_bw()+
  geom_smooth(method="lm")

# Boxplot
boxplot(mpg$hwy~mpg$class)  

ggplot(data=mpg,aes(x=class,y=hwy))+
  geom_boxplot(colour="grey50")+
  geom_jitter()

ggplot(data=mpg,aes(x=reorder(class,-hwy),y=hwy))+
  geom_boxplot(colour="grey50")+
  geom_jitter()
