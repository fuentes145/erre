
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
# year = año de fabricación 
table(mpg$year)
# cyl = número de cilindros
table(mpg$cyl)
# trans = tipo de transmisión
table(mpg$trans)
length(cbind(table(mpg$trans)))
# drv = tracción (f=delantera,r=trasera,4=cuatro ruedas)
table(mpg$drv)
# cty = millas por galón en la ciudad 
summary(mpg$cty)
# hwy = millas por galón en la carretera
summary(mpg$hwy)
# fl= tipo de combustible
table(mpg$fl)
# class = tipo del vehículo
table(mpg$class)


# Gráficos de barra 
barplot(table(mpg$manufacturer))
qplot(manufacturer,data=mpg,geom="bar",fill=manufacturer)

barplot(table(mpg$trans))
qplot(trans,data=mpg,fill=trans,geom="bar")

# Gráficos de puntos
plot(mpg$cty~mpg$displ)
ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy,size=hwy,
                 shape=drv))+
  labs(x="Desplazamiento",y="Millas por galón en la ciudad",
       color="Millas por galón \nen la carretera",
       size="Millas por galón \nen la carretera",
       shape="Tracción")

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy),size=8,alpha=0.5)+
  labs(x="Desplazamiento",y="Millas por galón en la ciudad",
       color="Millas por galón \nen la carretera")+
  theme_minimal()

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(size=8,alpha=0.5,col="blue")+
  labs(x="Desplazamiento",y="Millas por galón en la ciudad",
       color="Millas por galón \nen la carretera")+
  theme_minimal()

ggplot(data=mpg,aes(x=displ,y=cty))+
  geom_point(aes(color=hwy,size=hwy,
                 shape=drv))+
  labs(x="Desplazamiento",y="Millas por galón en la ciudad",
       color="Millas por galón \nen la carretera",
       size="Millas por galón \nen la carretera",
       shape="Tracción")+
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
