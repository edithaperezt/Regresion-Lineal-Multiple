#2.1 Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.
install.packages("readxl",dependencies=TRUE)
library(readxl)
po1<-sdata <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE, na=c(""))
po2<-sdata <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE, na=c(""))
dim(po1)
#La data población1 contiene 44 observaciones de 4 variables
dim(po2)
#La data población2 contiene 40 observaciones de 7 variables
#2.2 Una los archivos leídos en un mismo objeto llamado poblacion.
pobla<-match(po1$identificador,po2$identificador)
View(cbind(po1,po2[pobla,]))
poblacion1<-cbind(po1,po2[pobla,])
poblacion1
poblacion<-poblacion1[c(-24,-25,-30,-43),]
View(poblacion)
#2.3 Cree un código que identifique la clase de cada variable y genere diagramas de cajas
#para variables continuas y diagramas de barras para variables discretas.

for(i in 1:ncol(poblacion)) {
     print(class(poblacion[,i]))
}

for(i in 1:ncol(poblacion)) {
if(is.numeric(poblacion[,i])){
boxplot(poblacion[,i], xlab = "", ylab=names(poblacion)[i], main=paste("Diag cajas de",names(poblacion)[i]),
        col="steelblue", border="gray1") 
}else{
    barplot(table(poblacion[,i]), xlab = names(poblacion)[i], ylab="Frecuencia", main="Diagrama de barras",
            col="steelblue", border="gray1")  
 }
}

#2.3 Cree un código que calcule automáticamente el mínimo, media, máximo, desviación
#estándar, primer cuartil de cada variable numérica y la frecuencia en el caso de variables categóricas.



for(i in 1:ncol(poblacion)){
    if(is.numeric(poblacion[,i])){
        print( paste("El minimo, el maximo, la media,la desviacion estandar y el primer cuartil de ",names(poblacion)[i],"son"))
              print( c(min(poblacion[,i]),
                       max(poblacion[,i]),
                       mean(poblacion[,i]),
                       sd(poblacion[,i]),
                       
                       
                       quantile(poblacion[,i], probs = seq(0, 1, 0.25), na.rm = FALSE)))
    }else{
        print(paste("La frecuencia de ",names(poblacion)[i]))
        print(table(poblacion[,i]))
    }
}

#2.4 Calcule la correlación entre la variable dependiente poblacion y cada una de las
#variables explicativas (numéricas).
r<-numeric(ncol(poblacion))
for(i in 1:ncol(poblacion)){
    if(is.numeric(poblacion[,i])){
    r[i]<-cor(poblacion$poblacion,poblacion[,i])
  
    }
}
r
#2.5 Considere la variable categórica serv.bas.compl con una confiabilidad del 90%, ¿Puede asumirse que la media de la variable poblacion en el grupo serv.bas.compl: SI
#es distinta a la media del grupo serv.bas.compl: NO ? Utilice la función:


sb1<-subset(poblacion,subset=serv.bas.compl=="SI")
s1<-sb1$poblacion
s1

sb2<-subset(poblacion,subset=serv.bas.compl=="NO")
s2<-sb2$poblacion
s2
t.test(s1, s2, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.90)
#como qt=1.685954<t, no se rechaza la hipotesis, por tanto las medias son
#iguales, por tanto no hago 2 modelos
#2.6 Considerando los cálculos anteriores genere el modelo de regresión lineal múltiple que
#mejor se ajuste a los datos. Interprete los coeficientes obtenidos.
clase<-sapply(X = poblacion,FUN = class)
clase
data<-poblacion[,clase=="numeric"]
View(data)
datanum<-data[,c(-1,-5)]
View(datanum)
reg1 <- lm(poblacion~.,datanum)
summary(reg1)
regf<-step(object = reg1)
summary(regf)
regfinal<-lm(poblacion~tasa.crimen,data = datanum)
summary(regfinal)

#2.7 Interprete el R2.
# Nuestra regresión explica el 14% de la variabilidad total.

#2.8 Analice la significancia de la regresión y de cada uno de los parámetros individuales.
#Para la significancia de los parametros usamos
#la prueba t de Student
#> qt(p = 0.975,df = 38)
#[1] 2.024394
#Para B1
#Como t=11.147>t(alfa/2)=2.026192, el parametro B1 es significativo.
#Para B2
#Como t=2.493>t(alfa/2)=2.026192, el parametro B2 es significativo.
#Ahora para la significancia de la regresion utilizamos la prueba
#de Fisher
anova<-aov(regfinal)
summary(anova)
#> qf(p = 0.95,df1 = 1,df2 = 38)
#[1] 4.098172
#Como F=6.215 >F(alfa)=4.098172, entonces la regresion
#es significativa


#2.9 Realice un análisis detallado de los residuos.
library(ggplot2)
u_t<-regfinal$residuals
g<-ggplot(data=poblacion,aes(x=tasa.crimen,y=u_t))
g + geom_point()

#graf normal
qqnorm(u_t)
qqline(u_t)
