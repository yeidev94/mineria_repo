#librerias
install.packages("GGally")
install.packages("caret")

library(GGally)
library(corrplot)
library(PerformanceAnalytics)
require(psych)
library(dplyr)
library(caret)

## HAY QUE HACER PARTICIONES PARA ENTRENAR EL MODELO 
## 70% para entrenar y valido con el otro 30%


#-- Regresion multiple
# cargar los datos   
#  fileEncoding = "latin1"
datos<-read.csv("files/rentadebicis.csv",fileEncoding = "latin1", header = T,sep = ",")
summary(datos)
plot(datos$rentas_totales)
plot(datos$rentas_totales, type = "b")
hist(datos$rentas_totales)


# Show both points and line
plot(datos$rentas_totales, type = "b", pch = 19, 
     col = "green", xlab = "x", ylab = "y")


# revisamos algunos variables que podrias influir en las ventas

names(datos)

View(table( cor(datos)))

# ggpairs(datos) tarda mucho

corrplot(cor (datos),method = "circle")

names(datos)
variables<-data.frame(datos$temperatura,datos$humedad,datos$sensacion_termica)
chart.Correlation(variables, histogram = TRUE, method = "pearson")
chart.Correlation(datos, histogram = TRUE, method = "pearson" ,pch="+")


# ver si hay algun tipo de relaccion
plot (datos$temperatura,datos$rentas_totales, 
      main = " influencia de la temperaturasobre las ventas",
      xlab = "temperatura",ylab = "cantidad de rentas")


c1 <- rainbow(10)
c2 <- rainbow(10, alpha=0.2)
c3 <- rainbow(10, v=0.7)


par(mfrow=c(3,5))
for (i in 1:14) {
  boxplot(datos[,i],main=names(datos)[i],col=c2, medcol=c3, whiskcol=c1, 
          staplecol=c3, boxcol=c3, outcol=c3, pch=23, cex=1)
}

##
library()
t.id<-createDataPartition(datos$rentas_totales,p=0.7,list = F)
View(t.id)

# crear el modelo de regresion multiple
## con estos datos-t.id] vamos a predecir las ventas en funcion de.
regresion <- lm(data = datos[t.id,], rentas_totales ~ 
                  hora+dia+mes+año+estacion+dia_de_la_semana+asueto+
                  temperatura+sensacion_termica+humedad+velocidad_del_viento)

# ~. es igual a en funcion para todas las variables
regresion <- lm(data = datos[t.id,-c(12,13)], rentas_totales ~. )


step(regresion)

#Intervalos de confianza para los coeficientes del modelo
confint(regresion)

#evaluar el modelo para ver cuales son las mejores variables 
#para recalibrar el modelo
summary(regresion)
step(regresion, direction = "both", trace = 1)

# se selecionar las variables y se reprocesa el modelo
regresion <- lm(data = datos[t.id,], rentas_totales ~hora+mes+año)

# el modelo no es tan exacto con los residios se pueden distribuir los residuos
library(dplyr)
boxplot (regresion$residuals)


#Prediccion con los datos de testeo 
#
prediccion<-predict(regresion,datos[-t.id,-c(12,13)])


# contruir el modelo de preduccion con datos nuevos

datos_nuevos<-data.frame(hora=1,mes=2,año=2021)
predict(regresion,datos_nuevos)

#Error Cuadrático Medio (MSE): Mide la media de los errores cuadrados entre las predicciones y 
#los valores reales. Cuanto más bajo sea el MSE, mejor será el rendimiento del modelo.
mse <- mean((regresion$residuals)^2)


sqrt(mean((regresion$fitted.values-datos[t.id,-c(12,13)]$rentas_totales)^2))

