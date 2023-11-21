## en base a datos genera clasificaciones
## con esto podemos hcaer un boque aleatorio y podemos hacer un grafico para ver las variables mas importantes de los datos.
# tabmien puee ser utilizado como modelo analitico 
## minimo se ejecuta con 500 arboles


library(rpart) 
library(ROCR)
library(caret)
library(randomForest)
#install.packages("DMwR")

## ejemplo 1
library(randomForest)
library(dplyr)
library(PerformanceAnalytics)
library(caret)
library(rpart)
library(pROC)

## el RF es muy bueno quien le da pelea es la red neuronal

data("iris")
datos<-iris
View(datos)
tamano<-nrow(datos)
training<-round(tamano*0.7)
indices<-sample(1:tamano,size = training)

datos_train<-datos[indices,]
datos_test<-datos[-indices,]
chart.Correlation(datos_train[,-5])

help("randomForest")

modelo<-randomForest(Species~.,data=datos_train, 
                     ntree=500)

modelo
modelo$importance
varImpPlot(modelo)
plot(modelo)
getTree(modelo,1)
# hacemos las predicciones
predicciones<-predict(modelo,datos_test[,-5])
#  dos  formas
#  Errores o certezas del modelo
mc<-table(predicciones,datos_test$Species)
mc<-with(datos_test,table(predicciones,Species))
confusionMatrix(predicciones,datos_test$Species)


# para ver la efectividad del modelos
# sumamos los valores de la diagonal de la matriz entre el total de datos de la matriz
100 * sum(diag(mc)/sum(mc))

resultado <-datos_test %>% mutate(clasificacion= 
                                    predict(modelo, 
                                            datos_test[,-5])  )
View(resultado)
attach(resultado)
resultado %>% filter(Species!=clasificacion)
plot(modelo)


