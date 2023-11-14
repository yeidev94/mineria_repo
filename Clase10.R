install.packages("FactoMineR")

# el acp hace sentido cuando son muchas variables que se desean resumir
# las observaciones deben ser numericas
# solo hace sentido realizarlo si hay una correlacion alta entre las variales originales

datos<-read.csv("files/WholesaleCustomersData.csv",sep = ";",header = T)
View(datos)

# Preparar los datos
# Diagramas de caja para ver valores outlayer o fuera  de rando
# revisar si tenemos valores nulos o valores ausentes
# revisamos la funcion summary para ver la descripcion
# la funcion pairs para ver la distribucion de los dato s
# y el histograma para ver la distribucion de los datos 
# revisar la correlacion de los datos DEBE SER ALTA

#partimos la pantala en fila columna
par(mfrow=c(1,2))  #con esto dividimos el área 

datos<-na.omit(datos)

boxplot(datos$Milk,col=terrain.colors(4), 
        main = "total sales",horizontal = TRUE,
        boxwex = 0.5)$out
stripchart(datos$Milk, method = "jitter", pch = 18, add = TRUE, col = "blue")


boxplot(Fresh ~  Channel , notch = TRUE, 
        data = datos,horizontal = TRUE,
        main = "Sales for Month"
)

# para probar y ver todos los datos
par(mfrow=c(3,3))
c3<-c("red","blue")
for (i in 2:9) {
  boxplot(datos[,i],main=names(datos)[i],col="blue",medcol="red",
          pch=23, cex=1.5,whiskcol="red",staplecol=c3, boxcol=c3, outcol=c3)
}

#Como hacer funciones
vector<-c(1,2,3,4,5)
prueba <- function(x){
  y<-x*3
  y
}
prueba(vector)


# creamos una  funcion para limpiar los datos 
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.20, 0.80), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}



imputed_data <- impute_outliers(datos$Fresh)
datos$FreshImput<-impute_outliers(datos$Fresh)
View(datos)

boxplot(datos$Fresh)
boxplot(datos$FreshImput)


#hist(datos$Milk)
hist(datos$Milk, freq=FALSE,xlab = "Leche", ylab = "Frecuencia",
     main = "Histograma de datos de leche",col = "steelblue" )
curve(dnorm(x, 
            mean=mean(datos$Milk), 
            sd=sd(datos$Milk)), 
      add=TRUE, col="red")

# hacer un histograma en ggplot2
library(ggplot2)
ggplot(data = datos,
       mapping = aes(x = Milk)) +
  geom_histogram(bins = 9)

ggplot(datos, aes(Milk)) +
  geom_density(alpha = 0.5)

summary(datos)
pairs(datos)

# para ver donde hay valores NA
which( is.na(datos))

# vamos a la descretizacoin  que es transformar una variable numerica a categorica
# luego la numerizacion es alrever que la discretizacion
# vemos que hay correlaciones importantes para ver si tiene sentido hacer un ACP
# la matriz es simetrica tanto de la triangular superior o inferior
# la correlacion solo tiene sentido con variables nominales
# sirve para  ver si los datos son estadísticamente significativos,

cor(datos)
correlacion<-round(cor(datos), 1)
corrplot(correlacion, method="number", type="upper")
chart.Correlation(datos, histogram = F, pch = 19)


# para un acp debemos scarlar  los datos normalizarlos
# hacemos la normalizacion de los datos , escalar los  datos en una misma magnitud
# se puede hacer  la  estandirecazion (resta la media y divide por la SD)
View(datos)
sdatos<-scale(datos)
View(sdatos)

# otro grafico de  corelacion
install.packages("corplot")
library(corrplot)

datcor<-cor(sdatos)
corrplot(sdatos,method="circle")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Nuestros datos es mejor tenerlos en un data.frame
corsdatos

dat1 <- data.frame(datos)
dat1 <- data.frame(datos$Milk,datos$Grocery)
dat2 <- data.frame(datos$Fresh,datos$Frozen)

chart.Correlation(dat1)
chart.Correlation(dat2)

chart.Correlation(datcor, histogram = TRUE, method = "pearson")
corrplot.mixed(cor(datcor),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

install.packages("psych")
library(psych)

pairs.panels(datcor,
             smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
             scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
             density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
             ellipses = TRUE,    # Si TRUE, dibuja elipses
             method = "pearson", # Método de correlación (también "spearman" o "kendall")
             pch = 21,           # Símbolo pch
             lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
             cor = TRUE,         # Si TRUE, agrega correlaciones
             jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
             factor = 2,         # Nivel de ruido añadido a los datos
             hist.col = 4,       # Color de los histogramas
             stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
             ci = TRUE)          # Si TRUE, añade intervalos de confianza a los ajustes


#se usa prcomp() para hacer el  analisis de componentes principales
#puedo scarlar dentro del prcomp y central los elementos.
# procedemo a  sacar los PC

acp<-prcomp(datos,center = T,scale =T )
?prcomp # con esto vemos los valores  que se crean con la funcion

# vemos la matriz de rotacion
acp

# con summary vemos la proporcion de variiabilidad total de los datos

summary(acp)
#biplot(acp,scale = 0)

# con Cumulative Proportion  veo la cantidad de informacion que explica el 100%
# el segundo CP explica la varianza no explicada por el CP1 y asisucesivamente
# para decidir con cuanto CP nos quedamos nos basamos en un criterio (KEISER)
# La varianza de los  componentes principales no debe ser menor a 1
# porque si no no explicaria bien

# sacarmos la desviacion standar extrayento la sd de cada
desv_stand= acp[[1]]  # nos  da los primeros valores
# aca vemos que los ACP tiene una variaza pero nesecitamos la variaza 
desv_stand # es la raiz de la variaza

# pero necesitamos ver la  varianza para saber si esta varianza es mayor a  1
variaza=desv_stand^2 #multiplicamos por raiz cuadrada

# aca sacamos  los ACP que necesitamos porque tiene la varianza mayor a 1
variaza # aca explicamos la mayor variabliddad posive perdiendo la minima informacion

# otra manera grafica de ver la cantidad requerida de CP a utilizar
plot(acp,type="l")
library(factoextra)
fviz_eig(acp)

# guardamos los primero  CP
cp1 =acp[[2]][,1]
cp2 =acp[[2]][,2]
cp3 =acp[[2]][,3]
cp4 =acp[[2]][,4]
# guardamos todos
# la transformacion es ortogonal por eso no importa el signo negativo o positivo
componentes<- cbind(cp1,cp2,cp3,cp4)

#muestra las coordenadas de los individuos en el plano
individuos<-acp$x[,1:4]
# vamos a interpretar los datos

install.packages("ade4")
library(ade4)

# vamos graficar los individuos  que son la multiplicacion
View(componentes)

x11()
#CP1 en el eje X y CP2 eje Y
s.corcircle(componentes[,-3],sub = "CP1 y CP2",possub = "topright" )
x11()
s.label(individuos[,-3],label = row.names(datos),sub = "CP1 y CP2",possub = "topright")

x11()
s.corcircle(componentes[,-2],sub = "CP1 y CP3",possub = "topright")

x11()

s.label(individuos[,-2],label = row.names(datos),sub = "CP1 y CP3",possub = "topright")

x11()

s.corcircle(componentes[,-1],sub = "CP2 y CP3",possub = "topright")

x11()

s.label(individuos[,-1],label = row.names(datos),sub = "CP2 y CP3",possub = "topright")

# otra forma de ver los valores

library(FactoMineR)
acp_compras<-PCA(datos,graph = FALSE)
plot(acp_compras,choix = "ind")
plot(acp_compras,choix = "var")


# Otro ejemplo 

url <- "https://raw.githubusercontent.com/rociochavezmx/Rocio-Chavez-youtube-Files/master/compras%20edad.csv"
# Definir la ubicación local para guardar el archivo
filename <- "files/compras_edad.csv"
# Descargar el archivo desde GitHub y guardarlo en la ubicación local
download.file(url, destfile = filename, method = "curl")

library(dplyr)

compras<-read.csv("files/compras_edad.csv",header = T,sep=",",dec = ".",row.names = 1)

View(compras)

dim(compras)
library(FactoMineR)
acp_compras<-PCA(compras,graph = FALSE)


par(mfrow = c(2,2))
## ver la inormacion de los individos
plot(acp_compras,choix="ind")
## ver la inormacion de lo variables
plot(acp_compras,choix="var")



