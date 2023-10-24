#ver si estamos en el direcctorio
getwd()
setwd()

install.packages("dplyr")

library(dplyr)

#instalar librerias 
install.packages("ggplot2")
install.packages(c("xlsx","RODBC"))
install.packages(c("xlsx","RODBC"),dependencies = T)
#invocar las librerias
#
x <- 10
y =20
z<-c(1,2,"Freddy")
z<-c(1,2,3,4,5,20)
z
str(z)
class(z)
y
x
rm(x)


library(xlsx)
xlsx


iris
View(iris)
str(iris)
class(iris)
summary(iris)
View(cars)

a<- "1"
class(a)
a<-as.integer(a)

fecha<- "2019-01-01"
fecha<-as.Date(fecha)
class(fecha)

head(iris,10)
names(iris)

data.iris<-iris

data.iris.agregate<-data.iris
data.iris.agregate$dummy<-mean(data.iris.agregate$Petal.Width)
View(data.iris.agregate)

data.iris.agregate$dummy<-NULL
names(data.iris.agregate)



data.iris %>% filter(Species=="setosa")



## materia nueva

mat<-matrix(1:9,3,3)
mat
apply(mat,1,sum)
apply(mat,2,sum)

x<-list(9:16)
lapply(x,sqrt)


x<-list(1:10)
sapply(x,sqrt)
lapply(x,sqrt)

vector<-c(1.3,4,5,6,89,89)
c


select(iris, Sepal.Width, Petal.Length, Species)
select(iris, ends_with("Length"))
select(iris, starts_with("Sepal"))
select(iris, contains(".")) #contains character
select(iris, matches(".t.")) # match Regex
select(iris, num_range("x", 1:5))
select(iris, Sepal.Length:Petal.Width) #range between 2 columns
select(iris, -Species) #all except specified

slice(iris,1:4)
filter(iris,Sepal.Length>7.6)


