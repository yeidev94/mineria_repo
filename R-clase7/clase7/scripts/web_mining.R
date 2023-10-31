
# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)
install.packages("dplyr", dependencies = T)
library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 


# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#install.packages("rvest", dependencies = T)

library(rvest)
library(dplyr)
library(stringr)


link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# se debe trabajar con el Selector Gadject de Google Chrone para buscar como se llaman las secciones a descargar 
name = page %>% html_nodes(".lister-item-header a") %>% html_text()
year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()

movies = data.frame(name, year, rating, synopsis,duration,gender, stringsAsFactors = FALSE)

write.csv(movies, "files//movies.csv")
View(movies)

# Verificar la longitud de cada vector
length(name)
length(year)
length(rating)
length(synopsis)
length(duration)
length(gender)


# Multiples paginas
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  if (length(movie_page %>% html_nodes(".primary_photo+ td a")) > 0) {
    movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
      html_text() %>% paste(collapse = ",")
  } else {
    movie_cast = NA
  }
  return(movie_cast)
}


# declarar un dataframe vacion
movies = data.frame()

for (page_result in seq(from = 1, to = 201, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  #cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  duration = page %>% html_nodes(".lister-item-content .runtime") %>% html_text() %>% str_extract("\\d+") %>% as.integer()
  gender=page %>% html_nodes(".lister-item-content .genre") %>% html_text() %>% str_remove("\\s+") %>% str_replace_all(",", "|") %>% trimws()
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, duration,gender, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

#gender=page %>% html_nodes(".genre") %>% html_text() 
#gender = c(gender, rep(gender[length(gender)], 50 - length(gender)))
#investment =page %>% html_nodes(".text-muted+ span") %>% html_text() 

dim(movies)
View(movies)

datos<-movies
View(datos)
head(datos,6)

library(qdap)
library(ggplot2)
datos$year
datos<-datos %>% mutate(year=gsub("[][!#$%I()*,.:;<=>@^_`|~.{}]", "", datos$year))
datos$year<-as.numeric(datos$year)
View(datos)


df_gender<-datos %>% group_by(gender) %>% 
  summarise(total=n())

ggplot(data=df_gender, aes(x=gender, y=total)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=as.character(total),vjust=0))+
  theme(text = element_text(size=10),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x="", y="Total", title="Distribución por género de peliculas")

# para desagrupar los generos y ver una mejor visualizacion 
library(tidyr)
library(stringr)

conjunto <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

unique(conjunto$gender)

colores <- hcl(h = seq(0, 360, length.out = 20), c = 100, l = 65)

datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ") %>%
  count(gender, sort = TRUE) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = n), vjust = 0.5, size = 5, color = "black") +
  labs(x = "Género de la película", y = "Total", title = "Distribución por género de películas") +
  scale_fill_manual(values = colores) +
  theme_minimal()


library(tidyr)
library(ggplot2)

# Grafico por rating

df_rating<-datos %>% group_by(rating) %>% 
  summarise(total=n())

ggplot(data=df_rating, aes(x=rating, y=total, fill=rating)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  theme_minimal()

# vamos a sacar un analisis de las peliculas de aventura para ver cuales puedo ver 
adventure_movies <- datos %>%
  mutate(gender = str_squish(str_replace_all(`gender`, "\\|", ", "))) %>%
  separate_rows(gender, sep = ", ")

# Imprimir el resultado de separate_rows()
print(adventure_movies$gender)

# Filtrar y contar filas antes y después
adventure_movies_filtered <- adventure_movies %>%
  filter(gender == "Adventure" & rating > 7)
cat("Número de filas antes de filtrar:", nrow(adventure_movies), "\n")
cat("Número de filas después de filtrar:", nrow(adventure_movies_filtered), "\n")

View(adventure_movies_filtered)

# ahora que vimos los rating podemos hacer otras exploraciones con un grupo especifico
# ahora un poco a trabajar el texto cargamos el set de datos con condiciones especiales

analisis<-datos%>% filter(rating>=7.6)
synopsis<-analisis$synopsis

library(tm)
texto<-gsub(pattern = "\\W",replace=" ",synopsis)
#Elimino numeros
texto<-gsub(pattern = "\\d",replace=" ",texto)
texto<-tolower(texto)
texto<- removeWords(texto,stopwords("english"))

texto<-gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",texto)
# quitamos los espacios en blaco
texto<-stripWhitespace(texto)

texto<-tm::removePunctuation(texto)
library(wordcloud)
wordcloud(texto)

# se pinta como en circulo
wordcloud(texto,min.freq =2,random.order = F, scale = c(4,0.5),
          color = rainbow(3))

### -----------------------------------------------------XXXX
# Un analisis de sentimiento 
# Diccionarios de analisis de sentimiento

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")

install.packages("tidytext")
library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

library(tidytext)

datos_tidy <- datos %>%
  unnest_tokens(word, synopsis)

# un ejemplo es usar la libreria textdata que tiene una BD de positivo y negativos

install.packages("textdata")
library(textdata)
afinn <- get_sentiments("afinn")

datos_sentimiento <- datos_tidy %>%
  inner_join(afinn, by = "word") %>%
  group_by(name) %>%
  summarize(sentimiento = sum(value))

View( datos_tidy)
# ya podemos ver los resultados de los peliculas con elemento positivas y las que tienen reseñas negativas
datos_sentimiento
View(datos_sentimiento %>% filter(sentimiento>0))
View(datos_sentimiento %>% filter(sentimiento<0))

##############################

library(tidytext)
library(dplyr)
library(textdata)

# Transformar el texto a formato tidy
texto <- adventure_movies$synopsis %>% paste(collapse = " ")
datos_tidy <- tibble(texto) %>%
  unnest_tokens(word, texto)

# Obtener los sentimientos del diccionario NRC
nrc <- get_sentiments("nrc")

# Unir los sentimientos con los textos
datos_sentimiento <- datos_tidy %>%
  inner_join(nrc, by = "word",multiple = "all") %>%
  group_by(sentiment) %>%
  summarize(sentimiento = n())

# Ver los resultados
View(datos_sentimiento)

library(ggplot2)

ggplot(datos_sentimiento, aes(x = sentiment, y = sentimiento, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = sentimiento), vjust = -0.5) +
  xlab("Sentimiento") +
  ylab("Cantidad de elementos") +
  ggtitle("Análisis de sentimiento de los datos") +
  theme(plot.title = element_text(hjust = 0.5)) 

