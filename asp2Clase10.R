
corr <- cor(bh[,-14])
corr
chart.Correlation(corr, histogram = F, pch = 19)

corrplot(corr, method = "circle")

corrplot.mixed(cor(bh),
               lower = "number", 
               upper = "circle",
               tl.col = "black")


pairs.panels(cor(bh),
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



#scale = T, matriz de correlaciones
#scale = F, matriz de covarianzas
bh.acp <- prcomp(bh[,-14], scale = F)

summary(bh.acp)

plot(bh.acp)
plot(bh.acp, type = "lines")

#Seleccionamos los componentes principales con lo que se desea trabajar
res_acp <-bh.acp$x[,1:4]
str(res_acp)

View(res_acp)


