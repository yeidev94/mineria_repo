library(dplyr)

protein <- read.csv("files/protein.csv",sep = ";")
head(protein)
View(protein)
data <- as.data.frame(scale(protein[,-1]))
View(data)
data$Country = protein$Country
rownames(data) = data$Country
names(data)
View(data)
data$Country<-NULL
hc <- hclust(dist(data, method = "euclidean"),
             method = "ward.D2")
hc
hcd <- as.dendrogram(hc)

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")

# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)

plot(hcd, xlim = c(1, 20), ylim = c(1,8))

plot(hcd,  xlab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))


#plot(hc, hang = -0.01, cex = 0.7)
fit <- cutree(hc, k=4)
table(fit)
rect.hclust(hc, k=4, border="steelblue")

# install.packages("ape")
library("ape")
library("Rcpp")
library("ape")
library("Rcpp")

par(mar = c(5, 5, 2, 2), cex.axis = 0.8, cex.lab = 0.9, pin = c(6, 6), mai = c(0.8, 0.8, 0.2, 0.2))
plot(as.phylo(hc), type = "unrooted", cex = 0.8, no.margin = TRUE, lwd = 2, font = 4)

colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan",  font = 4, tip.color = colors[clus4],
     label.offset = 1, cex = 0.7,lwd = 2)

