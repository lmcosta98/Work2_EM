install.packages("fpc")
install.packages("readxl")
install.packages("NbClust")

library(fpc)
library(cluster)
library(readxl)
library(NbClust)

dados <-read_excel("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Time_Use.xlsx")
dados$Country
dados_without_country <- dados[, -1]
rownames(dados_without_country) <- dados$Country


nc <- NbClust(dados_without_country, min.nc = 3, max.nc = 15, method = "kmeans")
kmeans_clust <- kmeans(dados_without_country, 4, nstart = 10)
kmeans_clust

d <- daisy(dados_without_country)
d
plot(silhouette(kmeans_clust$cluster, d), col= c("blue", "red", "green", "yellow"))

#fviz_nbclust(dados_without_country, kmeans, method = "silhouette", k.max = 10)
dendrogramadistancia<-dist(dados_without_country, method="euclidian")
hc = hclust(dendrogramadistancia, method="complete")
hc2 = hclust(dendrogramadistancia, method="single")
plot(hc,hang=-1, main="Distancia euclidiana, e vizinho-mais-afastado")
plot(hc2,hang=-1, main="Distancia euclidiana, e vizinho-mais-proximo")

heatmap(as.matrix(dados_without_country), Rowv=as.dendrogram(hc, Colv=NA))
heatmap(as.matrix(dados_without_country), Rowv=as.dendrogram(hc2, Colv=NA))




