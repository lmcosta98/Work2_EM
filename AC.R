#install.packages("fpc")
#install.packages("readxl")
#install.packages("NbClust")
#install.packages("factoextra")
#install.packages('factoextra')

library(fpc)
library(cluster)
library(readxl)
library(NbClust)
library(factoextra)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(dendextend)

# Load the datasets
dados <-read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Time_Happiness_GDP.csv") # dataset for only 2020
data <- read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Happiness.csv")
n_dados <- read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Happiness_GDP.csv") # complete dataset for 10 years

# Dataframes for clustering
n_dados_clust <- n_dados[, -1]
n_dados_clust
tmp <- dados[, -1] # cluster based on time spent
rownames(tmp) <- dados$Countries
dados_clust <- tmp[, -c(1:3)]
dados_viz <- tmp[, c(1:3)] # auxiliar for plotting
dados_viz_2 <- dados_viz # another auxiliar for plotting
dados_hap <- tmp[, -2] # cluster based only on happiness
dados_hap_gdp <- tmp[, c(1:2)]
dados_hap_gdp


# Setting color palette
coul <- colorRampPalette(brewer.pal(3, "PuOr"))(50)

myColors <- brewer.pal(5,"PuOr")
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)

#### Clustering based on Happiness and GDP #####
n_dados_clust <- n_dados[, -1]
n_dados_clust
# Find out the optimal number of clusters
nc <- NbClust(n_dados_clust, min.nc = 2, max.nc = 15, method = "kmeans") # Ideal number found is 3 clusters
kmeans_hap_gdp <- kmeans(n_dados_clust, 3, nstart = 20)
kmeans_hap_gdp # By analyzing the clusters we can see that the countries tend to be
               # assigned to the same cluster on the several years. This is to be expected since 
               # GDP and happiness do not vary much in around 10 years.
               # To make the results more legible we will cluster only on the happiness data from 2020.

n_dados$cluster <- as.character(kmeans_hap_gdp$cluster)

# Plot the silhouette graph
d <- daisy(n_dados_clust)
sil <- silhouette(kmeans_hap_gdp$cluster, d)
rownames(sil) <- n_dados$Countries
plot(sil, col= c("blue", "red", "green"))#, "green", "yellow"))

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(n_dados_clust),clustering=kmeans_hap_gdp$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg sil

heatmap(as.matrix(n_dados_clust), scale = "column", col = coul)
#heatmap(as.matrix(tmp), scale = "column", col = coul)


# Clustering using only the 2020 data for happiness and gdp
nc <- NbClust(dados_hap_gdp, min.nc = 2, max.nc = 15, method = "kmeans") # Ideal number found is 3 clusters
kmeans_hap_gdp_2020 <- kmeans(dados_hap_gdp, 3, nstart = 20)
kmeans_hap_gdp_2020 # By analyzing the clusters we can see t
d <- daisy(dados_hap_gdp)
sil <- silhouette(kmeans_hap_gdp_2020$cluster, d)
rownames(sil) <- dados$Countries
plot(sil, col= c("blue", "red", "green"))#, "green", "yellow"))

res=cluster.stats(dist(dados_hap_gdp),clustering=kmeans_hap_gdp_2020$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg sil

heatmap(as.matrix(dados_hap_gdp), scale = "column", col = coul)

distances <-dist(dados_hap_gdp, method="euclidian") # Euclidean distance
hc_complete = hclust(distances, method="complete") # Trying to obtain clusters that are more compact (data points more similiar to each)
#hc_single = hclust(distances, method="single") # Trying to obtain fewer clusters
dend <- as.dendrogram(hc_complete)
dend <- dend %>% color_branches(k=4)
plot(dend, main="Euclidean distance and furthest neighbour")
#plot(hc_complete, hang=-1, main="Distancia euclidiana, e vizinho-mais-afastado")


# Plot the clusters in relation to happiness and gdp
dados_viz$cluster <- as.character(kmeans_hap_gdp_2020$cluster)
dados_viz
ggplot(dados_viz, aes(x=log(GDP_Per_Capita), y=Happiness, colour=cluster))+geom_point()
#plot(log(dados_viz$GDP_Per_Capita), dados_viz$Happiness, col=dados_viz$cluster)
ggplot(dados_viz, aes(x=log(GDP_Per_Capita), y=Happiness, colour=Continent))+geom_point()
# Plotting frequency of happiness responses and comparing total vs cluster happiness


#### Need to add the lines for the total frequency
# plotting the distrution of the answers regarding happines per country
data_clust_1 <- n_dados[n_dados$cluster == '1',]
data_clust_2 <- n_dados[n_dados$cluster == '2',]
data_clust_3 <- n_dados[n_dados$cluster == '3',]
par(mfrow=c(2,2))
hist(n_dados$Happiness, xlab = "Happiness", main="Happiness on complete dataset")
hist(data_clust_1$Happiness, xlab = "Happiness", main="Happiness on cluster 1", col="red")
hist(data_clust_2$Happiness, xlab = "Happiness", main="Happiness on cluster 2", col="green")
hist(data_clust_3$Happiness, xlab = "Happiness", main="Happiness on cluster 3", col="blue")




#### Clustering based on time spent #####

# Find out the optimal number of clusters
nc <- NbClust(dados_clust, min.nc = 3, max.nc = 15, method = "kmeans") # Ideal number found is 4 clusters

# Clustering the data
kmeans_clust <- kmeans(dados_clust, 4, nstart = 20)
kmeans_clust

# Adding the clusters to the data to be visualized
dados_viz$cluster <- as.character(kmeans_clust$cluster)
dados_viz

# Plot the silhouette graph
d <- daisy(dados_clust)
sil <- silhouette(kmeans_clust$cluster, d)
rownames(sil) <- dados$Countries
plot(sil, col= c("blue", "red", "green", "yellow"))
# The shilhouettes all seem to be small, but lets check the ARI

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(dados_clust),clustering=kmeans_clust$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg silhouette and a positive ARI, everything seems to be in order

# Plotting the clusters on the data relative to Happiness and GDP
ggplot(dados_viz, aes(x=GDP_Per_Capita, y=Happiness))+geom_point(aes(color=cluster)) # It does not seem to be a connection
                                                                                # between the cluster formed by time spent 
                                                                                # and the happiness/gdp relationship

# Creating a dendrogram to plot the clusters
distances <-dist(dados_clust, method="euclidian") # Euclidean distance
hc_complete = hclust(distances, method="complete") # Trying to obtain clusters that are more compact (data points more similiar to each)
hc_single = hclust(distances, method="single") # Trying to obtain fewer clusters
plot(hc_complete, hang=-1, main="Distancia euclidiana, e vizinho-mais-afastado")
plot(hc_single, hang=-1, main="Distancia euclidiana, e vizinho-mais-proximo") 
# With single linkage we can't check the presence of distinct clusters, so we opt for complete linkage

# Creating a heatmap to see relationships between countries and variables
heatmap(as.matrix(dados_clust), scale = "column", col = coul)
#heatmap(as.matrix(dados_clust), scale = "column", col = coul)


#### Clustering the variables of the time spent dataset
cols.cor <- cor(dados_clust, use = "pairwise.complete.obs", method = "pearson") # using pearson correlation for clustering variables
dist <- as.dist(1 - cols.cor)
hc_complete = hclust(dist, method="complete")
dend <- as.dendrogram(hc_complete)
dend <- dend %>% color_branches(k=4)
plot(dend, main="Pearson Correlation and nearest neighbour")
dev.off()




#### Clustering based on Time spent and Happiness #####  

# Find out the optimal number of clusters
nc <- NbClust(dados_hap, min.nc = 3, max.nc = 15, method = "kmeans")

# Clustering the data
kmeans_clust_hap <- kmeans(dados_hap, 4, nstart = 10)
kmeans_clust_hap

# Adding the clusters to the data to be visualized
dados_viz_2$cluster <- kmeans_clust_hap$cluster
dadoa_viz_2
# Plot the silhouette graph
d <- daisy(as.matrix(dados_hap))
sil <- silhouette(kmeans_clust_hap$cluster, d)
rownames(sil) <- dados$Countries
plot(sil, col= c("blue", "red", "green", "yellow"))

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(dados_clust_hap),clustering=kmeans_clust_hap$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Plotting the clusters on the data relative to Happiness and GDP
ggplot(dados_viz_2, aes(x=GDP_Per_Capita, y=Happiness))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")
#ggplot(dados_viz_2, aes(x=Happiness, y=GDP_Per_Capita))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")

heatmap(as.matrix(dados_hap), scale = "column", col = coul)

# No significant difference found when including happiness.
