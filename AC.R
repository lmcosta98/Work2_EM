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

# Load the datasets
dados <-read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Time_Happiness_GDP.csv")
data <- read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Happiness.csv")
n_dados <- read.csv("/Users/luismiguel/Desktop/Uni/EM/2022/Work2_EM/datasets/Happiness_GDP.csv")

# Dataframes for clustering
tmp <- dados[, -1] # cluster based on time spent
rownames(tmp) <- dados$Countries
dados_clust <- tmp[, -c(1:2)]
dados_viz <- tmp[, c(1:2)] # auxiliar for plotting
dados_viz_2 <- dados_viz # another auxiliar for plotting
dados_hap <- tmp[, -2] # cluster based only on happiness
dados_hap_gdp <- tmp[, c(1:2)]
dados_hap_gdp

#### Clustering based on Happiness and GDP #####
n_dados_clust <- n_dados[, -1]
n_dados_clust
# Find out the optimal number of clusters
nc <- NbClust(n_dados_clust, min.nc = 2, max.nc = 15, method = "kmeans") # Ideal number found is 3 clusters
kmeans_hap_gdp <- kmeans(n_dados_clust, 3, nstart = 20)
kmeans_hap_gdp # By analyzing the clusters we can see that the countries tend to be
               # assigned to the same cluster on the several years. This is to be expected since 
               # GDP and happiness do not vary much in around 10 years.
               # Can make use of this to cluster only on the happiness data from 2020.

# Plot the silhouette graph
d <- daisy(n_dados_clust)
plot(silhouette(kmeans_hap_gdp$cluster, d), col= c("blue", "red", "green"))#, "green", "yellow"))

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(n_dados_clust),clustering=kmeans_hap_gdp$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg sil

heatmap(as.matrix(dados_hap_gdp), scale = "column", col = coul)
heatmap(as.matrix(tmp), scale = "column", col = coul)

# clustering using only the 2020 data for happiness and gdp
nc <- NbClust(dados_hap_gdp, min.nc = 2, max.nc = 15, method = "kmeans") # Ideal number found is 3 clusters
kmeans_hap_gdp_2020 <- kmeans(dados_hap_gdp, 3, nstart = 20)
kmeans_hap_gdp_2020 # By analyzing the clusters we can see t
d <- daisy(dados_hap_gdp)
plot(silhouette(kmeans_hap_gdp_2020$cluster, d), col= c("blue", "red", "green"))#, "green", "yellow"))

res=cluster.stats(dist(dados_hap_gdp),clustering=kmeans_hap_gdp_2020$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg sil


# Plot the clusters in relation to happiness and gdp
n_dados$cluster <- kmeans_hap_gdp$cluster
ggplot(n_dados, aes(x=GDP_Per_Capita, y=Happiness))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")

# Plotting frequency of happiness responses and comparing total vs cluster happiness
hist(n_dados$Happiness, xlab = "Happiness", main="Happiness on complete dataset")

#### Need to add the lines for the total frequency
data_clust_1 <- n_dados[n_dados$cluster == 1,]
hist(data_clust_1$Happiness, xlab = "Happiness", main="Happiness on cluster 1")
#hist(data_clust_1$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 1")

data_clust_2 <- n_dados[n_dados$cluster == 2,]
hist(data_clust_2$Happiness, xlab = "Happiness", main="Happiness on cluster 2")
#hist(data_clust_2$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 2")

data_clust_3 <- n_dados[n_dados$cluster == 3,]
hist(data_clust_3$Happiness, xlab = "Happiness", main="Happiness on cluster 3")
#hist(data_clust_3$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 3")

# checking countries per cluster
dados_hap_gdp$cluster <- kmeans_hap_gdp_2020$cluster
dados_hap_gdp[dados_hap_gdp$cluster == 1,] # Countries in cluster 1
dados_hap_gdp[dados_hap_gdp$cluster == 2,] # Countries in cluster 2
dados_hap_gdp[dados_hap_gdp$cluster == 3,] # Countries in cluster 3




#### Clustering based on time spent #####

# Find out the optimal number of clusters
nc <- NbClust(dados_clust, min.nc = 3, max.nc = 15, method = "kmeans") # Ideal number found is 4 clusters

# Clustering the data
kmeans_clust <- kmeans(dados_clust, 4, nstart = 20)
kmeans_clust

# Adding the clusters to the data to be visualized
dados_viz$cluster <- kmeans_clust$cluster
dados_viz

# Plot the silhouette graph
d <- daisy(dados_clust)
plot(silhouette(kmeans_clust$cluster, d), col= c("blue", "red"))#, "green", "yellow"))
# The shilhouettes all seem to be small, but lets check the ARI

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(dados_clust),clustering=kmeans_clust$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3) # Small avg silhouette and a positive ARI, everything seems to be in order

# Plotting the clusters on the data relative to Happiness and GDP
ggplot(dados_viz, aes(x=GDP_Per_Capita, y=Happiness))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")
# trying switching the axis
ggplot(dados_viz, aes(x=Happiness, y=GDP_Per_Capita))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")

# Creating a dendrogram to plot the clusters
distances <-dist(dados_clust, method="euclidian") # Euclidean distance
hc_complete = hclust(distances, method="complete") # Trying to obtain clusters that are more compact (data points more similiar to each)
hc_single = hclust(distances, method="single") # Trying to obtain fewer clusters
plot(hc_complete, hang=-1, main="Distancia euclidiana, e vizinho-mais-afastado")
plot(hc_single, hang=-1, main="Distancia euclidiana, e vizinho-mais-proximo") 
# With single linkage we can't check the presence of distinct clusters, so we opt for complete linkage

# Creating a heatmap to see relationships between countries and variables
coul <- colorRampPalette(brewer.pal(11, "BrBG"))(50)
heatmap(as.matrix(dados_clust), scale = "row", col = coul)
#heatmap(as.matrix(dados_clust), scale = "column", col = coul)



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
plot(silhouette(kmeans_clust_hap$cluster, d))#, col= c("blue", "red", "green", "yellow"))

# Obtain the ARI and Avg Silhouette values
res=cluster.stats(dist(dados_clust_hap),clustering=kmeans_clust_hap$cluster)
resultadoIndices <- matrix(c(res$corrected.rand,res$avg.silwidth), byrow=TRUE,1,2)
colnames(resultadoIndices) <- c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Plotting the clusters on the data relative to Happiness and GDP
ggplot(dados_viz_2, aes(x=GDP_Per_Capita, y=Happiness))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")
#ggplot(dados_viz_2, aes(x=Happiness, y=GDP_Per_Capita))+geom_point(aes(color=cluster))+scale_color_viridis(option = "A")

heatmap(as.matrix(dados_hap), scale = "row", col = coul)

# No significant difference found when including happiness, 
# so we will use the results from the first clustering.


# Analyzing the clusters
dados$cluster <- kmeans_clust$cluster
data <- merge(data, dados, by="Countries")
#data <- data.frame(data)
data


# Plotting frequency of happiness responses and comparing total vs cluster happiness
hist(data$Happiness.x, xlab = "Happiness", main="Happiness on complete dataset")

#### Need to add the lines for the total frequency
data_clust_1 <- data[data$cluster == 1,]
hist(data_clust_1$Happiness.x, xlab = "Happiness", main="Happiness on cluster 1")
#hist(data_clust_1$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 1")

data_clust_2 <- data[data$cluster == 2,]
hist(data_clust_2$Happiness.x, xlab = "Happiness", main="Happiness on cluster 2")
#hist(data_clust_2$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 2")

data_clust_3 <- data[data$cluster == 3,]
hist(data_clust_3$Happiness.x, xlab = "Happiness", main="Happiness on cluster 3")
#hist(data_clust_3$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 3")

data_clust_4 <- data[data$cluster == 4,]
hist(data_clust_4$Happiness.x, xlab = "Happiness", main="Happiness on cluster 4")
#hist(data_clust_4$GDP_Per_Capita, xlab = "GDP per Capita", main="GDP on cluster 4")

