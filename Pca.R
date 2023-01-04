#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("NbClust", dependencies = TRUE)
library(readxl)
library(ggplot2)
library(NbClust)

d<-read_excel("Time_Use.xlsx")
par(mfrow = c(1,1))
View(d)
names<-d[1]
View(names)
dadosx<-d[,-1]
pca <- prcomp(dadosx)
pcas <- prcomp(dadosx,scale=TRUE)
summa=summary(pcas)


###### Find best number os pc's ######

variance = pcas$sdev^2 / sum(pcas$sdev^2)
print(variance)
plot(variance, type="o", col="blue", xlab=paste("Number of PC's"),ylab=paste("Variance Explained (%)"))
title(main="Variance Explained Vs Number PC's", col.main="Black", font.main=4)

############ Variance Explained Vs Number PC's ################

variance_index<-c(2,5,8,11,14,17,20,23,26,29,32,35,38,41)
variance_eva <- c(0)
for (x in variance_index){
  variance_eva<-append(variance_eva,variance_eva[length(variance_eva)]+round(summary(pcas)$importance[x]*100))
}

variance_eva=setdiff(variance_eva,0)
print(variance_eva)

plot(variance_eva, type="o", col="blue", xlab=paste("Number of PC's"),ylab=paste("Variance Explained (%)"))

# Create a title with a red, bold/italic font
title(main="Variance Explained Vs Number PC's", col.main="Black", font.main=4)

###############
# biplot preservando a metrica das colunas
pcas.data<-data.frame(PC1=pcas$x[,1],
                      PC2=pcas$x[,2],
                      PC3=pcas$x[,3],
                      PC4=pcas$x[,4],
                      PC5=pcas$x[,5])
plot(pcas.data,pch=5) # Check all 5 dim

#Compare only 2 of them
biplot(pcas,choices=1:2,pch=15,col=c("blue","red"), cex=0.8,cex.axis=0.7,arrow.len = 0.05,xlab=paste(" PC1  (", (round(100*summa$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*summa$importance[2,2],digits=1)), " % )"),var.axes=TRUE,  scale=1,  main="biplot")

############### Cluster ################
library(NbClust)
a=prcomp(dadosx, scale=TRUE)
print(a)
dados2G<-kmeans(a$x, 5, nstart=20)
dados2G

#Visualizar os cluster da k-medias:
str(dados2G)
a=prcomp(dadosx, scale=TRUE)
#nc <- NbClust(a$x,min.nc=3,max.nc=15,method="kmeans)
vcol=c("#65237D","#A875BA","#C4C3E0", "#D19852","#A85E03")
plot(a$x[,1:5], col = vcol[dados2G$cluster],pch=19, main="kmeans, 5 grupos")

#install.packages("cluster")
library(cluster)
D <- daisy(dadosx)
sil_cl <- silhouette(dados2G$cluster, D)
rownames(sil_cl) <- names$Country
plot(sil_cl, col = c("#65237D","#A875BA","#C4C3E0", "#D19852","#A85E03"),cex.names = par("cex.axis"))

#Check important Eigen Vectors in PCA#

library(nFactors)
library(psych)
library(GPArotation)
n_factors <-5
fit_princ <-principal(dadosx,n_factors,residuals=TRUE,rotate="none",covar=FALSE,)
loads2 <- fit_princ$loadings
# fa.diagram(loads2,col="#5E2B97") #Slides
fa.diagram(loads2,size=c(8,6),node.font=c("Helvetica",14),digits=1,main="Using PCA")

#HEATMAP
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(11, "PuOr"))(50)

cen <- a$x[,1:5]
rownames(cen) <- names$Country
colnames(cen) <- c("PC1", "PC2", "PC3", "PC4","PC5")
heatmap(cen, scale = "column", col = coul)
abline(h = 0.4, col = "red", lwd = 2, lty = 2)
abline(h = 10, col = "red", lwd = 2, lty = 2)
abline(h = 13, col = "red", lwd = 2, lty = 2)
abline(h = 16.5, col = "red", lwd = 2, lty = 2)
abline(h = 35.5, col = "red", lwd = 2, lty = 2)
abline(h = 37.1, col = "red", lwd = 2, lty = 2)

legend(
  0, 1,
  ncol = 3,
  legend = c("-100%", "-75%", "-50%", "-25%", "0%", "25%", "50%", "75%", "100%"),
  fill = colorRampPalette(brewer.pal(9, "PuOr"))(9)
)
