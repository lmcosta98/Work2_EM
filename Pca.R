#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("ggfortify")
library(readxl)
library(ggplot2)
#library(ggfortify)
d<-read_excel("Time_Use_in_OECD_Countries_OECD_after.xlsx")
#View(d)
dadosx<-d[,-1]
pca <- prcomp(dadosx)
pcas <- prcomp(dadosx,scale=TRUE)
#biplot(pcas)
summa=summary(pcas)
#print(pcas$x[,5])
#print(pcas[1:2])


###### Find best number os pc's ######

variance = pcas$sdev^2 / sum(pcas$sdev^2)
print(variance)
qplot(c(1:14), variance) +
  geom_line() +
  geom_point(size=14)+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
###############
# biplot preservando a metrica das colunas
pcas.data<-data.frame(PC1=pcas$x[,1],
                      PC2=pcas$x[,2],
                      PC3=pcas$x[,3],
                      PC4=pcas$x[,4],
                      PC5=pcas$x[,5])
plot(pcas.data,pch=5,col=c("blue","red","green","brown","pink")) # Check all 5 dim

#Compare only 2 of them
biplot(pcas,choices=1:2,pch=15,col=c("blue","red"), cex=0.8,cex.axis=0.7,arrow.len = 0.05,xlab=paste(" PC1  (", (round(100*summa$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*summa$importance[2,2],digits=1)), " % )"),var.axes=TRUE,  scale=1,  main="biplot")

#Analysing Biplot
# As we can see the countries with the number 14 and 15 are, respectively,
# Japan and Korea are the ones that stand out, as they are independent from the rest
# countries having no similarities. Country number 22(Norway) and 12(Ireland)
# also stand out for independence, it is clear that they no longer stand out
# than Japan and Korea, however, these are also independent. We can say
# it is well known that countries 32(India),24(Portugal),19(Mexico),33(SouthAfrica),
# 17(Lithuania) are quite similar to each other. The countries 28(Turkey), 31(China),
# 16(Latvia), 25(Slovenia) and 2(Austria) are quite similar between them.
# Countries 4(Canada),23(Poland),30(USA),6(Estonia),21(New zealand),1(Australia),
# 5(Denmark),26(Spain),are similar to each other. The countries 18(Luxembourg),27(Sweden),
# 3(Belgium),13(Italy), 9(Germany),7(Finland),29(UK),20(Netherlands), are similar to each other.
# 8(France), 11(Hungary),10(Greece) are also another group.

############ Variance Explained Vs Number PC's ################

variance_index<-c(2,5,8,11,14,17,20,23,26,29,32,35,38,41)
variance_eva <- c(0)
#print(variance_eva)
#print(variance_eva[length(variance_eva)])
for (x in variance_index){
    variance_eva<-append(variance_eva,variance_eva[length(variance_eva)]+round(summary(pcas)$importance[x]*100))
}
variance_eva=setdiff(variance_eva,0)
print(variance_eva)

plot(variance_eva, type="o", col="blue", xlab=paste("Number of PC's"),ylab=paste("Variance Explained (%)"))

# Create a title with a red, bold/italic font
title(main="Variance Explained Vs Number PC's", col.main="Black", font.main=4)


############### Cluster ################
dados2G<-kmeans(dadosx, 5)
dados2G

#Visualizar os cluster da k-medias:
str(dados2G)
a=prcomp(dadosx, scale=TRUE)
plot(a$x[,1:5], col = dados2G$cluster, pch=19, main="kmeans, 5 grupos")
#install.packages("cluster")
library(cluster)
D <- daisy(dadosx)
plot(silhouette(dados2G$cluster, D),col= c("blue", "purple","orange","red","black"))


#autoplot(, x = 1, y = 2, colour = "Species",
#         loadings = TRUE, loadings.label = TRUE,
#         loadings.colour = c("blue", "red", NA, NA),
#         loadings.label.colour = c("blue", "red", NA, NA),
#         main = "biplot")



# d[22,]$Country
# cor(dadosx,a$x)

######### Trying To add more PC's Into one graph ############
require(graphics)

par(pty="s",
    cex.main=1.2,
    cex.lab=1,
    font.main=2,
    font.lab=2,
    family="sans",
    col.main="gray10",
    col.lab="gray10",
    fg="gray10",
    las=1)

plot.new()
plot.window(xlim=c(-6,6),
            ylim=c(-6,6),
            zli~m=c(-6,6),
            asp=1)

axis(side=1,
     at=c(-6,-3,0,3,6),
     labels=TRUE)

axis(side=2,
     at=c(-6,-3,0,3,6),
     labels=TRUE)
axis(side=3,
     at=c(-6,-3,0,3,6),
     labels=TRUE)

title(main = "Biplot for PCs",
      line=3,
      adj=0)
title(xlab=paste(" PC1  (", round(summary(pcas)$importance[2]*100,digits=1), " % )",sep=""),
      ylab=paste(" PC2  (", round(summary(pcas)$importance[5]*100,digits=1), " % )",sep=""),
      #zlab=paste(" PC3  (", round(summary(pcas)$importance[8]*100,digits=1), " % )",sep=""),
      line=2,
      adj=0.5)

points(x=pcas$x[,1:5],
       pch=c(rep(16,times=1),
             rep(17,times=1)),
       cex=1,
       col=c(rep("darkcyan",times=1),
             rep("orangered",times=1)))
