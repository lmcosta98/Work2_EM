#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("ggfortify")
library(readxl)
#library(ggplot2)
#library(ggfortify)
d<-read_excel("Time_Use_in_OECD_Countries_OECD_after.xlsx")
View(d)
dadosx<-d[,-1]
#rownames(dadosx)<-d[,1]
pca <- prcomp(dadosx)
pcas <- prcomp(dadosx,scale=TRUE)
#biplot(pcas)
a=prcomp(dadosx,scale=TRUE)
b=summary(a)
# biplot preservando a m?trica das colunas (biplot cl?ssico, segundo Gabriel)
biplot(a,choices=1:2,pch=15,col=c("blue","red"), cex=0.8,cex.axis=0.7,arrow.len = 0.05,xlab=paste(" PC1  (", (round(100*b$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*b$importance[2,2],digits=1)), " % )"),var.axes=TRUE,  scale=1,  main="biplot")

#autoplot(, x = 1, y = 2, colour = "Species",
#         loadings = TRUE, loadings.label = TRUE,
#         loadings.colour = c("blue", "red", NA, NA),
#         loadings.label.colour = c("blue", "red", NA, NA),
#         main = "biplot")


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

# d[22,]$Country
# cor(dadosx,a$x)
