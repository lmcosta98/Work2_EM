install.packages("readxl")
library(readxl)
d<-read_excel("Time_Use_in_OECD_Countries_OECD_after.xlsx")
View(d)
dadosx<-d[,-1]
#rownames(dadosx)<-d[,1]
pca <- prcomp(dadosx)
pcas <- prcomp(dadosx,scale=TRUE)
biplot(pca)
#biplot(pcas)

#d[19,]$Country
