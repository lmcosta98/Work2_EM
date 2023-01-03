install.packages(c("wesanderson", "NbClust", "nFactors", "GPArotation", "psych"), dependencies = TRUE)

library(nFactors)
library(psych)
library(GPArotation)
library(cluster)
library(NbClust)
library(readxl)
library(tidyr)
library(ggplot2)

# Just to add more colours
library("wesanderson")
library(RColorBrewer)

time_used <- read_excel("datasets/Time_Use.xlsx")
dados <- time_used[, -1]

# Histogram
par(mfrow = c(3, 5))

for (i in colnames(dados)) { # for-loop over columns
  # hist(dados[[i]], main = i, col = "#5E2B97") # slides
  hist(dados[[i]], main = i, col = "#446455") # report
}


## Data cleaning
### Testing for high correlation, our dataset has 14 columns, so if this values returns more than 14 we might have columns that we can discard.
sum(cor(dados) > 0.6)

## To check for columns with MSA inferior to 0.49 Kaiser-Meyer-Olkin factor adequacy

# to_remove<- c("Paid Work", "Tv and Radio", "Seeing friends")
to_remove <- c("Paid Work") # , "Education", "Friends")#, "TV and Radio", "Seeing friends")

dados <- dados[, !(names(dados) %in% to_remove)] # Remove listed variables from data


KMO(dados)
mydat <- dados[, KMO(dados)$MSAi > 0.4] # Get rid of all variables with MSA < 0.50
dados <- mydat
KMO(dados)

## Other options and respective overall MSA
# 69% Care for household members                        Housework                         Shopping Other unpaid work & volunteering                            Sleep         Other leisure activities
# 74 % Housework                         Shopping Other unpaid work & volunteering                            Sleep                 Attending events         Other leisure activities
# 75% Housework                         Shopping Other unpaid work & volunteering                            Sleep         Other leisure activities


## Bartlett test
cortest.bartlett(cor(dados), n = nrow(dados))

r_matrix <- cov(dados)

## Scree Test
parallel_analysis <- nScree(r_matrix, model = "factors")
# Note that model factors is required otherwise the result will be for components
## If the above command does not work due to version, use the one below
# parallel_analysis <- nScree(data.frame(dados), model = "factors") # Note that model factors is required
parallel_analysis
plotnScree(parallel_analysis)

n_factors <- 4
fit_princ <- principal(
  dados,
  n_factors,
  residuals = TRUE,
  rotate = "varimax",
  covar = FALSE,
)
print(fit_princ, digits = 2, cutoff = 0.4, sort = TRUE)

fit <- factanal(dados, n_factors, rotation = "varimax", scores = c("Bartlett"))
print(fit, digits = 2, cutoff = 0.4, sort = TRUE)

par(mfrow = c(1, 2))
loads2 <- fit_princ$loadings
fa.diagram(loads2,
  size = c(8, 6), node.font = c("Helvetica", 14),
  digits = 1, main = "Using PCA"
)


loads <- fit$loadings
fa.diagram(loads,
  labels = NULL, cut = .3, simple = TRUE,
  size = c(8, 6), node.font = c("Helvetica", 14),
  digits = 1, main = "Maximum Likelihood"
)


fit_princ$scores
fit$scores

## Clustering after teh FA
nc <- NbClust(fit$scores, min.nc = 3, max.nc = 15, method = "kmeans")



kmeans_varimax <- kmeans(fit$scores, 3)
kmeans_varimax

D <- daisy(fit$scores)
tmp <- time_used[1]
sil_cl <- silhouette(kmeans_varimax$cluster, D)

rownames(sil_cl) <- tmp$Country
plot(
  sil_cl,
  col = c("#446455", "#FDD262", "#D3DDDC"), # report
  # col = c("#C77025", "#611BB8", "#7F7F7F"), #Slides
  cex.names = par("cex.axis"),
)

## Heatmap for the what can PT learn from the others
coul <- colorRampPalette(brewer.pal(9, "PuOr"))(50)

a <- fit$scores
rownames(a) <- tmp$Country
colnames(a) <- c("F1", "F2", "F3", "F4")
heatmap(a, scale = "column", col = coul)
abline(h = 0.68, col = "red", lwd = 2, lty = 2)
abline(h = 0.60, col = "red", lwd = 2, lty = 2) # cenas
legend(
  0, 1,
  ncol = 3,
  legend = c("-100%", "-75%", "-50%", "-25%", "0%", "25%", "50%", "75%", "100%"),
  fill = colorRampPalette(brewer.pal(9, "PuOr"))(9)
)
