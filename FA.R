install.packages("psych", dependencies = TRUE)
install.packages("GPArotation", dependencies = TRUE)
install.packages("nFactors", dependencies = TRUE)
install.packages("wesanderson", dependencies = TRUE)
install.packages("NbClust", dependencies = TRUE)

library(nFactors) # to help determine the number of factors/components to retain
library(psych)
library(GPArotation)
library(readxl)
library(cluster)
library(NbClust)

# Just to add more colours
library("wesanderson")
library(RColorBrewer)

time_used <- read_excel("datasets/Time_Use.xlsx")

dados <- time_used[, -1]
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
cortest.bartlett(dados)

r_matrix <- cov(dados)

ev <- eigen(cor(dados)) # get eigenvalues
ev$values

## Scree Test
parallel_analysis <- nScree(dados, model = "factors")
# Note that model factors is required otherwise the result will be for components
## If the above command does not work due to version, use the one below
# parallel_analysis <- nScree(data.frame(dados), model = "factors") # Note that model factors is required
parallel_analysis
plotnScree(parallel_analysis)

n_factors <- 4 # This is for four factors. You can change this as needed.
fit <- factanal(dados, n_factors, rotation = "varimax", scores = c("Bartlett"))
print(fit, digits = 2, cutoff = 0.3, sort = TRUE)

loads <- fit$loadings
fa.diagram(loads)

fit$scores

## Clustering after teh FA
nc <- NbClust(fit$scores, min.nc = 3, max.nc = 15, method = "kmeans")



kmeans_varimax <- kmeans(fit$scores, 3)
kmeans_varimax

D <- daisy(fit$scores)
tmp <- time_used[1]
sil_cl <- silhouette(kmeans_varimax$cluster, D)

rownames(sil_cl) <- tmp$Country
plot(sil_cl, col = c("#446455", "#FDD262", "#D3DDDC"), cex.names = par("cex.axis"))

## Heatmap for the what can PT learn from the others
coul <- colorRampPalette(brewer.pal(11, "BrBG"))(50)

a <- fit$scores
rownames(a) <- tmp$Country
heatmap(a, scale = "column", col = coul, par = 1)
abline(h = 0.66, col = "red", lwd = 2, lty = 2)
abline(h = 0.58, col = "red", lwd = 2, lty = 2)
legend(
    0.15, 0.95,
    ncol = 3, legend = c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4"),
    fill = colorRampPalette(brewer.pal(11, "BrBG"))(9)
)

# ## Also not used in cluster after FA section
# a <- prcomp(fit$scores, scale = TRUE)
# plot(a$x, col = kmeans_varimax$cluster, pch = 19, main = "kmeans, 5 grupos")
# text(a$x[, 1],
#     a$x[, 2] + 0.1,
#     tmp$Country,
#     col = "blue"
# )


# ### This was not used on the final version of the article
# ## Graph Comparing the multipel options of Rotation
# dados_fa_none <- factanal(dados, factors = 4, rotation = "none")
# dados_fa_varimax <- factanal(dados, factors = 4, rotation = "varimax")
# dados_fa_promax <- factanal(dados, factors = 4, rotation = "promax")

# par(mfrow = c(1, 3))
# plot(dados_fa_none$loadings[, 1],
#     dados_fa_none$loadings[, 2],
#     xlab = "Factor 1",
#     ylab = "Factor 2",
#     ylim = c(-1, 1),
#     xlim = c(-1, 1),
#     main = "No rotation"
# )
# text(dados_fa_none$loadings[, 1],
#     dados_fa_none$loadings[, 2],
#     colnames(dados),
#     col = "blue"
# )
# abline(h = 0, v = 0)

# plot(dados_fa_varimax$loadings[, 1],
#     dados_fa_varimax$loadings[, 2],
#     xlab = "Factor 1",
#     ylab = "Factor 2",
#     ylim = c(-1, 1),
#     xlim = c(-1, 1),
#     main = "Varimax rotation"
# )

# text(dados_fa_varimax$loadings[, 1],
#     dados_fa_varimax$loadings[, 2],
#     colnames(dados),
#     col = "blue"
# )
# abline(h = 0, v = 0)

# plot(dados_fa_promax$loadings[, 1],
#     dados_fa_promax$loadings[, 2],
#     xlab = "Factor 1",
#     ylab = "Factor 2",
#     ylim = c(-1, 1),
#     xlim = c(-1, 1),
#     main = "Promax rotation"
# )
# text(dados_fa_promax$loadings[, 1],
#     dados_fa_promax$loadings[, 2],
#     colnames(dados),
#     col = "blue"
# )
# abline(h = 0, v = 0)
