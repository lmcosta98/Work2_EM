install.packages("psych")
install.packages("GPArotation")
install.packages("nFactors")

library(nFactors) # to help determine the number of factors/components to retain
library(psych)
library(GPArotation)
library(readxl)

time_used <- read_excel("datasets/Time_Use_in_OECD_Countries_OECD_after.xlsx")
View(time_used)

dados <- time_used[, -1]

## To Normalize the data
r_matrix <- cov(dados)

af_quartimax <- principal(
    r_matrix,
    nfactors = 2,
    residuals = TRUE,
    rotate = "quartimax",
    covar = TRUE,
)


########
# Obten��o das estimativas das pontua��es (factor scores)
factor.scores(r_matrix, f = af_quartimax, method = c("Bartlett"))

########
# Extra��o dos fatores via Maxima verosimilhan�a
af_varimax <- factanal(
    r_matrix,
    factors = 2,
    scores = c("Bartlett"),
    rotation = "varimax",
)
af_varimax
# NOTA: "rotation" pode ser: "none" ou outra.
str(af_varimax)

########
# Obten��o das estimativas das pontua��es (factor scores)
af_varimax$scores
factor.scores(dados, f = af_varimax, method = c("Bartlett"))


########
# Adequa��o do modelo fatorial
# Regras de determina��o do numero de factores a reter:
parallel_analysis <- nScree(r_matrix)
parallel_analysis
plotnScree(parallel_analysis)

# C�lculo do KMO e MSA
print(KMO(r_matrix), digits = 3)

# Teste de Bartlett
cortest.bartlett(cor(r_matrix), n = nrow(dados))




## Graph Comparing the multipel options of Rotation
dados_fa_none <- factanal(dados, factors = 2, rotation = "none")
dados_fa_varimax <- factanal(dados, factors = 2, rotation = "varimax")
dados_fa_promax <- factanal(dados, factors = 2, rotation = "promax")

par(mfrow = c(1, 3))
plot(dados_fa_none$loadings[, 1],
    dados_fa_none$loadings[, 2],
    xlab = "Factor 1",
    ylab = "Factor 2",
    ylim = c(-1, 1),
    xlim = c(-1, 1),
    main = "No rotation"
)
text(dados_fa_none$loadings[, 1] - 0.08,
    dados_fa_none$loadings[, 2] + 0.08,
    colnames(dados),
    col = "blue"
)
abline(h = 0, v = 0)

plot(dados_fa_varimax$loadings[, 1],
    dados_fa_varimax$loadings[, 2],
    xlab = "Factor 1",
    ylab = "Factor 2",
    ylim = c(-1, 1),
    xlim = c(-1, 1),
    main = "Varimax rotation"
)

text(dados_fa_varimax$loadings[, 1] - 0.08,
    dados_fa_varimax$loadings[, 2] + 0.08,
    colnames(dados),
    col = "blue"
)
abline(h = 0, v = 0)

plot(dados_fa_promax$loadings[, 1],
    dados_fa_promax$loadings[, 2],
    xlab = "Factor 1",
    ylab = "Factor 2",
    ylim = c(-1, 1),
    xlim = c(-1, 1),
    main = "Promax rotation"
)
text(dados_fa_promax$loadings[, 1] - 0.08,
    dados_fa_promax$loadings[, 2] + 0.08,
    colnames(dados),
    col = "blue"
)
abline(h = 0, v = 0)
