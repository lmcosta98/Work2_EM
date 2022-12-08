library(readxl)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)

time_used <- read_excel("datasets/Time_Use_in_OECD_Countries_OECD_after.xlsx")
View(time_used[, -1])

dados <- time_used[, -1]

## Non Normalized data
rMatrix <- cov(dados)

(AF <- principal(rMatrix, nfactors = 2, residuals = TRUE, rotate = "quartimax", covar = TRUE))
