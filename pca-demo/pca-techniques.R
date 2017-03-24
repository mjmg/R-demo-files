library(ggplot2)

princomp(USArrests, cor = TRUE)$loadings

prcomp(USArrests, scale = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))

# Try qplot
qplot(prcomp(USArrests))

# Compute PCA using SVD decomposition
svd(cor(USArrests))$u

# Compute PCA using eigen decomposition
eigen(cor(USArrests))$vectors
