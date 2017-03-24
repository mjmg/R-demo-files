rm(list=ls()) # its always good practice to clear R’s memory

data(iris)
#?iris # gives info about the dataset
str(iris)
head(iris)
plot(iris)

# by plotting the variables against each other it becomes obvious 
# that some are strongly correlated: in other words, there is an 
# overlap in the power of some variables at explaining/accounting for 
# the data variability. A PCA will help disentangling these correlations.

# functions prcomp() performs PCA:
fit<-prcomp(iris[-5], scale=TRUE)
# scale=T standardizes the variables to the same relative scale, 
#avoiding some variables to become dominant just because of their 
#large measurement units.

summary(fit)
# the summary indicates that four PCs where created: the number 
# of possible PCs always equals the number of original variables.

# PC1 and PC2 explain respectively ~73% and ~23% of the data's 
# total variability, summing up to a more-than-respectable 96% of
# the total variability. There is no fixed rule about this, but 
# this already tells us that all the other PCs can be ignored as 
# they explain only crumbs of the total data variability.

plot(fit,type="lines")
# a "scree plot" allows a graphical assessment of the relative 
#contribution of the PCs in explaining the variability of the data.

fit[2] # the "Rotation" matrix contains the "loadings" of each 
#of the original variables on the newly created PCs.
#The concept of eigenvalue would require to be introduced for
#understanding how the loadings are estimated, and in general
#for a quantitative understanding of how the principal
#components are calculated: the interested reader might look
#it up in Ref. 2 and 3.

biplot(fit)
# the arrows provide a graphical rendition of the 
# loadings of each of the original variables on the used PCs.

# Package GGPLOT2 and its derivative GGBIPLOT have a somehow esoteric
# syntax, but they produce much fancier plots:

library(ggplot2) # this might need to be installed

# Variances of principal components
variances <- data.frame(variances=fit$sdev**2, pcomp=1:length(fit$sdev))
# **2 means ^2

#Plot of variances
varPlot <- ggplot(variances, aes(pcomp, variances))
+ geom_bar(stat="identity", fill="gray") + geom_line()
varPlot

# run these two lines only if ggbiplot is not installed yet:
#library(devtools)
#install_github("ggbiplot", "vqv")
# load ggbiplot
library(ggbiplot)

Species<-iris$Species
iris_pca <- ggbiplot(fit,obs.scale = 1, 
                     var.scale=1,groups=Species,ellipse=F,circle=F,varname.size=3)
# iris_pca <- iris_pca + theme_bw() # try this for a less 
# ink-demanding background
iris_pca

