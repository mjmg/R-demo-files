library(VPdtw)
library(ptw)

data("query")
data("reference")
reference <- log(reference)
query <- log(query)

penalty <- rep(0, sum(!is.na(reference)))
penalty <- cbind(penalty, dilation(reference, 350)/5)
result <- VPdtw(reference, query, penalty = penalty, maxshift = 350)
print(result)
plot(result, "After")
plot(result, "Shift")

plot(result, xlim = c(800, 1600), type = "Before")


data("gaschrom", package = "ptw")
query <- log(t(gaschrom))
image(1:nrow(query), 1:ncol(query), query, xlab = "Index", 
      ylab = "Sample", main = "gaschrom data")
box()


penalty <- dilation(apply(query, 1, median), 150)
penalty <- cbind(penalty/3, penalty/4, penalty/5, penalty/6)
result0 <- VPdtw(reference = query[, 1], query = query[,
            ncol(query)], penalty = penalty, maxshift = 150)
plot(result0, "After")


penalty <- dilation(apply(query, 1, median), 150)/6
result1 <- VPdtw(reference = NULL, query = query, penalty = penalty, maxshift = 150)
plot(result1, "After")




