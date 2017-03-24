

data("gaschrom", package = "ptw")
query <- log(t(gaschrom))
image(1:nrow(query), 1:ncol(query), query, xlab = "Index", 
      ylab = "Sample", main = "gaschrom data")
box()
