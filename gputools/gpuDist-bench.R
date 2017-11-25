library("gputools")

numVectors <- 500
dimension <- 10000
Vectors <- matrix(runif(numVectors*dimension), numVectors, dimension)

system.time(dist(Vectors, "euclidean"))
system.time(gpuDist(Vectors, "euclidean"))

system.time(dist(Vectors, "maximum"))
system.time(gpuDist(Vectors, "maximum"))

system.time(dist(Vectors, "manhattan"))
system.time(gpuDist(Vectors, "manhattan"))

system.time(dist(Vectors, "minkowski", 4))
system.time(gpuDist(Vectors, "minkowski", 4))




