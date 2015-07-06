## Requires BioInstaller package
biocLite("Rgraphviz")
library("Rgraphviz")

set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.2)

plot(g1)
