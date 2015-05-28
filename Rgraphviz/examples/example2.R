## Requires BioInstaller package
library("Rgraphviz")

set.seed(123)
V <- letters[1:5] # Determines number of Nodes
M <- 1:4 # Set (vector) of values assigned to V to determine build
p <- .2 # Value between 0-1 - determines probability of selecing an element of M
# Any two elements of V that share an element of M (based on p) are connected
g1 <- randomGraph(V, M, p)

plot(g1)

## Reciprocated edges
# Options: combines (double-sided edge) and distinct (seperate edges)

g1 <- addEdge("a", "b", g1, 1)
