### Requires BioInstaller package

library("Rgraphviz")

set.seed(123)
V <- letters[1:5] # Determines number of Nodes
M <- 1:4 # Set (vector) of values assigned to V to determine build
p <- .2 # Value between 0-1 - determines probability of selecing an element of M
# Any two elements of V that share an element of M (based on p) are connected
g1 <- randomGraph(V, M, p)

plot(g1)

### Reciprocated edges

rEG <- new("graphNEL", nodes=c("A", "B", "C", "D"), edgemode="directed") # Declares new graph
rEG <- addEdge ("A", "B", rEG, 1) # Declares new edge: to, from, graph, weight
rEG <- addEdge ("A", "C", rEG, 1)
rEG <- addEdge ("B", "D", rEG, 1)
rEG <- addEdge ("D", "A", rEG, 1)
rEG <- addEdge ("C", "A", rEG, 1)

# Options: combines (double-sided edge) and distinct (seperate edges)
plot(rEG, recipEdges="distinct")

### Subgraphs - Graphviz attempts to find a layout with subgraph-nodes relatively close

sg1 <- subGraph(c("A", "B", "D"), rEG)

# In order to plot with a subgraph, a subgraph list must be build
  # Attribute1 = name of subgraph in that index
  # Attribute2 = cluster=TRUE or FALSE (Default is true)
subGList <- vector(mode="list", length=1)
subGList[[1]] <- list(graph=sg1)

plot(rEG, subGList=subGList)

### Edge Names

edgeNames(rEG) # Vector of all x~y, where x=tail y=head
edgeNames(rEG, recipEdges="distinct") # Includes reciprocated edges as dinstict edges

### Global Attributes
# Link to all attributes: http://www.graphviz.org/pub/scm/graphviz2/doc/info/attrs.html


