### http://127.0.0.1:15876/library/Rgraphviz/doc/Rgraphviz.pdf

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
rEG <- addEdge ("B", "D", rEG, 3)
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

# Attributes can be set globally or individually
  # Global attributes are assigned to a list and passed to attrs in plot
    # List has four elements: graph, cluster, edge, node
      # Each element is another list (attributes)

defAttrs <- getDefaultAttrs() # Returns graphviz default attributes - these fill in unspecified values

# Global attribute definitions
plot(rEG, attrs=list(node=list(label="foo", fillcolor="lightgreen"), edge=list(color="cyan"), graph=list(rankdir="LR")))

# For attribute definitions case-by-case, attributes are set using a list where the names of the elements are the attributes
# Each element contains a named vector

# Build local attributes
eAttrs <- list()
eAttrs$label <- c("A~B"="e1")

nAttrs <- list()
z <- strsplit(packageDescription("Rgraphviz")$Description, " ")[[1]]
z <- z[1:numNodes(rEG)]
names(z) = nodes(rEG)
nAttrs$label <- z

# Build global attributes
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))

# Apply both local and global attributes
plot(rEG, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)

### Using edge weights for labels

# Get edge weights, unlist them, provide in vector format
ew <- as.character(unlist(edgeWeights(rEG)))

# Determine edges to remove - necessary if recipEdges=TRUE (Default)
ew <- ew[setdiff(seq(along=ew), removedEdges(rEG))]

# Get the set of edge names
names(ew) <- edgeNames(rEG)

eAttrs$label <- ew
attrs$edge$fontsize <- 27
plot(rEG, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)

### Adding color

## Specify node drawing color

nAttrs$color <- c(A="red", B="red", C="green", D="blue")
# Specify edge drawing color
eAttrs$color <- c("A~B"="blue", "A~C"="purple")
# Specify node fill color
nAttrs$fillcolor <- c(C="yellow")
# label color
nAttrs$fontcolor <- c(D="green", C="red")
eAttrs$fontcolor <- c("A~B"="green", "B~D"="red")

plot(rEG, nodeAttrs=nAttrs, edgeAttrs=eAttrs, attrs=attrs)

### Node shapes

# Supported: cirlce (Default), ellipse, plaintext, box

attrs$node$shape <- "ellipse"

nAttrs$shape <- c(A="box", B="circle", C="box", D="plaintext")

plot(rEG, attrs=attrs, nodeAttrs=nAttrs)
