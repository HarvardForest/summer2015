##### Imports and Dependencies #####

# To install Rgraphviz
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# Load the following:
library(enaR) # Load dependencies
library(Rgraphviz)
data(enaModels) # Declare data (.rda) - relative location

##### Functions #####

# Input: a network object from the enaModels library
# Output: a graph/diagram of the model-network object
enaPlot <- function(x) {

  attach(unpack(x)) # Allows access to unpacked network object elements

  # F - matrix of flows from each node to each node oreinted row to column.
  # z	- Node boundary inputs.
  # r	- Node boundary loss from respiration.
  # e	- Node boundary loss due to exportation
  # y	- Node boundary loss; summation of r and e
  # X	- Node storage or biomass

  ##### Visualize with Rgraphviz #####

  # Declare new graph
  g1 <- new("graphNEL", nodes=rownames(F), edgemode="directed")

  # Object used to store graph properties
  graphInfo <- agopen(g1, name="g1Properties")

  edges <- which(F > 0, arr.in=TRUE) # Find all edges

  # Add all edges to their appropriate Nodes
  for (i in 1:length(edges[,1])) {
    g1 <- addEdge (rownames(F)[edges[i,1]], colnames(F)[edges[i,2]], g1,
                   F[edges[i], edges[i,2]])
  }

  ## Add edge labels (by weight=flow) ##

  # Get edge weights, unlist them, provide in vector format
  ew <- as.character(unlist(edgeWeights(g1)))

  # Determine edges to remove - necessary if recipEdges=TRUE (Default)
  #ew <- ew[setdiff(seq(along=ew), removedEdges(g1))]

  # Get the set of edge names
  names(ew) <- edgeNames(g1)

  ## Adjust edge width by weight ##
  ##########...####################

  ## Set global attributes ##
  eAttrs <- list() # Global edge attributes
  nAttrs <- list() # Global node attributes
  attrs <- list(node=list(shape="ellipse", fixedsize=FALSE)) # General attributes

  eAttrs$label <- ew # Assign edge-weights to edge labels
  attrs$edge$fontsize <- 25 # Adjust edge label font-size

  # Remove node labels
  #nw <- strsplit(packageDescription("Rgraphviz")$Description, " ")[[1]]
  nw <- list()
  for (i in 1:numNodes(g1)){
    nw[i] <- " "
  }

  names(nw) <- nodes(g1)
  nAttrs$label <- nw

  plot(g1, recipEdges="distinct", edgeAttrs=eAttrs, attrs=attrs, nodeAttrs=nAttrs)

  detach(unpack(x))
}

###############################################################################

enaPlot(enaModels[[4]])

##### Bugs:
# 1) Reciprocal edges aren't completely labeled

##### Issues:
# 1) Rgraphviz does not support edge width
