##### Import network data from enaR #####

library(enaR) # Load dependencies
data(oyster) # Declare data (.rda) - relative location

summary(oyster) # View the data

unpackedOyster <- unpack(oyster) # Organized network object (oyster) into list
attach(unpackedOyster) # Allows access to unpacked network object elements

# F - matrix of flows from each node to each node oreinted row to column.
# z	- Node boundary inputs.
# r	- Node boundary loss from respiration.
# e	- Node boundary loss due to exportation
# y	- Node boundary loss; summation of r and e
# X	- Node storage or biomass

##### Visualize with Rgraphviz #####

library(Rgraphviz)

g1 <- new("graphNEL", nodes=rownames(F), edgemode="directed") # Declare new graph

graphInfo <- agopen(g1, name="g1Properties") # Object used to store graph properties

edges <- which(F > 0, arr.in=TRUE) # Find all edges

for (i in 1:length(edges[,1])) {
  x[i] <- F[edges[i], edges[i,2]]
  print(edges[i])
  g1 <- addEdge (rownames(F)[edges[i,1], colnames(F)edges[i,2]], g1, F[edges[i], edges[i,2]])
}

plot(g1)

