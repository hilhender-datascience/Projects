# Load igraph
library(igraph)
# set.seed(123)

#Importing DataSet
gump <- read.csv("gump.csv", stringsAsFactors = FALSE)
# Inspect Forrest Gump Movie dataset
head(gump)

# Make an undirected network
g <- graph_from_data_frame(gump, directed = FALSE)

# Identify key nodes using eigenvector centrality
g.ec <- eigen_centrality(g)
which.max(g.ec$vector)

# Plot Forrest Gump Network
plot(g,
     vertex.label.color = "black", 
     vertex.label.cex = 0.6,
     vertex.size = 25*(g.ec$vector),
     edge.color = 'gray88',
     main = "Forrest Gump Network"
)

# Get density of a graph
gd <- edge_density(g)
gd <- gd/2
# 0.0309997712194005937968999120357693755067884922027587890625
# 0.0619995424388011875937998240715387510135769844055175781250

# Get the diameter of the graph g
diameter(g, directed = FALSE)

# Get the average path length of the graph g
g.apl <- mean_distance(g, directed = FALSE)
g.apl


# Create one random graph with the same number of nodes and edges as g
g.random <- erdos.renyi.game(n = gorder(g), p.or.m = gd, type = "gnp")

g.random

plot(g.random)

# Get density of new random graph `g.random`
edge_density(g.random)

#Get the average path length of the random graph g.random
average.path.length(g.random, directed = FALSE)

# Generate 1000 random graphs
gl <- vector('list', 1000)

for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(n = gorder(g), p.or.m = gd, type = "gnp")
}

# Calculate average path length of 1000 random graphs
gl.apls <- unlist(lapply(gl, average.path.length, directed = FALSE))

# Plot the distribution of average path lengths
hist(gl.apls, xlim = range(c(1.5, 6)))
abline(v = g.apl, col = "red", lty = 3, lwd = 2)

# Calculate the proportion of graphs with an average path length lower than our observed
mean(gl.apls < g.apl)

# Show all triangles in the network.
matrix(triangles(g), nrow = 3)

# Count the number of triangles that vertex "BUBBA" is in.
count_triangles(g, vids='BUBBA')

# Calculate  the global transitivity of the network.
g.tr <- transitivity(g)
g.tr

# Calculate the local transitivity for vertex BUBBA.
transitivity(g, vids='BUBBA', type = "local")

# Calculate average transitivity of 1000 random graphs
gl.tr <- lapply(gl, transitivity)
gl.trs <- unlist(gl.tr)

# Get summary statistics of transitivity scores
summary(gl.trs)

# Calculate the proportion of graphs with a transitivity score higher than Forrest Gump's network
mean(gl.trs > g.tr)