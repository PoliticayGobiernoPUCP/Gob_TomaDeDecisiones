#Networks  in R
#JM Magallanes
#Exporting the Network

linkAdjMx='https://docs.google.com/spreadsheets/d/e/2PACX-1vTLG9kDboD-JQ1feg4imSnbW8qU9QXHUxtm2VmQHIZML6crZrz9RPTbPPm-JOzq1WBX18W0g20LGBb5/pub?gid=0&single=true&output=csv'
EdgesAsDF = read.csv(linkAdjMx, 
                     header=TRUE,
                     row.names=1,
                     check.names=FALSE) 


# This matrix is squared?
dim(EdgesAsDF)

# last column:
table(EdgesAsDF['sexo'])


# bye
adjacency=EdgesAsDF[,-c(17)]



# to **matrix**.

adjacency=as.matrix(adjacency) 

# to network

library(igraph)
EliteNet=graph.adjacency(adjacency,
                         mode="undirected",
                         weighted=NULL)


vcount(EliteNet) #count of nodes

ecount(EliteNet) #count of edges

# see it:

plot.igraph(EliteNet,
            vertex.color = 'yellow',
            edge.color='lightblue')


# add attribute?

head(EdgesAsDF$sexo)

vertex_attr_names(EliteNet) 

# adding:

EliteNet=set_vertex_attr(EliteNet,
                         "sexo",
                         value=EdgesAsDF$sexo)


# worked? 
vertex_attr_names(EliteNet) 



# A visual follows:
Labels=V(EliteNet)$sexo
numberOfClasses = length(unique(Labels)) 
library(RColorBrewer)
colorForScale='Set2'
colors = brewer.pal(numberOfClasses, 
                    colorForScale)

plot.igraph(EliteNet,
            vertex.color = colors[Labels],
            edge.color='lightblue')


# Giant Component (component with max nodes):

#1. Get the sizes of each component:
Sizes=components(EliteNet)$csize


#2. Get the subnet with the largest component:
EliteNet_giant=induced.subgraph(EliteNet, 
                                which(Labels == which.max(Sizes)))

# see it:
plot.igraph(EliteNet_giant)


# Basic summary:
summary(EliteNet_giant)


## 2. Exploring the NETWORK

### Exploring the Network as a whole


# * Density: from 0 to 1, where 1 makes it a 'complete' network: 
#    there is a link between every pair of nodes.
graph.density(EliteNet_giant)


# * Diameter: worst case escenario for number of steps 
# for someone to contact another one (only for connected component).
diameter(EliteNet_giant)



#* Local clustering coefficient of a node is 
#a way to measure the level of connectivity its neighbors. 
#If all its neighbors are connected among one another, you get 1; 
# if none of them is connected you get zero. 
# Then, the average clustering coefficient tells you 
# the average of those values.

transitivity(EliteNet_giant,type = 'average')

# * Shorter path (average): it gets the average of every shortest path 
#among the nodes in the network. 
#A shorter path is the shortest _walk_ fromone node to another.

average.path.length(EliteNet_giant)


# **Random networks** have *small shortest path* and *small clustering coefficient*...Is this the case?. The high clustering coefficient would suggest a **small world**, as most nodes are not neighbors of one another, but most nodes can be reached from every other in few steps.

# * Transitivity: How probable is that two business men 
#with a common business friend, 
#are also friends.
transitivity(EliteNet_giant)


#* Assortativity (degree): it is a measure to see if nodes are 
# connecting to other nodes similar in degree.  
# Closer to 1 means higher assortativity, 
# closer to -1 diassortativity; while 0 is no assortitivity.
assortativity_degree(EliteNet_giant)


# You can also compute assortativity using an attribute of interest:
attrNet=V(EliteNet_giant)$multi
assortativity(EliteNet_giant,attrNet)