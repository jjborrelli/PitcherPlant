# Now in my github repo - test commit branch1
library(igraph)
library(NetIndices)

sarracenia<-
  graph.formula(A-+B,
                B-+C,
                C-+D,
                E-+D,
                A-+E,
                C-+E,
                E-+F,
                F-+G,
                E-+G,
                C-+G,
                G-+H,
                F-+H,
                E-+H,
                I-+I,
                H-+I,
                G-+I,
                F-+I,
                E-+I,
                A-+I
                )

par(mar=c(.1,.1,.1,.1))
plot.igraph(sarracenia,vertex.size=10,edge.arrow.size=.5,layout=layout.fruchterman.reingold)

degree(sarracenia,mode="in")

lay1<-matrix(nrow=9,ncol=2)
lay1[,1]<-c(2,3,3,3,2,1,3,2,2.5)
lay1[,2]<-c(1,2,3,4,3,4,5,6,7)

plot.igraph(sarracenia,vertex.size=10,edge.arrow.size=.5,layout=lay1)

gi<-GenInd(get.adjacency(sarracenia,sparse=F))

ti<-TrophInd(get.adjacency(sarracenia,sparse=F))

lay1[,2]<-ti$TL

# Checking out the cheddar package with sarracenia food web 
library(cheddar)
# To make a community I need 3 things
# First a matrix or data frame with the node labels in a column named "node"
nodeS<-matrix(c("A","B","C","D","E","F","G","H","I"),nrow=9,ncol=1)
colnames(nodeS)<-"node"

# Second I need the trophic links with a column of resources and matching column of consumer (named)
# note come up with a better way to make this
trophlink<-matrix(nrow=18,ncol=2)
trophlink[,1]<-c("A","A","A","B","C","C","C","E","E",'E',"E","E","F","F","F","G","G","H")
trophlink[,2]<-c("B","E","I","C","D","E","G","D","F","G","H","I","G","H","I","H","I","I")
colnames(trophlink)<-c("resource","consumer")

# Third I need community properties, this can be as simple as a list containing the title of the community
props<-list("Sarracenia")
names(props)<-c("title")

# Now I can make the community 
pitcherplant<-Community(nodes=nodeS,properties=props,trophic.links=trophlink)

# And use all of cheddar's fun toys
ChainAveragedTrophicLevel(pitcherplant)
TLched<-TrophicLevels(pitcherplant)
levelsmat<-matrix(c(TLched,ti$TL),nrow=9,ncol=7)
colnames(levelsmat)<-c(colnames(TLched),"igraph")
rownames(levelsmat)<-rownames(TLched)
rowSums(levelsmat[,1:6])/6

Degree(pitcherplant)
TrophicVulnerability(pitcherplant) #same as out degree

DirectedConnectance(pitcherplant)

FractionBasalNodes(pitcherplant)
FractionIntermediateNodes(pitcherplant)
FractionTopLevelNodes(pitcherplant)

SumDietGaps(pitcherplant)

LinkageDensity(pitcherplant)

plot(pitcherplant)

remPP<-RemoveNodes(pitcherplant,"E","remPP")

ThreeNodeChains(pitcherplant)
TrophicChainsStats(pitcherplant)
TrophicChains(pitcherplant)

threemot<-graph.formula(a+-b,b+-c)

graph.count.subisomorphisms.vf2(sarracenia,threemot)

pmpp<-PredationMatrix(pitcherplant)
order(rowSums(pmpp))
order(colSums(pmpp))

eigen(pmpp)

