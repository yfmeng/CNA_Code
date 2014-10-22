# test
folder<-'/Users/feimeng/CNA_code'
setwd(folder)
g<-network(100,density=0.02,directed=F)
set.edge.attribute(g,'sf',runif(96,1,2))
set.vertex.attribute(g,'age',runif(100,0,100))

attribute.name<-'age'