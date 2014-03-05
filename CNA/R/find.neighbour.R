find.neighbour<-function(g,root.nodes,size, ...){
  require('network')
  require('sna')
  edgelist <- as.edgelist.sna(g)
  x1<-edgelist[,1]
  x2<-edgelist[,2]
  if(!exists('direction')) {
    direction<-'undirected'
  }
  if(direction=='out'){
    linked<-which(x1%in%root.nodes)
  }
  if(direction=='in'){
    linked<-which(x2%in%root.nodes)
  }
  if(direction=='undirected'){
    linked<-which(x1%in%root.nodes)
    temp<-which(x2%in%root.nodes)
    linked<-union(linked,temp)
  }
  
  neighboured<-unique(c(x1[linked],x2[linked]))
  neighbours<-data[linked,]
  neighbours<-neighbours[!duplicated(neighbours$edge.index),]
  invisible(neighbours)
}