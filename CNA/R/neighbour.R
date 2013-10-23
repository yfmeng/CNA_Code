neighbour<-function(x1.id, x2.id, root.nodes, data, ...){
  
  x1<-data[,which(names(data)==x1.id)]
  x2<-data[,which(names(data)==x2.id)]
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
  neighbours<-neighbours[!duplicated(data$edge.index),]
  invisible(neighbours)
}