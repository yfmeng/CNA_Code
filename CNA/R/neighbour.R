neighbour<-function(x1.id, x2.id, root.nodes, data, size){
  x1<-data[,which(names(data)==x1.id)]
  size<-as.numeric(size)
  size<-size-length(unique(root.nodes))
  if(size<0){
    warning('Required neighbourhood is smaller than the original nodes.')
    stop
  }
  linked<-which(x1%in%root.nodes)
  if (size>=length(linked)){
    included<-linked
  }else{
  included<-sample.int(length(linked),size)
  included<-linked[included]
  }
  neighbours<data[included,]
  invisible(neighbours)
}