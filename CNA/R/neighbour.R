neighbour<-function(x1.id, x2.id, root.nodes, data, ...){
  
  x1<-data[,which(names(data)==x1.id)]
  x2<-data[,which(names(data)==x2.id)]
  if(!exists('direction')) {
    direction<-'undirected'
  }
  if(!exists('size')) {
    size<-length(unique(c(x1,x2)))
  }
  size<-as.numeric(size)
  size<-size-length(unique(root.nodes))
  if(size<0){
    warning('Required neighbourhood is smaller than the original nodes.')
    stop
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
  
  if (size>=length(neighboured)){
    included<-linked
  }else{
  included<-sample.int(length(linked),size)
  included<-linked[included]
  }
  neighbours<-data[included,]
  invisible(neighbours)
}