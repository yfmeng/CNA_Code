summary.local<-function(g,attribute.name,bi=c(T,'gender'),...){
  require('network')
  require('sna')
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  x <- get.vertex.attribute(g,attribute.name)
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,attribute.name))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
  }else{contact<-rep(1,nrow(edgelist))}
  
  if(bi[1]){
    swap <- get.vertex.attribute(g,bi[2])[edgelist[,1]]==1
    temp <- x1[swap]
    x1[swap]<-x2[swap]
    x2[swap]<-temp
  }
  
  ids<-1: network.size(g)
  mean.dif<-c()
  max.dif<-c()
  gap.dif<-c()
  op<-options(warn = (-1))
  for (i in ids){ 
    range<-which(edgelist[,1]==i|edgelist[,2]==i)
    mean.dif[i]<-mean(x1[range]-x2[range])
    max.dif[i]<-max(x1[range]-x2[range])
    gap.dif[i]<- max.dif[i] - min(x1[range]-x2[range])
  }
  options(op)
  max.dif[is.infinite(max.dif)]<-NA
  gap.dif[is.infinite(gap.dif)]<-0
  local.summary<-data.frame(ids,mean.dif,max.dif,gap.dif)
}