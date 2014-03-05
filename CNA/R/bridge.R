bridge<-function(g,attribute.name,gap,...){
  require('network')
  require('sna')
  edgelist <- as.edgelist.sna(g)
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  x1id<-edgelist[,1]
  x2id<-edgelist[,2]
  
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,attribute.name))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
  }else{contact<-rep(1,nrow(edgelist))}
  
  node.id<-unique(c(x1id,x2id))
  node.degree<-rep(0,length(node.id))
  node.bridge<-rep(FALSE,length(node.id))
  node.gap<-rep(0,length(node.id))
  
  for (i in 1:length(node.id)){
    id<-node.id[i]
    this.x<-c(x2[x1id==id],x1[x2id==id])
    this.pair<-c(x2id[x1id==id],x1id[x2id==id])
    node.gap[i]<-max(this.x)-min(this.x)
    node.degree[i]<-length(unique(this.pair))
    if (node.gap[i]>=gap){
      node.bridge[i]<-TRUE
    }
  }
  bridge<-data.frame(node.id,node.degree,node.gap)
  bridge<-bridge[node.bridge,]
  bridge
}