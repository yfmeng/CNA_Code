bridge<-function(x1id,x2id,x1,x2,contact,data,gap,...){
  x1.name<-deparse(substitute(x1))
  x2.name<-deparse(substitute(x2))
  x1id.name<-deparse(substitute(x1id))
  x2id.name<-deparse(substitute(x2id))
  contact.name<-deparse(substitute(contact))
  data.name<-names(data)
  if(x1.name %in% data.name){x1<-data[,which(data.name==x1.name)]}
  if(x2.name %in% data.name){x2<-data[,which(data.name==x2.name)]}
  if(x1id.name %in% data.name){x1id<-data[,which(data.name==x1id.name)]}
  if(x2id.name %in% data.name){x2id<-data[,which(data.name==x2id.name)]}
  if(contact.name %in% data.name){contact<-data[,which(data.name==contact.name)]}
  
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
  node.bridge<-data.frame(node.id,node.degree,node.gap,node.bridge)
}