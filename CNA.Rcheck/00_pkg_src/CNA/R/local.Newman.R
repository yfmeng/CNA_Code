
local.Newman<-function(g,attribute.name,cate,contact,...){
  # network = g, attribute.name, contact, cate
  require('network')
  require('sna')
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (!is.null(get.edge.attribute(g,contact))){
    weight <- get.edge.attribute(g,contact)
  }else{weight<-rep(1,nrow(edgelist))}
  cate<-cut.cna(x1,cate,add = F)$cate
  cate2<-cut.cna(x2,cate,add = F)$cate
  if(cate2[1]<cate[1]){cate[1]<-cate2[1]}
  if(cate2[length(cate2)]>cate[length(cate)]){cate[length(cate)]<-cate2[length(cate2)]}
  x1<-cut.cna(x1,cate,add = F)$x
  x2<-cut.cna(x2,cate,add = F)$x
  l <- levels(x1)
  n<-length(cate)-1
  a<-rep(0,n)
  b<-rep(0,n)
  eii<-0
  for (i in 1:n){
    for (j in 1:n){
      contactij<-sum(weight[x1==l[i]&x2==l[j]])
      a[i]<-a[i]+weightij
      b[j]<-b[j]+weightij
      eii<-eii+weightij*(i==j)
    }
  }
  a <-a/sum(weight)
  b <-b/sum(weight)
  eii<-eii/sum(weight)
  divide<-(1-sum(a*b))*Ne
  
  ids<- 1:network.size(g)
  local.r <- c()
  for ( i in ids){
    range <- which(edgelist[,1]==i|edgelist[,2]==i)
    local.r[i]<-sum(x1[range]==x2[range])
  }
  local.r<-local.r/divide
}
