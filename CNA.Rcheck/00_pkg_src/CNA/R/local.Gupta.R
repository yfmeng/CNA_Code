local.Gupta<-function(g,attribute.name,cate,...){
  require('network')
  require('sna')
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x <-get.vertex.attribute(g,attribute.name)
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,contact))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
  }else{contact<-rep(1,nrow(edgelist))}
  
  n<-length(cate)-1
  eii<-c()
  ai<-c()
  bi<-c()
  temp<-x1
  x1<-c(x1,x2)
  x2<-c(x2,temp)
  contact<-c(contact,contact)/2
  contact<-contact/sum(contact)
  cate<-cut.cna(x1,cate,add = F)$cate
  cate2<-cut.cna(x2,cate,add = F)$cate
  if(cate2[1]<cate[1]){cate[1]<-cate2[1]}
  if(cate2[length(cate2)]>cate[length(cate)]){cate[length(cate)]<-cate2[length(cate2)]}
  x1<-cut.cna(x1,cate,add = F)$x
  x2<-cut.cna(x2,cate,add = F)$x
  l <- levels(x1)
  for ( i in 1:n){
    eii[i]<-sum(contact[x1==l[i]&x2==l[i]])
    ai[i]<-sum(contact[x1==l[i]])
    bi[i]<-sum(contact[x2==l[i]])
  }
  ids<- 1:network.size(g)
  local.Q <- c()
  for (i in ids){
    range <- which(edgelist[,1]==i|edgelist[,2]==i)
    this <- sum(x1[range]==x2[range])/ai[which(l==x[i])]
    if(length(this)==0|is.na(this)){
      local.Q[i]<-0
    }else{
      local.Q[i]<-this
    }
  }
  local.Q<-local.Q/(n-1)/sum(ai)/Ne
  local.Q
}
