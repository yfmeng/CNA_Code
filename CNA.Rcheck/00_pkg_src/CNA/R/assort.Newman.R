
assort.Newman<-function(g,attribute.name,...){
  # network = g, attribute.name, contact, cate
  require('network')
  require('sna')
  edgelist <- as.edgelist.sna(g)
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,attribute.name))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
    }else{contact<-rep(1,nrow(edgelist))}
  
  # default size of bins = 1
  if(!exists('cate')){cate<-floor(min(c(x1,x2))):ceiling(max(c(x1,x2)))}
  n<-length(cate)-1
  beta<-matrix(0,n,n)
  a<-rep(0,n)
  b<-rep(0,n)
  eii<-0
  for (i in 1:n){
    for (j in 1:n){
      contactij<-sum(contact[x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j]])
      a[i]<-a[i]+contactij
      b[j]<-b[j]+contactij
      eii<-eii+contactij*(i==j)
    }
  }
  a <-a/sum(contact)
  b <-b/sum(contact)
  eii<-eii/sum(contact)
  r<-(eii-sum(a*b))/(1-sum(a*b))
  r<-data.frame(categories = n,sample.size = network.size(g),contacts=sum(contact),Newman=r)
  r
}

