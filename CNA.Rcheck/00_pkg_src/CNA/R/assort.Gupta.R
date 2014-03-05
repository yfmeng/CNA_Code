assort.Gupta<-function(g,attribute.name,...){
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
  eii<-c()
  ai<-c()
  bi<-c()
  temp<-x1
  x1<-c(x1,x2)
  x2<-c(x2,temp)
  contact<-c(contact,contact)/2
  contact<-contact/sum(contact)
  for ( i in 1:n){
    eii[i]<-sum(contact[x1>=cate[i]&x1<=cate[i+1]&x2>=cate[i]&x2<=cate[i+1]])
    ai[i]<-sum(contact[x1>=cate[i]&x1<=cate[i+1]])
    bi[i]<-sum(contact[x2>=cate[i]&x2<=cate[i+1]])
  }
  Q<-sum((eii[eii!=0]-ai[eii!=0]*bi[eii!=0])/ai[eii!=0])/(n-1)
  Q<-data.frame(categories = n,sample.size = length(x1),contacts=sum(contact),Q)
  Q
}
