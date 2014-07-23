assort.Gupta<-function(g,attribute.name,cate,contact,verbose,...){
  require('network')
  require('sna')
  if(missing(contact)){contact<-''}
  if(missing(verbose)){verbose<-FALSE}
  if (verbose){
    cat('Network assortativity with Gupta\'s Q method')
  }
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (!is.null(get.edge.attribute(g,contact))){
      weight <- get.edge.attribute(g,contact)
    }else{
      weight<-rep(1,nrow(edgelist))
      if (verbose) cat('edges unweighted')
    } 
  n<-length(cate)-1
  eii<-c()
  ai<-c()
  bi<-c()
  temp<-x1
  x1<-c(x1,x2)
  x2<-c(x2,temp)
  weight<-c(weight,weight)/2
  weight<-weight/sum(weight)
  for ( i in 1:n){
    eii[i]<-sum(weight[x1>=cate[i]&x1<=cate[i+1]&x2>=cate[i]&x2<=cate[i+1]])
    ai[i]<-sum(weight[x1>=cate[i]&x1<=cate[i+1]])
    bi[i]<-sum(weight[x2>=cate[i]&x2<=cate[i+1]])
  }
  Q<-sum((eii[eii!=0]-ai[eii!=0]*bi[eii!=0])/ai[eii!=0])/(n-1)
  if(sum(eii[eii!=0])==sum(ai[eii!=0])){Q<-1}
  Q
}
