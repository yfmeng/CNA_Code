assort.Farrington<-function(g,attribute.name,bi=c(F,'gender'),directed=TRUE,standard = 'sd',...){
  require('network')
  require('sna')
  edgelist <- as.edgelist.sna(g)
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  size.x<-length(x1)/2
  x1 <- x1[1:size.x]
  x2 <- x2[1:size.x]
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,attribute.name))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
  }else{contact<-rep(1,nrow(edgelist))}
  
  # default size of bins = 1
  if(!exists('cate')){cate<-floor(min(c(x1,x2))):ceiling(max(c(x1,x2)))}
  # 
  if(!directed){
    # homogenous nodes (temporary solution)
    temp<-x1
    x1<-c(x1,x2)
    x2<-c(x2,temp)
    contact<-c(contact,contact)/2
  }
  # mean value in each bin
  x1.temp<-get.vertex.attribute(g,attribute.name)
  x2.temp<-x1.temp
  if (bi[1]){
    bi.attr<-get.vertex.attribute(g,bi[2])
    x1.temp<-x1.temp[bi.attr==unique(bi.attr)[1]]
    x2.temp<-x2.temp[bi.attr==unique(bi.attr)[2]]
  }
  s1<-c()
  s2<-c()
  for (i in 1:(length(cate)-1)){
    s1[i]<-mean(x1.temp[x1.temp>cate[i]&x1.temp<=cate[i+1]])
    s2[i]<-mean(x2.temp[x2.temp>cate[i]&x2.temp<=cate[i+1]])
    if (is.na(s1[i])){s1[i]<-(cate[i]+cate[i+1])/2}
    if (is.na(s2[i])){s2[i]<-(cate[i]+cate[i+1])/2}
  }
  
  # n = number of categories 
  # distr = cumulative distribution of x1&x2
  n<-length(cate)-1
  distr1<-rep(0,n)
  distr2<-rep(0,n)
  for (i in 1:n){
    distr1[i]<-sum(x1>cate[i]&x1<=cate[i+1])
    distr2[i]<-sum(x2>cate[i]&x2<=cate[i+1])
  }
  proportion1<-distr1/sum(distr1)
  proportion2<-distr2/sum(distr2)
  beta<-matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      contactij<-sum(contact[x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j]])
      beta[i,j]<-contactij/sum(x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j])
    }
  }
  beta[is.na(beta)]<-0
  
  total<-0
  fc<-matrix(0,ncol = ncol(beta),nrow=nrow(beta))
  for (i in 1:nrow(fc)){
    for (j in 1:ncol(fc)){
      fc[i,j]<- proportion1[i]*proportion2[j]*beta[i,j]
      total<-total+proportion1[i]*proportion2[j]*beta[i,j]
    }
  }
  fc<-fc/total
  
  f.c<-list()
  f.c[[1]]<-fc
  f.c[[2]]<-data.frame(proportion1,proportion2)
  f.c[[3]]<-data.frame(s1,s2)
  I<-0
  for (i in 1:nrow(f.c[[1]])){
    for (j in 1:ncol(f.c[[1]])){
      I <- I+(f.c[[3]]$s1[i]-f.c[[3]]$s2[j])^2*f.c[[1]][i,j]
    }
  }
  if(standard=='sd'){I<-I/2/sd(c(x1,x2))^2}
  I<-data.frame(categories = n,sample.size = network.size(g),contacts=sum(contact),Is=I)
  rm(fc)
  rm(f.c)
  I
}

