assort.Farrington<-function(g,attribute.name,cate,contact,bi=c(F,'gender'),directed=TRUE,standard = 'sd',...){
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  
  if(missing(bi)){bi<-c(F,'')}
  if(missing(directed)){directed<-F}
  if(missing(standard)){standard<-'sd'}
  
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (!is.null(get.edge.attribute(g,contact))){
    weight <- get.edge.attribute(g,contact)
  }else{weight<-rep(1,nrow(edgelist))}
  if(!directed){
    # homogenous nodes (temporary solution)
    temp<-x1
    x1<-c(x1,x2)
    x2<-c(x2,temp)
    weight<-c(weight,weight)/2
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
  if(standard=='sd'){delta <-sd(c(x1,x2))^2}
  cate<-cut.cna(x1.temp,cate,add = F)$cate
  cate2<-cut.cna(x2.temp,cate,add = F)$cate
  if(cate2[1]<cate[1]){cate[1]<-cate2[1]}
  if(cate2[length(cate2)]>cate[length(cate)]){cate[length(cate)]<-cate2[length(cate2)]}
  x1<-cut.cna(x1,cate,add = F)$x
  x2<-cut.cna(x2,cate,add = F)$x
  x1.temp<-cut.cna(x1.temp,cate,add = F)$x
  x2.temp<-cut.cna(x2.temp,cate,add = F)$x
  l<-levels(x1)
  n<-length(cate)-1
  distr1<-rep(0,n)
  distr2<-rep(0,n)
  for (i in 1:n){
    distr1[i]<-sum(x1.temp==l[i])
    distr2[i]<-sum(x2.temp==l[i])
  }
  proportion1<-distr1/sum(distr1)
  proportion2<-distr2/sum(distr2)
  beta<-matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      weightij<-sum(weight[x1==l[i]&x2==l[j]])
      beta[i,j]<-weightij/(sum(x1.temp==l[i])*sum(x2.temp==l[j]))
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
 
  Is<-I/2/delta 
  I.output<-data.frame(categories = n,sample.size = network.size(g),weights=sum(weight),I=I,Is = Is)
  rm(fc)
  rm(f.c)
  I.output
}
