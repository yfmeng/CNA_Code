local.Farrington<-function(g,attribute.name,bi=c(F,'gender'),directed=TRUE,standard = 'sd',cate,...){
  require('network')
  require('sna')
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if (exists('contact')){
    if (!is.null(get.edge.attribute(g,contact))){
      contact <- get.edge.attribute(g,contact)
    }else{contact<-rep(1,nrow(edgelist))}
  }else{contact<-rep(1,nrow(edgelist))}
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
      contactij<-sum(contact[x1==l[i]&x2==l[j]])
      beta[i,j]<-contactij/(sum(x1.temp==l[i])*sum(x2.temp==l[j]))
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
  
  ids<- 1:network.size(g)
  local.I <- rep(0,length(idx))
  for (i in ids){
    range <- which(edgelist[,1]==i|edgelist[,2]==i)
    for (r in range){
      ii<-which(l==x1[r])
      jj<-which(l==x2[r])
      sq <- (s1[ii]-s2[jj])^2
      local.I[i] <-  local.Q[i]+sq*proportion1[ii]*proportion2[jj]*beta[ii,jj]
    }
  }
  local.I[is.na(local.I)]<-0
  local.I<-local.I/2/delta
  local.I
}
