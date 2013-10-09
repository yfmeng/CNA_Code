fc<-function(x1,x2,contact,cate,data,directed=TRUE,...){
  # generate & plot the conditional contact distribution fc matrix
  x1.name<-deparse(substitute(x1))
  x2.name<-deparse(substitute(x2))
  contact.name<-deparse(substitute(contact))
  data.name<-names(data)
  if(x1.name %in% data.name){x1<-data[,which(data.name==x1.name)]}
  if(x2.name %in% data.name){x2<-data[,which(data.name==x2.name)]}
  if(contact.name %in% data.name){contact<-data[,which(data.name==contact.name)]}
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
  s1<-c()
  s2<-c()
  for (i in 1:(length(cate)-1)){
    s1[i]<-mean(x1[x1>cate[i]&x1<=cate[i+1]])
    s2[i]<-mean(x2[x2>cate[i]&x2<=cate[i+1]])
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
  names(f.c)<-c('contact distribution matrix','population distribution','mean within bins')
  f.c
}
