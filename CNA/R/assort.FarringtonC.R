assort.FarringtonC<-function(x1,x2,contact,data,directed=TRUE,...){
  # continuous Farrington
  x1.name<-deparse(substitute(x1))
  x2.name<-deparse(substitute(x2))
  contact.name<-deparse(substitute(contact))
  data.name<-names(data)
  if(x1.name %in% data.name){x1<-data[,which(data.name==x1.name)]}
  if(x2.name %in% data.name){x2<-data[,which(data.name==x2.name)]}
  if(contact.name %in% data.name){contact<-data[,which(data.name==contact.name)]}
   
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
  
  if(!exists('family')){family<-'empirical'}
  
  if (family=='empirical'){
    # use empirical distribution
    s1<-sort(unique(x1))
    s2<-sort(unique(x2))
  } 
  # n = number of categories 
  # distr = cumulative distribution of x1&x2
  distr1<-rep(0,length(s1))
  distr2<-rep(0,length(s2))
  for (i in 1:length(s1)){
    distr1[i]<-sum(x1==s1[i])
  }
  for (i in 1:length(s2)){
    distr2[i]<-sum(x2==s2[i])
  }
  
  proportion1<-distr1/sum(distr1)
  proportion2<-distr2/sum(distr2)
  beta<-matrix(0,length(s1),length(s2))
  
  for (i in 1:length(s1)){
    for (j in 1:length(s2)){
      contactij<-sum(contact[x1==s1[i]&x2==s2[j]])
      beta[i,j]<-contactij/sum(x1==s1[i]&x2==s2[j])
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
  I<-0
  for (i in 1:nrow(f.c[[1]])){
    for (j in 1:ncol(f.c[[1]])){
      I <- I+(s1[i]-s2[j])^2*f.c[[1]][i,j]
    }
  }
  I<-I/2/sd(c(x1,x2))^2
  I<-data.frame(vertex.value.size=length(unique(c(s1,s2))),edge.sample.size = nrow(data),contacts=sum(contact),Is=I)
  rm(fc)
  rm(f.c)
  I
  
}