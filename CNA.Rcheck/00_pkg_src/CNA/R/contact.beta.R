contact.beta<-function(x1,x2,contact,cate,data,directed=TRUE,...){
  # generate & plot the contact rate matrix
  x1.name<-deparse(substitute(x1))
  x2.name<-deparse(substitute(x2))
  contact.name<-deparse(substitute(contact))
  data.name<-names(data)
  if(x1.name %in% data.name){x1<-data[,which(data.name==x1.name)]}
  if(x2.name %in% data.name){x2<-data[,which(data.name==x2.name)]}
  if(contact.name %in% data.name){contact<-data[,which(data.name==contact.name)]}
  # default size of bins = 1
  if(!exists('cate')){cate<-floor(min(c(x1,x2))):ceiling(max(c(x1,x2)))}
  n<-length(cate)-1
  beta<-matrix(0,n,n)
  
  for (i in 1:n){
    for (j in 1:n){
      contactij<-sum(contact[x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j]])
      beta[i,j]<-contactij/sum(x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j])
    }
  }
  beta[is.na(beta)]<-0
  beta
}
