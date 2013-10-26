sd.kappa<-function(x1,x2,contact,data,cate,...){
  x1.name<-deparse(substitute(x1))
  x2.name<-deparse(substitute(x2))
  contact.name<-deparse(substitute(contact))
  data.name<-names(data)
  if(x1.name %in% data.name){x1<-data[,which(data.name==x1.name)]}
  if(x2.name %in% data.name){x2<-data[,which(data.name==x2.name)]}
  if(contact.name %in% data.name){contact<-data[,which(data.name==contact.name)]}
  n<-length(cate)
  a<-rep(0,n)
  b<-rep(0,n)
  for (i in 1:n){
    a[i]<-sum(contact[x2>=cate[i-1]&x2<cate[i]])
    b[i]<-sum(contact[x1>=cate[i-1]&x1<cate[i]])
  }
  a <- a/sum(contact)
  b <- b/sum(contact)
  sd.Newman<- (sum(a*b)+(sum(a*b))^2-sum(a^2*b)-sum(a*(b^2)))/(1-sum(a*b))
  sd.Newman<- sd.Newman/nrow(data)
  sd.Newman
}