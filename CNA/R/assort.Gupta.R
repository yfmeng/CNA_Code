assort.Gupta<-function(x1,x2,contact,data,cate,...){
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
  contact.matrix<-matrix(0,n,n)
  on.diag<-0
  all<-0
  for (i in 1:n){
    for (j in 1:n){
      contactij<-sum(contact[x1<=cate[i+1]&x1>cate[i]&x2<=cate[j+1]&x2>cate[j]])
      if (i==j){on.diag<-on.diag+contactij}
      all<-all+contactij
    }
  }
  Q<-on.diag/all
  Q<-data.frame(categories = n,sample.size = length(x1),contacts=sum(contact),Q)
  Q
}
