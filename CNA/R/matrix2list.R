matrix2list<-function(m,directed=TRUE,head=TRUE,...){
  m[is.na(m)]<-0
  m[m=='']<-0
  if(head){
    if(nrow(m)<2|ncol(m)<2){
      print('inadequate rows/columns in the matrix')
      stop
    } else {
      x.id<-as.vector(m[2:nrow(m),1])
      y.id<-as.vector(m[1,2:ncol(m)])
      m<-m[2:nrow(m),2:ncol(m)]
    }
  }else{
    # head = F
    x.id<-rownames(m)
    y.id<-colnames(m)
    if(is.null(x.id)){x.id<-1:nrow(m)}
    if(is.null(y.id)){
      y.id<-1:ncol(m)
      if(directed){
        y.id<-y.id+nrow(m)
      }
    }

  }
  
  if(!directed){
    m[lower.tri(m)]<-0
  }
  
  l<-sum(m!=0)
  x1.id<-rep(0,l)
  x2.id<-rep(0,l)
  contact<-rep(0,l)
  index<-which(m!=0)
  for (i in 1:l){
    temp<-index[i]%%nrow(m)
    temp[temp==0]<-nrow(m)
    x1.id[i]<-x.id[temp]
    x2.id[i]<-y.id[ceiling(index[i]/nrow(m))]
    contact[i]<-m[index[i]]
  }
  edge.list<-data.frame(x1.id,x2.id,contact)
  edge.list
}