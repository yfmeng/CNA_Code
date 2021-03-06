list2matrix<-function(id.names,contact,data){
  location<-which(names(data)%in%id.names)
  data[,location[1]]<-as.vector(data[,location[1]])
  data[,location[2]]<-as.vector(data[,location[2]])
  names<-unique(c(data[,location[1]],data[,location[2]]))
  names<-sort(names)
  index<-1:length(names)
  size<-length(names)
  mat<-matrix(0,size,size)

  for (i in 1:nrow(data)){
    data[i,location[1]]<-index[which(names==data[i,location[1]])]
    data[i,location[2]]<-index[which(names==data[i,location[2]])]
  } 
  edges<-as.matrix(data[,location])
  edges<-matrix(as.numeric(edges),ncol = 2)
  contact.name<-deparse(substitute(contact))
  contact<-data[,which(names(data)==contact.name)]
  mat[edges]<-contact
  colnames(mat)<-names
  rownames(mat)<-names
  invisible(mat)
}