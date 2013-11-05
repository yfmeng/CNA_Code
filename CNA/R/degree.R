degree<-function(id.names,data,directed = T){
  location<-which(id.names%in%names(data))
  relation.id<-paste(data[,location[1]],data[,location[2]])
  data<-data[!duplicated(relation.id),]
  degree<-list()
  if (directed){
    degree[[1]]<-table(table(data[,location[1]]))
    degree[[2]]<-table(table(data[,location[2]]))
  } else {
    idx1<-c(data[,location[1]],data[,location[2]])
    idx2<-c(data[,location[2]],data[,location[1]])
    degree[[1]]<-table(table(idx1))
    degree[[2]]<-table(table(idx2))
    degree[[1]]<-degree[[1]]/2
    degree[[2]]<-degree[[2]]/2
  }
  invisible(degree)
}