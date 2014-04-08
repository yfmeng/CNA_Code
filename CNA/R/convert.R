convert<-function(edgelist,ids,attributes){
  idx<-1:max(max(edgelist[,ids[1]]),max(edgelist[,ids[2]]))
  ages<-c()
  for(i in idx){
    j1<-which(edgelist[,ids[1]]==i)
    if (length(j1)>0){
      #male
      ages[i]<-edgelist[j1,attributes[1]][1]
    }
    
    j2<-which(edgelist[,ids[2]]==i)
    if (length(j2)>0){
      #male
      ages[i]<-edgelist[j2,attributes[2]][1]
    }
    
    if(length(j1)==0&length(j2)==0){
      ages[i]<-NA
    }
  }
  ages
}