cut.cna<-function(x,cate,add = F,...){
  
  if (add){# add new groups
    if (min(x)<cate[1]){cate<-c(min(x),cate)}
    if (max(x)>cate[length(cate)]){cate<-c(cate,max(x))}
  }else{
    if (min(x)<cate[1]){cate[1]<-min(x)}
    if (max(x)>cate[length(cate)]){cate[length(cate)]<-max(x)}
  }
  x<-cut(x,breaks = cate,include.lowest = T)
  output<-list()
  output$cate<-cate
  output$x<-x
  output
}