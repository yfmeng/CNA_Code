list2network<-function(edge.list,...){
  library(network)
  edge.list[,1]<-as.factor(edge.list[,1])
  edge.list[,2]<-as.factor(edge.list[,2])
  net<-as.network(edge.list,directed = F,matrix.type='edgelist')
  node.name<-network.vertex.names(net)
  if(exists(deparse(substitute(node.attr)))){
    node.attr.names<-names(node.attr)[2:ncol(node.attr)]
    node.order<-rep(0,length(node.name))
    for (i in 1:length(node.name)){
      node.order[i]<-which(node.attr[,1]==node.name[i])[1]
    }
    for (a.name in node.attr.names){
      set.vertex.attribute(net,a.name,node.attr[node.order,which(names(node.attr)==a.name)])
    }
  }
  edge.attr.names<-names(edge.list)[3:ncol(edge.list)]      
  for (e.name in edge.attr.names){
    set.edge.attribute(net,e.name,edge.list[,which(names(edge.list)==e.name)])
  }     
  invisible(net)
}