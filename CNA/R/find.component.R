find.component<-function(g,root.nodes,waves,...){
  require('network')
  if(missing(waves)){waves=Inf;print('default:sampling until boudary reached')}
  last.size<-0
  included<-root.nodes
  component.size<-length(unique(root.nodes))
  count<-0
  while(last.size!=component.size&count<waves){
    last.size<-component.size
    neighbours<-c()
    for(i in included){
      neighbours<-c(neighbours,get.neighborhood(g,i))
    }
    neighbours<-unique(neighbours)
    included<-unique(c(included,neighbours))
    component.size<-length(included)
    count<-count+1
  }
  included
}