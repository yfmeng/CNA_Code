summary.bridge<-function(node.bridge){
  print('summary of bridging behaviour')
  summary.gap<-summary(node.bridge$node.gap)
  no.of.nodes<-nrow(node.bridge)
  ave.degree<-round(mean(node.bridge$node.degree),2)
  max.gap<-max(node.bridge$node.gap)
  no.of.bridge<-sum(node.bridge$node.bridge)
  percentage<-round(no.of.bridge/no.of.nodes*100,2)
  sum.bridge<-data.frame(no.of.nodes,ave.degree,max.gap,no.of.bridge,percentage)
  sum.bridge
}