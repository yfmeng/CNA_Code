assort.summary<-function(g,attribute.name,cate,contact,bipart){
  if(missing(contact)){contact<-''}
  if(missing(bipart)){
    bipart=F
  }else{
    bi.name=bipart;bipart=T
  }
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  output<-list()
  gap<-x1-x2
  if(bipart){
    bi<-get.vertex.attribute(g,bi.name)
    bi.tag<-unique(bi)
    bi<-bi[edgelist[,1]]
    gap[bi==bi.tag[2]]<--gap[bi==bi.tag[2]]
  }else{
    gap<-abs(gap)
  }
  output[['summary']]<-summary(gap)
  output[['sd']]<-sd(gap)
  gap<-cut.cna(gap,cate)$x
  output[['gap.category']]<-table(gap)
  output
}