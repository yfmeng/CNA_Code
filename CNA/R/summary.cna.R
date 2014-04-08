summary.cna <-function(g,attribute.name,...){
  # bipart = attribute name
  edgelist <- as.edgelist.sna(g)
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  x1 <- x1[1:(length(x1)/2)]
  x2 <- x2[1:(length(x2)/2)]
  
  if (exists('bipart')){
    g1 <- get.vertex.attribute(g,bipart)[edgelist[,1]]
    g2 <- get.vertex.attribute(g,bipart)[edgelist[,2]]
    g1 <- g1[1:(length(g1)/2)]
    bi <- unique(c(g1,g2))
    xtemp<-x1
    xtemp[g1==bi[2]]<-x2[g1==bi[2]]
    x2[g1==bi[2]]<-x1[g1==bi[2]]
    x1<-xtemp
  }
  
  summary.cna<-list()
  summary.cna[[1]]<-data.frame(x1,x2)
  summary.cna[[2]]<-data.frame(mean = mean(x1-x2),median=median(x1-x2),sd = sd(x1-x2),min = min(x1-x2),max = max(x1-x2))
  summary.cna
}