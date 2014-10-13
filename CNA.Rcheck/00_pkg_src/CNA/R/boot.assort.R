boot.assort<-function(g,attribute.name,cate,contact,method,R,verbose,...){
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]

  if (!is.null(get.edge.attribute(g,contact))){
    weight <- get.edge.attribute(g,contact)
  }else{weight<-rep(1,nrow(edgelist))}
  if (method[1]=='Farrington'){
    #bi=c(F,'gender'),directed=TRUE,standard = 'sd'
    bi1<-'F'
    if(as.logical(method[2])) {bi1<-'T'}
    di<-'F'
    if(as.logical(method[4])) {di<-'T'}
    standard<-'sd'
    if(length(method)>4){standard<-method[5]}
    opt.args <-sprintf(',bi=c(%s,\'%s\'),directed = %s,standard=\'%s\'',bi1,method[3],di,standard)
  }else{opt.args<-''}
  
  if (missing('verbose')){verbose<-F}
  if(missing(R)){R = 1000}
  edge.id<-1:Ne
  results<-rep(0,R)
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'%s\'%s)',method[1],'',opt.args)
  
  if(verbose){
    print(sprintf('Boosstrapping calls:%s',xpress))
    print(sprintf('Network name: %s',deparse(substitute(g))))
    print(sprintf('Assortativity method: %s',method))
    print(sprintf('Number of samples: %d', R))
    print(sprintf('Number of categories: %d', length(cate)-1))
    print(sprintf('Edge weight: %s',contact))
    t0<-Sys.time()
  }
  
  xpress<-parse(text = xpress) 
  for(i in 1:R){
    sample.edges<-sample.int(Ne,size = Ne, replace = T,prob = weight)
    sample<-g
    sample<-delete.edges(sample,valid.eids(sample))
    sample<-add.edges(sample,edgelist[sample.edges,1],edgelist[sample.edges,2])
    temp<-eval(xpress)    
    results[i]<-as.numeric(temp[length(temp)])
  }
  if (verbose){
    t<-Sys.time()-t0
    print(sprintf('Resampled %d times in %f minutes.',R,as.numeric(t,unites='mins')))
  }
  results
}
