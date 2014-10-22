assort.bootstrap<-function(g,attribute.name,cate,contact,method,method.args,R,verbose,...){
  
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if(missing(contact)) contact <-''
  if(missing(method)) method<-'Newman'
  if (missing('verbose')){verbose<-F}
  if(missing(R)){
    R = 1000 
    if(verbose) cat('Default: 1000 samples',sep='\n')}
  
  if (!is.null(get.edge.attribute(g,contact))){
    weight <- get.edge.attribute(g,contact)
  }else{
    weight<-rep(1,nrow(edgelist))
    if (verbose) cat('Edges unweighted',sep='\n')
  }
  if (method=='Farrington'){
    if (missing(method.args)){
      bi1<-'F';bi2<-'';standard='sd'
    }else{
      bi1<-as.logical(method.args[[1]][1])
      bi2<-method.args[[1]][2]
      standard<-method.args[[2]]
    }
    opt.args <-sprintf(',bi=c(%s,\'%s\'),standard=\'%s\'',bi1,bi2,standard)
  }else{opt.args<-''}
  edge.id<-1:Ne
  results<-rep(0,R)
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'%s\'%s)',method,'',opt.args)
  
  if(verbose){
    cat(sprintf('Boostrapping calls:%s',xpress),sep='\n')
    cat(sprintf('Network name: %s',deparse(substitute(g))),sep='\n')
    cat(sprintf('Assortativity method: %s',method[1]),sep='\n')
    cat(sprintf('Number of samples: %d', R),sep='\n')
    cat(sprintf('Edge weight: %s',contact),sep='\n')
    t0<-Sys.time()
  }
  pb <- txtProgressBar(style = 3)
  xpress<-parse(text = xpress) 
  for(i in 1:R){
    sample.edges<-sample.int(Ne,size = Ne, replace = T,prob = weight)
    sample<-g
    sample<-delete.edges(sample,valid.eids(sample))
    sample<-add.edges(sample,edgelist[sample.edges,1],edgelist[sample.edges,2])
    temp<-eval(xpress)    
    results[i]<-as.numeric(temp[length(temp)])
    setTxtProgressBar(pb, i/R)
  }
  close(pb)
  if (verbose){
    t<-Sys.time()-t0
    cat(sprintf('Resampled %d times in %f minutes.',R,as.numeric(t,unites='mins')),sep='\n')
    print(summary(results))
  }
  results
}
