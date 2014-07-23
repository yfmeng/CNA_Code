assort.bootstrap<-function(g,attribute.name,cate,contact,method,R,verbose,...){
  
  Ne<-network.edgecount(g)
  edgelist <- as.edgelist.sna(g)[1:Ne,]
  x1 <- get.vertex.attribute(g,attribute.name)[edgelist[,1]]
  x2 <- get.vertex.attribute(g,attribute.name)[edgelist[,2]]
  if(missing(contact)) contact <-''
  if(missing(method)) method<-'Newman'
  if (missing('verbose')){verbose<-F}
  if(missing(R)){
    R = 1000 
    if(verbose) cat('Default: 1000 samples')}
  
  if (!is.null(get.edge.attribute(g,contact))){
    weight <- get.edge.attribute(g,contact)
  }else{
    weight<-rep(1,nrow(edgelist))
    if (verbose) cat('Edges unweighted')
  }
  if (method[1]=='Farrington'){
    #bi=c(F,'gender'),directed=TRUE,standard = 'sd'
    bi1<-'F';bi2<-'';di<-'F'
    if(length(method)>1){      
      if(as.logical(method[2])) {bi1<-'T';bi2<-method[3]}
      di<-'F'
      if(as.logical(method[3])) {di<-'T'}
    }
    opt.args <-sprintf(',bi=c(%s,\'%s\'),directed = %s,standard=\'%s\'',bi1,bi2,di,standard)
  }else{opt.args<-''}
  edge.id<-1:Ne
  results<-rep(0,R)
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'%s\'%s)',method,'',opt.args)
  
  if(verbose){
    cat(sprintf('Boostrapping calls:%s',xpress))
    cat(sprintf('Network name: %s',deparse(substitute(g))))
    cat(sprintf('Assortativity method: %s',method[1]))
    cat(sprintf('Number of samples: %d', R))
    cat(sprintf('Edge weight: %s',contact))
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
    cat(sprintf('Resampled %d times in %f minutes.',R,as.numeric(t,unites='mins')))
    print(summary(results))
  }
  results
}
