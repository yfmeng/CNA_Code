assort.jackknife<-function(g,attribute.name,cate,contact,method,method.args,verbose,...){
  Ne<-network.edgecount(g)
  if(missing(contact)) contact <-''
  if(missing(method)) method<-'Newman'
  if (missing('verbose')){verbose<-F}
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
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'%s\'%s)',method,'',opt.args)
  
  if(verbose){
    cat(sprintf('Jackknife calls:%s',xpress))
    cat(sprintf('Network name: %s',deparse(substitute(g))))
    cat(sprintf('Assortativity method: %s',method))
    cat(sprintf('Edge weight: %s',contact))
    t0<-Sys.time()
  }
  pb <- txtProgressBar(style = 3)
  jackknives<-c()
    for (i in 1:Ne){
      sample<-g
      sample<-delete.edges(sample,i)
      xpress<-parse(text = xpress)
      temp<-eval(xpress)    
      jackknives[i]<-as.numeric(temp[length(temp)])
      setTxtProgressBar(pb, i/Ne)
    }
  close(pb)
  if(verbose){
  cat(sprintf('Resampled %d times in %f minutes.',Ne,as.numeric(t,unites='mins')))
  print(summary(jackkives))}
  jackknives
}