assort.jackknife<-function(g,attribute.name,cate,contact,method,verbose,...){
  Ne<-network.edgecount(g)
  if(missing(contact)) contact <-''
  if(missing(method)) method<-'Newman'
  if (missing('verbose')){verbose<-F}
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
  
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'%s\',%s)',method,contact,opt.args)
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