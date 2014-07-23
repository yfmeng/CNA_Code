jackknife.assort<-function(g,attribute.name,cate,contact,method,verbose,...){
  Ne<-network.edgecount(g)
  if (method=='Farrington'){
    #bi=c(F,'gender'),directed=TRUE,standard = 'sd'
    opt.args <-sprintf(',bi=c(%d,\'%s\',directed = %d,standard=\'%s\'',as.integer(bi[1]),bi[2],as.integer(directed),standard)
  }else{opt.args<-''}
  
  xpress<-sprintf('assort.%s(sample,attribute.name,cate,\'weight\',%s)',method,opt.args)
  if (missing('verbose')){verbose<-F}
  if(verbose){
    print(sprintf('Jackknife calls:%s',xpress))
    print(sprintf('Network name: %s',deparse(substitute(g))))
    print(sprintf('Assortativity method: %s',method))
    print(sprintf('Number of categories: %d', length(cate)-1))
    print(sprintf('Edge weight: %s',contact))
    t0<-Sys.time()
  }
  
  jackknives<-c()
    for (i in 1:Ne){
      sample<-g
      sample<-delete.edges(sample,i)
      xpress<-parse(text = xpress)
      temp<-eval(xpress)    
      jackknives[i]<-as.numeric(temp[length(temp)])
    }
  print(sprintf('Resampled %d times in %f minutes.',Ne,as.numeric(t,unites='mins')))
  jackknives
}