sampling.method<-function(method){
  name <- method[1]
  r<-data.frame(name='')
  methods=c('induced','incident','star','snowball','tracing')
  if (!name%in%methods){
    r$valid<-F
    warning('Unknown sampling method.')
  } else {
    
    r$valid<-T
    if (name=='induced'){
      r$name<-'induced'
      r$p.node<-as.numeric(method[2])
    }
    if (name=='incident'){
      r$name<-'incident'
      r$p.edge<-as.numeric(method[2])
    }
    if (name=='snowball'){
      r$name<-'snowball'
      if (length(method)>=2){r$n.start<-as.numeric(method[2])} else {r$n.start<-1}
      if (length(method)>=3){r$increase<-as.numeric(method[3])} else {r$increase<-1}
      if (length(method)>=4){r$resample<-as.logical(method[4])} else {r$resample<-T}
      if (length(method)>=5){r$waves<-as.integer(method[5])} else {r$waves<-1000}
    }
    if (name=='star'){
      r$name<-'star'
      r$n.start<-as.numeric(method[2])
    }
    
    if (name=='tracing'){
      r$name <- 'tracing'
      r$n.start<-as.numeric(method[2])
      r$threshold<-as.numeric(method[3])
      r$recruit.rate<-as.numeric(method[4])
    }
  
  }
  invisible(r)
}