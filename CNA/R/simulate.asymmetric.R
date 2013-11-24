simulate.asymmetric<-function(size,distribution,preferred,sd,sym,...){
  # norm, uniform, weibull,poission
  # 'norm', mean, sd
  # 'unif', min, max
  # 'weibull' shape, scale
  # 'pois', lambda
  distr.name <-distribution[1]
  base <- rep(0,size)
  if (distr.name =='norm'){base <- rnorm(size,as.numeric(distribution[2]),as.numeric(distribution[3]))}
  if (distr.name =='unif'){base <- runif(size,as.numeric(distribution[2]),as.numeric(distribution[3]))}
  if (distr.name =='weibull'){base <- rweibull(size,as.numeric(distribution[2]),as.numeric(distribution[3]))}
  if (distr.name =='pois'){base <- rpois(size,as.numeric(distribution[2]))}
  
  add<-rnorm(length(base), mean = preferred, sd)
  if (exists('shape')){
    if(shape>0)(add<-add*shape*base)
    if(shape<0)(add<-add*abs(shape)*(max(base)-base))
  }  
  if(!sym){
    part<-base+add
  }else{
    part<-c(base+add,base-add)
    base<-rep(base,2)
  }
  data<-data.frame(base=base,part=part)
  invisible(data)
}