sd.jackknife<-function(x1,x2,contact,data,cate,method,...){
    x1<-deparse(substitute(x1))
    x2<-deparse(substitute(x2))
    contact<-deparse(substitute(contact))
    data<-deparse(substitute(data))
    cate<-deparse(substitute(cate))
    method<-methoddeparse(substitute(cate))
    xpress<-sprintf('%s(%s,%s,%s,%s,%s)',method,x1,x2,contact,data,cate)
    xpress<-parse(text = xpress)
    full <- eval(xpress)
    full <- full[,end]
    full.i<-rep(0,nrow(data))
    for (i in 1:nrow(data)){
      press<-sprintf('%s(%s,%s,%s,%s[-%d,],%s)',method,x1,x2,contact,i,data,cate)
      xpress<-parse(text = xpress)
      temp<-eval(xpress)
      full.i[i]<-temp[,end]
    }
    sd.jackknife<-sum((full.i-full)^2)
  sd.jackknife
}