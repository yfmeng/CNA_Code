sd.jackknife<-function(x1,x2,contact,data,cate,method,...){
    x1.name<-deparse(substitute(x1))
    x2.name<-deparse(substitute(x2))
    contact.name<-deparse(substitute(contact))
    data.name<-deparse(substitute(data))
    cate.name<-deparse(substitute(cate))
    xpress<-sprintf('%s(%s,%s,%s,%s,%s)',method,x1.name,x2.name,contact.name,data.name,cate.name)
    xpress<-parse(text = xpress)
    full <- eval(xpress)
    full <- full[,ncol(full)]
    full.i<-rep(0,nrow(data))
    for (i in 1:nrow(data)){
      xpress<-sprintf('%s(%s,%s,%s,%s[-%d,],%s)',method,x1.name,x2.name,contact.name,data.name,i,cate.name)
      xpress<-parse(text = xpress)
      temp<-eval(xpress)
      full.i[i]<-temp[,ncol(temp)]
    }
    sd.jackknife<-sum((full.i-full)^2)
  sd.jackknife
}