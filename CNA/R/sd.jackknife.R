sd.jackknife<-function(x1,x2,contact,data,cate,method,...){
    x1<-deparse(bquote(x1,where = parent.frame(data)))
    x2<-deparse(bquote(x2,where = parent.frame(data)))
    contact<-deparse(bquote(contact,where = parent.frame(data)))
    data<-deparse(bquote(data))
    cate<-deparse(bquote(cate))
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