\name{cut.cna}
\alias{cut.cna}
\alias{cut.cna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Factorise numeric vectors
}
\description{
\code{cut.cna} is a extension of {\code{\link{base::cut}}}.
Values that exceed the range of given cut points can be handled to avoid coerced \code{NA}. 
}
\usage{
cut.cna(x, cate, add = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric
}
  \item{cate}{
a vector, containing cutpoints
}
  \item{add}{
logical, if \code{TRUE}, extra intervals are added to the given categories when \code{x} exceed the range of \code{cate}, otherwise the lowest/highest cutpoint is replaced by the minimal/maximal value in \code{x}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{cut.cna} returns a list with two elements. 
\item{x}{a vector of factor, categorised corresponding to input \code{x}}
\item{cate}{updated \code{cate}}
}
\references{
}
\author{
Fei MENG\email{fei.d.meng@gmail.com}
}

\seealso{
 \code{\link{cut}}
}
\examples{
 x<- 1:25
 cate<- c(5,10,15,20)
 x.1<-cut.cna(x,cate,add=F)$x
 cate.1<-cut.cna(x,cate,add=F)$cate # cate.1=c(1,10,15,25)
 x.2<-cut.cna(x,cate,add=T)$x
 cate.2<-cut.cna(x,cate,add=T)$cate # cate.2=c(1,5,10,15,20,25)
}