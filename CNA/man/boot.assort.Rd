\name{boot.assort}
\alias{boot.assort}
\alias{boot.assort}
\title{
Assortativity measures with bootstrap resampling
}
\description{

}
\usage{
boot.assort(g, attribute.name, cate, contact, method='Gupta', R, verbose, ...)
boot.assort(g, attribute.name, cate, contact, method='Newman', R, verbose, ...)
boot.assort(g, attribute.name, cate, contact, method=c('Farrington',bi=c(F,''),directed=T), R, verbose, ...)
}
\arguments{
  \item{g}{
a network
}
  \item{attribute.name}{
a string, name of the vertex attribute to be studied
}
  \item{cate}{
a vector, containing cut points of the attribute
}
  \item{contact}{
a string. If contact is name of an edge attribute, the value of this attribute will be used as weight on edges, otherwise edges are not weighted. Use "contact = ''" for unweighted networks.
}
  \item{method}{
a string or vector, indicating the measurement to be used. 

}
  \item{R}{
number of bootstrap samples
}
  \item{verbose}{
logical, if TRUE, additional information will be printed
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a vector of assortativity measures.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fei MENG\email{fei.d.meng@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{help}}
}
\examples{
require(network)
m <- matrix(rbinom(25,1,.1),200,200)
diag(m) <- 0
g <- network(m, directed=FALSE)
g <- set.vertex.attribute(g,'attr',runif(20))
r <- assort.Newman(g,'attr',cate=seq(0,1,by=0.1),contact = '')
boot<-boot.assort(g,'attr',cate=seq(0,1,by=0.1),contact = '',method='Newman',R = 500,verbose = T)
hist(boot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ network assortativity }
\keyword{ bootstrap }% __ONLY ONE__ keyword per line
