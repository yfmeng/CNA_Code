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
boot.assort(g, attribute.name, cate, contact, method='Farrington',method.args=list(bi=c(T,'gender'),standar='sd'), R, verbose, ...)

}

\arguments{
  \item{g}{a network}
  \item{attribute.name}{a string, name of the vertex attribute to be studied}
  \item{cate}{a vector, containing cut points of the attribute}
  \item{contact}{
a string. If contact is name of an edge attribute, the value of this attribute will be used as weight on edges, otherwise edges are not weighted. Use "contact = ''" for unweighted networks.
}
  \item{method}{
a string, indicating the measurement to be used. Options are \code{'Gupta'}, \code{'Newman'}, \code{'Farrington'}. If method is \code{'Farrington'}, \code{method.args} can be specified. 
}
  \item{method.args}{
  a list. 
  The first item is a vector, whose first member is a logical indicating whether the network is biparted and the second member is name of the vertext attribute that divides the vertices into two segments. 
  The second item is a string, indicating the standardisation method used by Farrington's measure. Only \code{'sd'} is supported for the current version.
  Default is \code{method.args=list(bi=(F,''),standard='sd')}
  }
  \item{R}{
number of bootstrap samples, Default value is 1000.
}
  \item{verbose}{
logical, if TRUE, additional information will be printed
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{assort.Gupta}}
 \code{\link{assort.Newman}}
 \code{\link{assort.Farrington}}
}
\examples{
require(network)
m <- matrix(rbinom(25,1,.1),200,200)
diag(m) <- 0
g <- network(m, directed=FALSE)
g <- set.vertex.attribute(g,'attr',runif(20))
r <- assort.Newman(g,'attr',cate=seq(0,1,by=0.1),contact = '')
boot<-assort.bootstrap(g,'attr',cate=seq(0,1,by=0.1),contact = '',method='Newman',R = 200,verbose = T)
hist(boot)
}
\keyword{ network assortativity }
\keyword{ bootstrap }