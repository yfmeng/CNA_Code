\name{assort.Newman}
\alias{assort.Newman}
\alias{assort.Newman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating Newman's Q assortativity coefficient
}
\description{
assort.Newman is used to calculate network assortativity with Newman's Q measurement.
}
\usage{
assort.Newman(g, attribute.name, cate, contact, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
the network to be measured
}
  \item{attribute.name}{
a string, name of the vertex attribute
}
  \item{cate}{
a vector, cut points of the attribute
}
  \item{contact}{
a string. If contact is name of an edge attribute, the value of this attribute will be used as weight on edges, otherwise edges are not weighted. Use "contact = ''" for unweighted networks.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Newman ME (2003) Mixing patterns in networks. Phys Rev E Stat Nonlin Soft Matter Phys 67: 026126.
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
m <- matrix(rbinom(25,1,.2),20,20)
diag(m) <- 0
g <- network(m, directed=FALSE)
g <- set.vertex.attribute(g,'attr',runif(20))
g <- set.edge.attribute(g,'weight',rweibull(network.edgecount(g),1.5,1))
cut<-seq(0,1,by = 0.1)
assort.Newman(g,'attr',cate=cut,contact='weight')
assort.Newman(g,'attr',cate=cut,contact='')
}
\keyword{ network assortativity }
\keyword{ Newman coefficient }