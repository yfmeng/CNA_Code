\name{assort.summary}
\alias{assort.summary}
\alias{assort.summary}
\title{
Summary statistics of the gap between linked nodes
}
\description{
This function provide summary statistics of the gap of vertex attribute between linked nodes.
}
\usage{
assort.summary(g,attribute.name,cate,contact,bipart)
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
a vector, cut points of the gap
}
  \item{contact}{
a string. If contact is name of an edge attribute, the value of this attribute will be used as weight on edges, otherwise edges are not weighted. Use "contact = ''" for unweighted networks.
}
  \item{bipart}{
  a string. If bipart is name of a vertex attribute, the network is assumed to be bipart. If the vertex attribute has more than 3 possible values, only the first 2 are used.
  }
  \item{\dots}{}
}


\value{
the output value is a list
\item{summary}{quantiles, extreme values, mean and median of the gap between partners}
\item{sd }{standard deviation of the gap between partners}
\item{gap.category }{frequency of the gap between partners given a categorisation method}
}
\references{
}
\author{
Fei MENG \email{fei.d.meng@gmail.com}
}
\examples{
require(network)
m <- matrix(rbinom(25,1,.2),20,20)
diag(m) <- 0
g <- network(m, directed=FALSE)
g <- set.vertex.attribute(g,'attr',runif(20))
g <- set.vertex.attribute(g,'type',c(rep(0,10),rep(1,10)))
g <- set.edge.attribute(g,'weight',rweibull(network.edgecount(g),1.5,1))
cut<-seq(0,5,by = 0.5)
assort.summary(g,attribute.name='attr',cate=cut,bipart='type')
}
\keyword{ network assortativity }
