\name{assort.Gupta}
\alias{assort.Gupta}
\alias{assort.Gupta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating Gupta's Q assortativity coefficient
}
\description{
assort.Gupta is used to calculate network assortativity with Gupta's Q measurement.
}
\usage{
assort.Gupta(g, attribute.name, cate, contact, ...)
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
Gupta S, Anderson RM, May RM (1989) Networks of sexual contacts: implications for the pattern of spread of HIV. AIDS 3: 807-817.
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
assort.Gupta(g,'attr',cate=cut,contact='weight')
assort.Gupta(g,'attr',cate=cut,contact='')
}
\keyword{ network assortativity }
\keyword{ Q coefficient }