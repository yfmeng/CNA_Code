\name{assort.Farrington}
\alias{assort.Farrington}
\alias{assort.Farrington}
\alias{assort.FarringtonC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculating Farrington's I and Is disassortativity index
}
\description{
assort.Farrington is used to calculate network disassortativity with Farrington's I and Is measurement, using categorical network attributes. 
assort.FarringtonC applies to continuous data.
}
\usage{
assort.Farrington(g, attribute.name, cate, contact, bi = c(F, "gender"), directed = TRUE, standard = "sd", ...)
assort.FarringtonC(g, attribute.name, contact, bi = c(F, "gender"), directed = TRUE, standard = "sd", ...)
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
  \item{bi}{
a vector, optional argument. The first element indicates if the network is bipartite. If bipartite, the second element is name of the vertex attribute that identify the two parts. Default value is FALSE.
}
  \item{directed}{
logical, optional argument. If true the network is taken as directed, regardless to the built-in network attribute 'directed'. Default value is TRUE.
}
  \item{standard}{
a string. Optional argument that indicates standardisation method. Default value is 'sd'.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a vector of two elements.
 \item{I}{naive Farrington's I index}
 \item{Is}{standardised Farrington's I index}
%% ...
}
\references{
Farrington CP, Whitaker HJ, Wallinga J, Manfredi P (2009) Measures of disassortativeness and their application to directly transmitted infections. Biom J 51: 387-407.
}
\author{
Fei MENG\email{fei.d.meng@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(network)
m <- matrix(rbinom(25,1,.2),20,20)
diag(m) <- 0
g <- network(m, directed=FALSE)
g <- set.vertex.attribute(g,'attr',runif(20))
g <- set.edge.attribute(g,'weight',rweibull(network.edgecount(g),1.5,1))
cut<-seq(0,1,by = 0.1)
assort.Farrington(g,'attr',cate=cut,contact='weight')
assort.Farrington(g,'attr',contact='weight')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ network assortativity }
\keyword{ network disassortativity }
