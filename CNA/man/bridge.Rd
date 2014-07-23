\name{bridge}
\alias{bridge}
\alias{bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtaining bridging vertices in a network
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bridge(g, attribute.name, gap, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a network
}
  \item{attribute.name}{
a string, name of the vertex attribute that groups the population
}
  \item{gap}{
numeric, if the maximum difference among a vertex's neigbhours is larger than value of 'gap', this vertex is a brigher
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{bridge} returns a data frame. The first column contains the IDs of brighers in network \code{g}; the second column contains degree of each bridger; the third contains width of the bridge, i.e. max gap between one's neighbours.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fei MENG \email{fei.d.meng@gmail.com}
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
bridgers<-bridge(g,'attr',gap = 0.2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ network bridging }

