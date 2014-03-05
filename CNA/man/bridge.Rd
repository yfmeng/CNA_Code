\name{bridge}
\alias{bridge}
\alias{bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identification of Bridging Nodes in Networks
}
\description{
bridge detects bridging nodes between two or more population groups.
}
\usage{
bridge(g,attribute.name, gap...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a network
}
  \item{attribute.name}{
name of the concerned vertex attribute
}
  \item{gap}{
threshold of being a bridge
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The bridge function uses edge list data of contact networks.
}
\value{
The returned value is a table, listing the id, degree, maximum gap among neighbours of the bridging vertices.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fei MENG <fei.d.meng@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{summary.bridge}} provides summary of the output table from bridge function.
}
\examples{

}

\keyword{contact network}
\keyword{network bridging}% __ONLY ONE__ keyword per line
