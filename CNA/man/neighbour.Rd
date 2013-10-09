\name{neighbour}
\alias{neighbour}
\alias{neighbour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Neighbourhood of a Set of Nodes
}
\description{
\code{neighbourhood} looks for the nodes (neighbours) that are linked to a given set of nodes (origins).
}
\usage{
neighbour(x1.id,x2.id, root.nodes, data, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1.id}{
name of the original nodes
}
  \item{x2.id}{
name of the neighbouring nodes
}
  \item{root.node}{
a vector, specifying the origin nodes
}
  \item{data}{
a data.frame, containing the edge list of a network
}
  \item{size}{
the maximum size of the returned neighbourhood including the origins. If the value of size is smaller than the neighbourhood, a subset of neighbours will be randomly selected.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a data frame, containing the neighbourhood
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
\code{\link{sampling}},\code{\link{sampling.method}}
}
\examples{
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contact network }
\keyword{ neighbourhood }% __ONLY ONE__ keyword per line
