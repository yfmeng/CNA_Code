\name{find.component}
\alias{find.component}
\alias{find.component}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtaining the component(s) that contains a give list of vertices
}
\description{
\code{find.component} returns the IDs of vertices that belong to the components that contains the given vertices. This function uses a snowball strategy and the number of link-tracing waves can be defined.
}
\usage{
find.component(g,root.nodes,waves,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a network
}
  \item{root.nodes}{
a vector of vertex ID
}
  \item{waves}{
optional argument indicating maximum number of waves, infinite if missing.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector containing the vertex IDs in the component(s).
}
\references{
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
components<-find.component(g,c(1,7))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ network component }
