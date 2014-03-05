\name{assort.Newman}
\alias{assort.Newman}
\alias{assort.Newman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Newman's Index for Measuring Network Assortativeness
}
\description{
assort.Newman is used to calculate the network assortativeness using Newman's method. 
}
\usage{
assort.Newman(g,attribute.name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a network 
}
  \item{attribute.name}{
name of the vertex attribute to be measured
}
  \item{cate}{
a vector, specifying the cutpoints of subgroup division
}
  \item{directed}{
logical, indicating whether the network is directed
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a data frame.
}
\references{
Newman, M.E.J. (2003) Mixing Patterns in Networks. Physical Review E 67. 026126.
}
\author{
%%  ~~who you are~~
Fei MENG <fei.d.meng@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
Other measures of assortativeness in \link{CNA} package are \code{\link[CNA]{assort.Gupta}}, \code{\link[CNA]{assort.Farrington}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ assortativeness }
\keyword{ contact network}
