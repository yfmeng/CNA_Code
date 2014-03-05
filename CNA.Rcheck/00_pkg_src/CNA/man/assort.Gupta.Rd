\name{assort.Gupta}
\alias{assort.Gupta}
\alias{assort.Gupta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gupta's Q Index for Measuring Network Assortativeness
}
\description{
assort.Gupta is used to calculate the network assortativeness using Gupta's Q method. 
}
\usage{
assort.Gupta(g,attribute.name, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
a network
}
  \item{attribute.name}{
the name of the vertex attribute to be measured
}
  \item{contact}{
contact rate between two individuals; default value is 1 for every edge
}
  \item{cate}{
a vector, specifying the cutpoints of subgroup division
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The assort.Newman function uses edge list data of contact networks.
}
\value{
The returned value is a data frame.
}
\references{
Gupta S., Anderson R.M., and May, R.M. (1989) Networks of sexual contacts: implications for the patterns of spread of HIV. AIDS. 3. 807-817.
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
Other measures of assortativeness in \link{CNA} package are \code{\link{assort.Newman}}, \code{\link{assort.Farrington}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{network assortativeness}
\keyword{contact network}
