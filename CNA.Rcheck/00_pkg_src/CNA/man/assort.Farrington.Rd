\name{assort.Farrington}
\alias{assort.Farrington}
\alias{assort.Farrington}
\title{
Farrington's I Index for Measuring Network Assortativeness
}
\description{
assort.Farrington is used to calculate the network assortativeness using Farrington's I square method. 
}
\usage{
assort.Newman(g,attribute.name,directed = T,bi=c(T,bi.name) ...)
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
  \item{bi}{
a vector, the first element is logical, indicating whether the network is bipartite; the second element is the name of the vertex attribute that labels the bipartitions
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The assort.Farrington function uses edge list data of contact networks.
}
\value{
The returned value is a data frame.
}
\references{
Farrington C.P, Whitaker H.J., Wallinga J and Manfredi P. (2009) Measures of Disassortativeness and their Application to Directly Transmitted Infections. Biometrical Journal. 51:3. 387-407.
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
Other measures of assortativeness in \link{CNA} package are \code{\link{assort.Newman}}, \code{\link{assort.Gupta}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{network assortativeness}
\keyword{contact network}