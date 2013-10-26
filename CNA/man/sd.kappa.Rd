\name{sd.kappa}
\alias{sd.kappa}
\alias{sd.kappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Error of Newman's Coefficient using Cohen's Kappa Method
}
\description{
sd.kappa estimates the standard error of Newman's mixing coefficient using a formula that is adapted from Cohen's Kappa.
}
\usage{
sd.kappa(x1, x2, contact, data, cate, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
  the value of the concerned variable of the individual at one end of a link
%%     ~~Describe \code{x1} here~~
}
  \item{x2}{
  the value of the concerned variable of the other individual
%%     ~~Describe \code{x2} here~~
}
  \item{contact}{
  contacts between two individuals
}
  \item{data}{
  a data frame, containing the variables in the model
}
  \item{cate}{
  a vector, specifying the cutpoints of subgroup division}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is numeric.
}
\references{
Cohen, Jacob (1960). "A coefficient of agreement for nominal scales". Educational and Psychological Measurement 20 (1): 37–46. doi:10.1177/001316446002000104
}
\author{
Fei MENG <fei.d.meng@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{assort.Newman}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Newman's network mixing coefficient }
\keyword{ Cohen's kappa coefficient }% __ONLY ONE__ keyword per line
