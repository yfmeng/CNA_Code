\name{sd.jackknife}
\alias{sd.jackknife}
\alias{sd.jackknife}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Error Using Jackknife Method
}
\description{
sd.jackknife estimates the standard error of a network mixing coefficient using a jackknife method.
}
\usage{
sd.jackknife(x1, x2, contact, data, cate, method, ...)
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
  \item{method}{
  a string, the name of the function that calculate mixing coefficient. Available choices are 'assort.Newman','assort.Gupta','assort.Farrington'
}
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
\code{\link{assort.Newman}},
\code{\link{assort.Gupta}},
\code{\link{assort.Farrington}},
\code{\link{sd.kappa}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
\keyword{ standard error }
\keyword{ jackknife }% __ONLY ONE__ keyword per line
