\name{sampling.method}
\alias{sampling.method}
\alias{sampling.method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Network Sampling Method
}
\description{
\code{sampling.method} examine whether the input vector contains name and parameters of a valid sampling method
}
\usage{
sampling.method(method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{
a vector, whose first elements are the name and paramteters of a sampling method 
}
}
\details{
Currently implemented methods are 'induced','incidenced','star','snowball' and link 'tracing'.
Valid formats of method vectors are:
 \code{
 method = c('induced', p.node);
 method = c('incident', p.edge);
 method = c('snowball', start.size, increase,resample,waves);
 method = c('star', start.size);
 method = c('tracing', start.size, threshold, recruit.rate);
 }
}
\value{
The returned value is logical false if the input value is invalid.
Otherwise the returned value is a data frame containing parameters of sampling methods.
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
\code{\link{sampling}},
\code{\link{neighbour}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contact network }
\keyword{ network sampling }% __ONLY ONE__ keyword per line
