\name{contact.beta}
\alias{contact.beta}
\alias{contact.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Contact Rate Matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{contact.beta} constructs a contact rate matrix based on edge list data
}
\usage{
contact.beta(x1, x2, contact, cate, data, directed = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x1}{
the value of the concerned variable of the individual at one end of a link
}
  \item{x2}{
the value of the concerned variable of the other individual
}
  \item{contact}{
contact rate between two individuals
}
  \item{data}{
a data frame, containing the variables in the model
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
The returned value is a matrix.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contact networks }
