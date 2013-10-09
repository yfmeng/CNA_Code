\name{fc}
\alias{fc}
\alias{fc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contact Density Matrix
}
\description{
\code{fc} constructs a contact density matrix based on edge list data
}
\usage{
fc(x1, x2, contact, cate, data, directed = TRUE, ...)
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
The returned item is a list.
 \item{contact distribution matrix}{
 the distribution of contacts between pupulation subgroups
 }
 \item{population distribution}{the size of each subgroup}
 \item{mean within bins}{ mean value within each subgroup}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
}
\examples{
}
\keyword{contact network}% __ONLY ONE__ keyword per line
