\name{sampling}
\alias{sampling}
\alias{sampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Network Sampling
}
\description{
\code{sampling} samples subgraphs from networks.
}
\usage{
sampling(id.names,data,method,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id.names}{
a vector of strings, specifying the identifier of nodes
}
  \item{data}{
a data frame, containing the edge list of a network
}
  \item{method}{
a vector, containing the name and parameters of an existing network sampling method
}
  \item{initial}{
a vector, containing the id of pre-specified seeds, used in snowball and tracing method and overwrite randomly generated seeds.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a sampled subset of the original data frame.
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
\code{\link{sampling.method}}
}
\examples{
male.id<-c(1, 2, 3, 4, 4, 5, 5, 6, 7)
female.id<-c(8, 9, 10, 10, 11, 12, 10, 13, 14)
male.age <- c(15, 20, 18, 40, 40, 26, 26, 25, 23)
female.age <- c(19, 14, 15, 15, 45, 24, 15, 25, 22)
contact<-c(1, 1.2, 0.5, 0.2, 0.8, 1.4, 1, 2, 1.1)
r<-data.frame(male.id,female.id, male.age,female.age,contact)
id<-c('male.id','female.id')
#method<-c('star', start.size)
method<-c('star', 4)
sample<-sampling(id.names=id,data=r,method)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contact network }
\keyword{ network sampling }% __ONLY ONE__ keyword per line
