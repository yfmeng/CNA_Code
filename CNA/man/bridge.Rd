\name{bridge}
\alias{bridge}
\alias{bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identification of Bridging Nodes in Networks
}
\description{
bridge detects bridging nodes between two or more population groups.
}
\usage{
bridge(x1id, x2id, x1, x2, contact, data, gap...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1id}{
identifier of the individual at one end of a link
}
  \item{x2id}{
identifier of the other individual
}
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
  \item{gap}{
numeric, the individual is identified as a bridging node in the network if the distance between two of his/her/its neighbours is larger than gap
  }
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The bridge function uses edge list data of contact networks.
}
\value{
The returned value is a table, listing the id, degree, maximum gap among neighbours of the indivduals and whether the individual is a bridging node.
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
\code{\link{summary.bridge}} provides summary of the output table from bridge function.
}
\examples{
male.id<-c(1, 2, 3, 4, 4, 5, 5, 6, 7)
female.id<-c(8, 9, 10, 10, 11, 12, 10, 13, 14)
male.age <- c(15, 20, 18, 40, 40, 26, 26, 25, 23)
female.age <- c(19, 14, 15, 15, 45, 24, 15, 25, 22)
contact<-c(1, 1.2, 0.5, 0.2, 0.8, 1.4, 1, 2, 1.1)
r<-data.frame(male.id,female.id, male.age,female.age,contact)
gap<-10
this.bridge<-bridge(male.id,female.id,male.age,female.age,contact,data=r,gap=10)
print(this.bridge)
}

\keyword{contact network}
\keyword{network bridging}% __ONLY ONE__ keyword per line
