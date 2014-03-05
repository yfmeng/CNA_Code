\name{summary.bridge}
\alias{summary.bridge}
\alias{summary.bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Network Bridging Summaries
}
\description{
summary.brige summarises the bridging behaviour in a network
}
\usage{
summary.bridge(node.bridge)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node.bridge}{
data frame, obtained by \code{\link{bridge}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned object is a data frame, specifying number of nodes, average degree, the maximum bridge width, number of bridges in the network and the percentage of individuals who play the roll of a bridge.
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
\code{\link{bridge}}
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
summary.bridge(this.bridge)

#  "summary of bridging behaviour"
#  no.of.nodes ave.degree max.gap no.of.bridge percentage
#          14       1.29      30            2      14.29
# 2 of the 14 nodes are bridges
# the mean number of partners is 1.29
# the maximum age difference among partners of one individual is 30 years
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contact network }
\keyword{ network bridging }% __ONLY ONE__ keyword per line
