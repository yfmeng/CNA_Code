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
assort.Gupta(x1, x2, contact, data, cate, directed = TRUE, ...)
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
male.id<-c(1, 2, 3, 4, 4, 5, 5, 6, 7)
female.id<-c(8, 9, 10, 10, 11, 12, 10, 13, 14)
male.age <- c(15, 20, 18, 40, 40, 26, 26, 25, 23)
female.age <- c(19, 14, 15, 15, 45, 24, 15, 25, 22)
contact<-c(1, 1.2, 0.5, 0.2, 0.8, 1.4, 1, 2, 1.1)
r<-data.frame(male.id,female.id, male.age,female.age,contact)
directed<-TRUE

cate<-seq(10,50,10)
Q<-assort.Gupta(male.age,female.age,contact,cate,data=r,directed)
print(Q)
# the Gupta's Q coefficient of the network is 0.78
# if the population is divided into 4 age groups

cate<-seq(10,50,5)
Q<-assort.Gupta(male.age,female.age,contact,cate,data=r,directed)
print(Q)
# the Gupta's Q coefficient of the network is 0.34
# if the population is divided into 9 age groups
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{network assortativeness}
\keyword{contact network}
