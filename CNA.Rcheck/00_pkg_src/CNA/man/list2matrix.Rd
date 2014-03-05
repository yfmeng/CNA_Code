\name{list2matrix}
\alias{list2matrix}
\alias{list2matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Edge List to Contact Matrix
}
\description{
}
\usage{
list2matrix(id.names, contact, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id.names}{
a vector, containing column names of the edge list
}
  \item{contact}{
  name of the column with contact data
%%     ~~Describe \code{contact} here~~
}
  \item{data}{
  a data frame
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a contact matrix.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
males<-c('m2','m1','m3','m1','m4')
females<-c('f1','f4','f2','f2','f3')
sexuality<-c(1.2, 2, 1.1, 3, 1)
data<-data.frame(males,females,sexuality)
id.names<-names(data)[1:2]
m<-list2matrix(id.names, sexuality, data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
