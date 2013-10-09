\name{matrix2list}
\alias{matrix2list}
\alias{matrix2list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix to Edge List Convertion
}
\description{
\code{matrix2list} converts contact matrix to an edge list 
}
\usage{
matrix2list(m,directed=TRUE,head=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
the matrix to be converted
}
\item{directed}{
logical, indicating whether the contacts in the network is symmetric
}
\item{head}{
logical, indicating whether the first row/column is the identifier of the nodes
}
\item{...}{
%%     ~~Describe \code{m} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a three-column data frame (see examples).
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
}
\examples{
# m is a contact matrix
m <- matrix(c(0, 1, 0, 4, 0, 0, 0, 0, 1, 2, 1, 0),nrow=3)
rownames(m)<-c('male.1','male.2','male.3')
colnames(m)<-c('female.1','female.2','female.3','female.4')

# the heterosexual contact network is not symmetric
# the first row and the first column are not 
# identifiers of the individuals
edge.list<-matrix2list(m,directed=TRUE,head=FALSE)

# edge.list
# x1.id    x2.id    contact
# male.2 female.1       1
# male.1 female.2       4
# male.3 female.3       1
# male.1 female.4       2
# male.2 female.4       1
}
\keyword{}
