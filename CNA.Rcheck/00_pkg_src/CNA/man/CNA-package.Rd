\name{CNA-package}
\alias{CNA}
\alias{CNA}
\docType{package}
\title{
Contact Network Assortativeness
}
\description{
This package calculate the assortativeness and bridging behaviours of a network using edge list data.

}
\details{
\tabular{ll}{
Package: \tab CNA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-10\cr
License: \tab Artistic Lisence\cr
}
 \code{
 assort.Farrington,
 assort.Gupta,
 assort.Newman,
 bridge,
 summary.bridge,
 contact.beta,
 fc,
 matrix2list,
 neighbour
 sampling,
 sampling.method,
 }
}
\author{
Fei MENG <fei.d.meng@gmail.com>
}

\references{
}

\keyword{ package }
\seealso{
}
\examples{ 
# let data frame r be an edge list dataset of 
# a small heterosexual contact network with 7 males and 7 females
male.id<-c(1, 2, 3, 4, 4, 5, 5, 6, 7)
female.id<-c(8, 9, 10, 10, 11, 12, 10, 13, 14)
male.age <- c(15, 20, 18, 40, 40, 26, 26, 25, 23)
female.age <- c(19, 14, 15, 15, 45, 24, 15, 25, 22)
contact<-c(1, 1.2, 0.5, 0.2, 0.8, 1.4, 1, 2, 1.1)
r<-data.frame(male.id,female.id, male.age,female.age,contact)

# thus the data frame r is an edge list of the network
#  male.id female.id male.age female.age contact
#       1         8       15         19     1.0
#       2         9       20         14     1.2
#       3        10       18         15     0.5
#       4        10       40         15     0.2
#       4        11       40         45     0.8
#       5        12       26         24     1.4
#       5        10       26         15     1.0
#       6        13       25         25     2.0
#       7        14       23         22     1.1

# in order to examine the assortativeness between age groups, 
# divide the population into 5-year age groups
cate<-seq(10,50,5)
directed<-TRUE

# calculate the assortative coefficients using different methods
assort.Gupta(male.age,female.age,contact,cate,data=r,directed)
assort.Newman(male.age,female.age,contact,cate,data=r,directed)
assort.Farrington(male.age,female.age,contact,cate,data=r,directed)

# visualise the contacts between subgroups
beta<-contact.beta(male.age,female.age,contact,cate,data=r,directed)
f.c<-fc(male.age,female.age,contact,cate,data=r,directed)
image(cate,cate,f.c[[1]],col = rainbow(50),zlim = c(0,0.1),xlab='male age',ylab='female age')
image(cate,cate,beta,col = rainbow(50),zlim = c(0,0.1),xlab='male age',ylab='female age')

# identify the bridging nodes
this.bridge<-bridge(male.id,female.id,male.age,female.age,contact,data=r,gap=10)
summary.bridge(this.bridge)

}
