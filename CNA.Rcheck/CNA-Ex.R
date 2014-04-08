pkgname <- "CNA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('CNA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CNA-package")
### * CNA-package

flush(stderr()); flush(stdout())

### Name: CNA-package
### Title: Contact Network Assortativeness
### Aliases: CNA CNA
### Keywords: package

### ** Examples
 
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




cleanEx()
nameEx("find.neighbour")
### * find.neighbour

flush(stderr()); flush(stdout())

### Name: find.neighbour
### Title: Find the Neighbourhood of a Set of Nodes
### Aliases: find.neighbour find.neighbour
### Keywords: contact network neighbourhood

### ** Examples

#


cleanEx()
nameEx("list2matrix")
### * list2matrix

flush(stderr()); flush(stdout())

### Name: list2matrix
### Title: Convert Edge List to Contact Matrix
### Aliases: list2matrix list2matrix
### Keywords: ~kwd1 ~kwd2

### ** Examples

males<-c('m2','m1','m3','m1','m4')
females<-c('f1','f4','f2','f2','f3')
sexuality<-c(1.2, 2, 1.1, 3, 1)
data<-data.frame(males,females,sexuality)
id.names<-names(data)[1:2]
m<-list2matrix(id.names, sexuality, data)



cleanEx()
nameEx("matrix2list")
### * matrix2list

flush(stderr()); flush(stdout())

### Name: matrix2list
### Title: Matrix to Edge List Convertion
### Aliases: matrix2list matrix2list

### ** Examples

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



cleanEx()
nameEx("sampling")
### * sampling

flush(stderr()); flush(stdout())

### Name: sampling
### Title: Network Sampling
### Aliases: sampling sampling
### Keywords: contact network network sampling

### ** Examples

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



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
