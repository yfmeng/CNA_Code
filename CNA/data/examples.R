# library(CNA)
# This is a simple example of applying CNA functions to heterosexual contact data
# The data is a subset of Cape Town Sexual Behaviour Survey, formatted as:

# male.id  female.id   male.age  female.age  contact(sexual contact in the 1st wk of relationships)
# 5500017   6204010       54         47       3
# 5500043   6204015       64         48       3
# 5500043   6204016       64         38       4
# 5500043   6204017       64         19       2
# 5500062   6204019       31         25       2
# 5500070   6204022       43         38       3
# ...       ...           ..         ..       ..

# Read the data from the file '/data/relationship.csv'
#relationships<- read.csv('./data/relationship.csv')

# (Arbitarily) Divide the population into subgroups by age
#age.category<- seq(10,100,5)
#directed <- TRUE

# Calculate assortativity using different methods
#Q<-assort.Gupta(male.age,female.age,contact,cate=age.category,data=relationships,directed)
#N<-assort.Newman(male.age,female.age,contact,cate=age.category,data=relationships,directed)
#I<-assort.Farrington(male.age,female.age,contact,cate=age.category,data=relationships,directed)
# Generate and plot the contact rate matrix
#beta<-contact.beta(male.age,female.age,contact,cate=age.category,data=relationships,directed)
#image(age.category,age.category,beta,col = rainbow(50),zlim = c(0,2,xlab='male age',ylab='female age')

# Generate and plot the contact rate matrix conditional on age distribution
#f.c<-fc(male.age,female.age,contact,cate=age.category,data=relationships,directed)
#image(age.category,age.category,f.c[[1]],col = rainbow(50),zlim = c(0,max(f.c[[1]])/10),xlab='male age',ylab='female age')

# detect bridges who are linked with two (or more) persons in different age categories
# this.bridge<-bridge(male.id,female.id,male.age,female.age,data=relationships,gap=10)
# summary.bridge(this.bridge)
