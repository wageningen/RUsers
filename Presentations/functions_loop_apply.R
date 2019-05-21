###
### R Users group Wageningen University, Oct 9th, 2013
### by Joost van Heerwaarden
###
### Adjusted by Luc Steinbuch, for presentation February 14th, 2018
### Further adjusted by Dainius Masiliunas, 2019
### level: basic
###
### Used dataset is related to paper 
### "Genome-wide association study of 107 phenotypes in Arabidopsis thaliana inbred lines."
browseURL("http://doi.org/10.1038/nature08800")


####### Load and explore data ####

load("pheno_data_and_markers.Rdata")

# list of objects in memory
objects()

class(pheno.data)
dim(pheno.data)
names(pheno.data)

pheno.data[1:10, 1:6]
pheno.data[1:10, 39:41]

## filter out one genetic group (rows) and one trait (column)
tmp <- pheno.data[pheno.data$genetic.group==1,"X1_LD"]

# note row selection
pheno.data$genetic.group[1:20]
(pheno.data$genetic.group==1)[1:20]

# and column selection
pheno.data[,"X1_LD"]
# which equals 
pheno.data[,2]
# and
pheno.data$X1_LD


# investigate tmp
tmp
class(tmp)
dim(tmp)
length(tmp)


####### Define simple function ####

mean(tmp) 
# one or more NA's (Not Available) > answer is NA, unusable

mean(tmp,na.rm=TRUE)
# NA's have to be ReMoved 

# Note: default setting is "na.rm = FALSE", see help function
?mean


#####custom made function
My.mean<-function(x){

  result<-sum(x,na.rm=TRUE)/sum(!is.na(x))

  return(result)
}


# note how this function is constructed:
# numerator
tmp
sum(tmp,na.rm=TRUE) 

# denominator
is.na(tmp)
!is.na(tmp)
class( !is.na(tmp) )
sum(!is.na(tmp))


## Does our function work?
My.mean(tmp)


## Note that a function is also an object
objects()

## Conditionals

## if

# Define a function that combines two values based on a strategy
combine_using = function(x, y, using="sum")
{
    # If: if the condition evaluates TRUE, run the following
    if (using == "sum") {
        return(x + y)
    } else { # If it does not, run the following
        stop(paste("Don't know how to combine x and y using", using))
    }
}

combine_using(1, 11, "sum")
combine_using(1, 11, "difference")

## ifelse

# Same, but more concise
combine_using = function(x, y, using="sum")
{
    # ifelse can be seen as an assignable "if"
    result = ifelse(using == "sum", x + y, stop(paste("Don't know how to combine x and y using", using)))
    return(result)
}

combine_using(1, 11, "sum")
combine_using(1, 11, "difference")

combine_using = function(x, y, using="sum")
{
    # We could chain ifelses (e.g. for more functionality)
    result = ifelse(using == "sum", x + y,
        ifelse(using=="difference", x - y,
            stop(paste("Don't know how to combine x and y using", using))))
    return(result)
}

combine_using(1, 11, "sum")
combine_using(1, 11, "difference")
combine_using(1, 11, "multiplication")

## switch (case)

# Most times it's easier to use switch for complex if(else) chains
combine_using = function(x, y, using="sum")
{
    result = switch(using,
        "sum"=x + y,
        "difference"=x - y,
        "multiplication"=x * y,
        stop(paste("Don't know how to combine x and y using", using)) # Last line is the default, i.e. if nothing matches
    )
    return(result)
}

combine_using(1, 11, "sum")
combine_using(1, 11, "difference")
combine_using(1, 11, "multiplication")
combine_using(1, 11, "division")

##### a slightly more complicated custom made function ####
## data: the dataframe unmder investigation
## var: column of interest
## factor: column used to make row selection
## level: defenition of rows to be selected

subset_mean<-function(data,var,factor,level){

x <- data[,var] 

result <- My.mean(x[which(data[,factor]==level)])  

return(result)  

}

# example of use
subset_mean(data=pheno.data,var="X1_LD",factor="genetic.group",level=1)

# Column selection:
x <- pheno.data[,"X1_LD"]

# Elements in vector selection
# for rows of dataframe selection
# data[,factor]==level) with specific objects
pheno.data[,"genetic.group"]==1

# "which" returns the row numbers
which(pheno.data[,"genetic.group"]==1)

x[which(pheno.data[,"genetic.group"]==1)]
x[pheno.data[,"genetic.group"]==1]

# Finally, this vector is given to the earlier 
# defined function My.mean:
# My.mean(x[which(data[,factor]==level)]) 


## Again: test subset_mean
subset_mean(data=pheno.data,var="X1_LD",factor="genetic.group",level=1)

# The short form also works, if you keep the same order in function arguments
subset_mean(pheno.data,"X1_LD","genetic.group",1)


### A simple for loop ####

# to avoid typing by hand:
subset_mean(data=pheno.data,var="X1_LD",factor="genetic.group",level=1)
subset_mean(data=pheno.data,var="X1_LD",factor="genetic.group",level=2)
subset_mean(data=pheno.data,var="X1_LD",factor="genetic.group",level=3)
#etc., we script a for loop

# create vector of different genetic groups
group.vector<-unique(pheno.data$genetic.group)
group.vector

res<-NULL # result vector, empty
res

for (i in group.vector){
  # Calculate mean of genetic.group i
  t <- subset_mean(pheno.data,var="X1_LD",factor="genetic.group",level=i)
  # Place the result in the (growing) vector
  res<-c(res,t)   
}

res
# for readability, assign names to vector elements
names(res)<-group.vector 
res 

# Note that the names are strings, in 
# this case representing numbers
class(names(res))

barplot(res)

## Note: apart from 'for' loops, there are also
## 'while' and 'repeat' loops, see 
## https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r

## foreach loops
# As above, but with foreach

# It is an external package
library(foreach)

# Shorter and easier to read
foreach(i=group.vector, .combine=c) %do% {
    subset_mean(pheno.data,var="X1_LD",factor="genetic.group",level=i)
}

### Nested for loop: traits & genetic groups ####

res_tot <- NULL # now null, but will become a matrix
group.vector <- unique(pheno.data$genetic.group)

# We need the column names of the traits, so all the columnnames
# except for "genotype" and "genetic.group"
trait.vector <- setdiff(colnames(pheno.data),c("genotype","genetic.group"))
trait.vector
# Same as
colnames(pheno.data)[!colnames(pheno.data) %in% c("genotype","genetic.group")]

for (i in group.vector){

  res<-NULL  # clean vector every genetic group
  for (j in trait.vector){
      
        t<-subset_mean(pheno.data,var=j,factor="genetic.group",level=i) 
            
        res<-c(res,t)     
    }
  
  res_tot<-rbind(res_tot,res)  
  
}

rownames(res_tot)<-group.vector
colnames(res_tot)<-trait.vector

dim(res_tot)
res_tot[,1:6]

# foreach

res_tot = foreach(i=group.vector, .combine=rbind) %do% {
    foreach(j=trait.vector, .combine=c) %do% {
        subset_mean(pheno.data,var=j,factor="genetic.group",level=i) 
    }
}

rownames(res_tot)<-group.vector
colnames(res_tot)<-trait.vector

dim(res_tot)
res_tot[,1:6]

#### apply ####

## first have a look at our other, much larger, supplied dataframe
class(markers)
dim(markers)

markers[1:20,1:10]

##try this for loop, first 2500 of 216095
freq<-NULL
t0<-Sys.time()  # to measure calculation time
for(i in 1:2500 ) {
    freq<-c(freq,My.mean(markers[i,]))
    if(round(i/500)==(i/500)) print(i) # for convenience, print progress
  }
Sys.time()-t0   

length(freq)

freq[1:20]
names(freq) <- rownames(markers[1:2500,])
freq[1:20]

remove(freq) 

# foreach equivalent, not any faster in this case
t0<-Sys.time()
freq = foreach(i=1:2500, .combine=c) %do% {
    if(round(i/500)==(i/500)) print(i) # for convenience, print progress
    My.mean(markers[i,])
  }
Sys.time()-t0

## Now try this (note: all 216095 rows )
t0<-Sys.time()
freq<-apply(X      = markers, # data of interest; dataframe or matrix
            MARGIN = 1,       # apply FUN per row
            FUN    = My.mean) # function to apply
Sys.time()-t0

names(freq) <- rownames(markers)
freq[1:20]

hist(freq)

## What did we do? 
## "apply" applies a function over rows, or columns, 
# of a matrix or dataframe

# other members of the apply family:


#### lapply and sapply ####
# "l"apply: list apply, apply a function to each list element, returning a list
# "s"apply: simple apply, same as lapply but tries to return something simpler than a list
# such as a vector, matrix, etc.

#first, create a list
(group.list<-tapply(pheno.data[,"X1_LD"],pheno.data$genetic.group,list))

class(group.list)
names(group.list)
str(group.list)

# select one element from the list
group.list[["1"]]
class(group.list[["1"]])


### lapply applies FUN to each element of a list
# and returns a list
result<-lapply(group.list,My.mean)
result
class(result)

### sapply applies FUN to each element of a list
# and returns a vector or matrix
result<-sapply(group.list,My.mean)
result
class(result)

#### mapply ####
# "m" because of it is a Multivariate version of sapply
# It combines input from several objects, for a function with 
# several arguments

vector1 <- c(1  , 2,    3, 4, 5)
vector2 <- c(100, 0, -100, 0, 1)

# define a two arguments function
My.two.arg.fun <- function(x,y)
{
  return(x+y)
}

mapply(FUN = My.two.arg.fun,
       vector1,
       vector2
       )


#### vapply ####
# equals sapply but also Validates on
# an explicit type-of-return-value definition;
# this is slightly faster and can make
# your code safer in large projects
result<-vapply(X = group.list,
               FUN = My.mean,
               FUN.VALUE = 1) # answer is numerical 
result
class(result)

result<-vapply(X = group.list,
               FUN = My.mean,
               FUN.VALUE = "a") # answer is character

#Fails!

result<-vapply(X = group.list,
               FUN = My.mean,
               FUN.VALUE = complex(real = 1, imaginary = 1)
               # answer is complex number
)
result
class(result)

#### tapply ####
# "t" because of simularity with "table" function

## "tapply" groups by factor, returns 
tapply(X = pheno.data[,"X1_LD"], # a vector
       INDEX = pheno.data$genetic.group,
       FUN = My.mean)
# reminder:
pheno.data$genetic.group
# so: X is factorized (grouped) by tapply

#### rapply ####
# applies a function Recursively on lists within list

# create a nested list
nested_list <- list(
  list(a = pi, 
       b = list(
            c = 1)
       ), 
  d = "a test"
  )

# show the STRucture of the list
str(nested_list) 

rapply(nested_list,
       f = class # a function with a single argument
       )



#### Overview apply family ####
# ! literally taken from https://stackoverflow.com/questions/3505701/grouping-functions-tapply-by-aggregate-and-the-apply-family

## apply - When you want to apply a function to the rows or columns of 
# a matrix (and higher-dimensional analogues); not generally advisable 
# for data frames as it will coerce to a matrix first.

## lapply - When you want to apply a function to each element of a list 
# in turn and get a list back.

## sapply - When you want to apply a function to each element of a list 
# in turn, but you want a vector back, rather than a list.

## vapply - When you want to use sapply but perhaps need to squeeze 
# some more speed out of your code.

## mapply - For when you have several data structures (e.g. vectors, lists) 
# and you want to apply a function to the 1st elements of each, and then 
# the 2nd elements of each, etc., coercing the result to a vector/array as 
# in sapply.

## rapply - For when you want to apply a function to each element of a 
# nested list structure, recursively.

## tapply - For when you want to apply a function to subsets of a vector 
# and the subsets are defined by some other vector, usually a factor.


# some more related functions

#### outer ####
# Applying functions over all combinations of 
# elements from two vectors

x <- 1:10
y <- 11:21

out<-outer(x,y,FUN="+")
colnames(out) <- y
rownames(out) <- x

out

## expand.grid
# Same as outer but doesn't apply a function, just gives a long format data.frame of combinations

expand.grid(x,y)

## by
# Remember "tapply" which groups by factor
tapply(X = pheno.data[,"X1_LD"],
       INDEX = pheno.data$genetic.group,
       FUN = My.mean)

# Wrapper for tapply for data.frames
(by_res = by(pheno.data[,"X1_LD"],
       INDICES = pheno.data$genetic.group,
       FUN = My.mean))

class(by_res) # class "by" which is also a list
unlist(as.list(by_res)) # a bit annoying to deal with when you expect a data.frame

#### aggregate ####
# Applying functions over factor levels, multivariate

# now multivariate, and with slightly different arguments:
(result <- aggregate(x   = pheno.data[,trait.vector],
                    by  = list(pheno.data$genetic.group), # this must be a list!
                    FUN = My.mean))

## by and aggregate with iris: getting mean of each trait per species

(by_iris = by(iris[,-which(names(iris)=="Species")], iris$Species, colMeans))
Reduce(rbind, by_iris) # Reduce or do.call to merge list elements into a data.frame

# With aggregate; note that we do mean rather than colMeans, as input is vector
aggregate(iris[,-which(names(iris)=="Species")], list(iris$Species), mean)

############## The End
