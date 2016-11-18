###########################################################
# Good practices and Data structures in R - Vincent Garin #
###########################################################

# source of part on data structure: 

# Advanced R book : http://adv-r.had.co.nz/ (Hadley Wickham)

##################
# good practises #
##################

# 1. Use R studio (latest version)
##################################

### 1.1 Dynamic documentation

model <- lm()
help(lm)

### 1.2 Automatic correction

if(x = 3){
  
  
### 1.3 Use R project : File > New project
  
  
### 1.4 Access to many facilities from R studio: packaging, version control
# (git), sweave, etc.
  
  
# 2. Coding style
#################
  
# "Good coding style is like correct punctuation. You can manage without it, but
# it makes sure things easier to read." (H. Wickham)
  
  
### 2.1 Google R style guide: https://google.github.io/styleguide/Rguide.xml
  
# 2.1.1 Line space
  
# The maximum line length is 80 characters
  
# Tools > Global options > Code > display > Margin column (80).
  
# 2.1.2 Spacing
  
# place space around binary operators (=, +, -, <-, etc.)
  
# GOOD
  
a <- 4
b <- 3 + 1
x <- matrix(0, 3, 4)
x[, 3]
model <- lm(formula = mpg ~ wt, data = mtcars)
  
# BAD
  
a<-4
b<-3+1
x <- matrix(0,3,4)
x[,3]
model <- lm(formula=mpg~wt,data=mtcars)
  
# 2.1.3 Braces
  
# GOOD
  
test <- TRUE
  
if(test){
    
    print("a")
    
} else {
    
  print("b")
    
}
  
x <- 1:10
  
for(i in seq_along(x)){
    
    x[i] <- x[i] + 1
    
}
  
x
  
# BAD
  
test <- TRUE
  
if(test){print("a")
    
} else {print("b") }
  
x <- 1:10
  
for(i in seq_along(x)){x[i] <- x[i] + 1}
  
x
  
# 2.1.4 Assignement
  
# GOOD
  
a <- 2
  
# BAD
  
a = 2
  
# 2.1.5 Commentary
  
# Comment your code as much as possible
  
# possible hierarchy:
  
#########
# Title #
#########
  
# Sub-title
###########
  
### paragraph
  
# regular comment
  
# 2.1.7 clean your code
  
library(formatR)
  
a<-4
b<-3+1
x <- matrix(0,3,4)
x[,3]
model <- lm(formula=mpg~wt,data=mtcars)
  
if(test){print("a")
    
} else {print("b") }
  
  
# copy the "messy" code (ctrl+c)
  
# run the following command:
  
# tidy_source(width.cutoff = 70)
  
  
# 2.1.8 Conclusion: TRY TO BE CONSISTENT
  

###################
# Data structures #
###################

# five types of data structures most used in R

#       Homogeneous       Heterogeneous
#
# 1d    Atomic vectors    List
# 2d    Matrix
# nd    Array

# Best mean to know the structure of an object str()

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"))

str(df)

# 1. Vectors
############

# two types of vectors: atomic vectors and list

x <- letters[1:5]
y <- list(a = 1:5, b = c("a", "b"))

# test for a vector

is.atomic(x) || is.list(x)

### 1.1 Atomic vectors

# four types of atomic vectors: double, integer, logical, character

dbl_var <- c(1, 2.5, 4.5)
# With the L suffix, you get an integer rather than a double
int_var <- c(1L, 6L, 10L)
# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

typeof(dbl_var)
typeof(int_var)
typeof(log_var)
typeof(chr_var)

is.double(dbl_var)
is.integer(int_var)

is.numeric(dbl_var)
is.numeric(int_var)

is.logical(log_var)
is.character(chr_var)

# coercion

# All elements of an atomic vector must be the same, so if you try to combine
# elements of different type together, they will be coerce from the least to the
# most flexible type: logical, integer, double, character

c("a", 1)

1:5 > 4

sum(1:5 > 4)

### 1.2 Lists

# The elements of a list can be from any type even list

my.list <- list(a = 1:6, b = matrix(1:12, 3, 4),
                c = data.frame(1:5, letters[1:5]),
                d = vector(mode = "list", 3))

str(my.list)
typeof(my.list)
is.list(my.list)

model <- lm(formula = mpg ~ wt, data = mtcars)

is.list(model)

# transform a list into an atomic vector

unlist(my.list)

# 2. Attributes
###############

# element used to store metadata about the object.

y <- 1:20

attr(y, "my_attribute") <- "this is a vector"

attr(y, "my_attribute")

attr(model$coefficients, "names")

### 2.1 special types of attributes: dimension, name and class

# these attributes can be accessed by specific functions

dim(y)            # not attr(y, "dim")
names(my.list)    # not attr(my.list, "names")
class(my.list)    # not attr(my.list, "class")

### 2.2 Names

# you can name the value of a vector in different way

x1 <- c(a = 1, b = 2, c = 3)
x2 <- 1:3
names(x2) <- letters[1:3]

names(x1)
names(x2)

# lookup is an intresting technique to assisgn value to a character vector of
# observation. For example:

results <- c(male = 10, female = 30)
observation <- sample(c("male", "female"), 50, replace = TRUE)
scores <- results[observation]
scores



### 2.3 Factors

# a factor is a vector that contain only pre-defined values. Can be interesting
# if we know precisely the possible values of a variable.

sex <- factor(c("male", "female", "female", "male"))
sex

sex[3] <- "trans"


# 3. Matrix and Array
#####################

# Matrices and array are extension of the vector into 2 and n dimension

mat <- matrix(1:16, 2, 8)
mat
dim(mat)

arr <- array(1:12, c(2, 3, 2))
arr
dim(arr)

# access specific elements of matrix (array)

mat[2, 5]

arr[1, 2, 2] <- NA

is.matrix(mat)
is.array(arr)

# generalisation of c() argument for matrix: cbind(), rbind()

mat2 <- cbind(c(1, 2), c(3, 4))
mat2

# 4. Data frame
###############

# This is the most common way to store data into R. Data.frame is a list of
# equal vectors of different type so it has properties of matrix and list

is.data.frame(mtcars)
data(Orange)
Orange
is.data.frame(Orange)

# matrix properties

Orange[1, ]
colnames(Orange)
rownames(Orange)

# list properties

Orange$Tree

# the regular behaviour of data.frame is to turn character into factors

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"))

str(df)

# use stringsAsFactors = FALSE to avoid that

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE)

read.table()


### 4.1 Combine data.frame

Orange1 <- Orange[1:17, ]
Orange2 <- Orange[18:35, ]

Orange <- rbind(Orange1, Orange2)

# But

colnames(Orange1)[2] <- "Age"

Orange <- rbind(Orange1, Orange2) # the columnames must match


##################
# Recapitulation #
##################

# Questions
###########

# 1. What is the difference between atomic vector and list?

# 2. is data.frame a list? If yes what kind of list

# 3. try to reproduce the lookup technique to assigne value given a character
# indicator for one of your specific example.

# 4. In the following array, change the value 5 to NA.

arr <- array(1:12, c(2, 3, 2))
arr

# 5. which argument do you need to use to avoid that R convert character into
# factor when you form or load data.frame

# 6. Clean this code using tidy_source from formatR library

a= rnorm(50,mean=2)
b <- 3
c= a+b



# Answers
#########

# 1. The element type of a vector are homogeneous wherease list can have
# different type of elements.

# 2. Data.frame are also list. They are composed of equal size vectors of 
# different types.

# 3.

rad.rate <- c(low = 10, mid = 30, high = 50) # define three radiation level
observation <- sample(c("low", "mid", "high"), 50, replace = TRUE)
score <- rad.rate[observation]
score

# 4.

arr <- array(1:12, c(2, 3, 2))
arr

arr[1, 3, 1] <- NA
arr


# 5. Use !stringsAsFactors = FALSE

################
# subsetting ? #
################