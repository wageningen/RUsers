


#### useful functions to explore objects ####

class()
str()
names()
length()
dim()


#### basic types: vector ####

# https://www.statmethods.net/input/datatypes.html

a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

# subset
a[2]
a[1:2]
a[c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)]
length(a)
dim(a)

# naming
names(a) <- LETTERS[1:length(a)]
a
d <- c(A = 3, B = 2, C = -1)
# subsetting with names
a['A']


#### basic types: matrix ####
# generates 5 x 4 numeric matrix
matrix(1:20, nrow=5,ncol=4)

# another example
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow = 2, ncol = 2, byrow = TRUE,
                   dimnames = list(rnames, cnames)) 
dim(mymatrix)
rownames(mymatrix)
colnames(mymatrix)


#### basic types: data.frame ####

df <- data.frame(X = runif(100, 0, 100),
                 Y = runif(100, 0, 100))
names(df)
head(df, 3)
dim(df)
nrow(df)
ncol(df)

# more careful with the class of this one ...
df2 <- cbind(A = rnorm(10), B = sample(LETTERS, 10))
class(df2)

df3 <- cbind(data.frame(A = rnorm(10)), B = sample(LETTERS, 10))
class(df3)

# subsetting
df[1,] # first line
class(df[1,]) # mind the class!
df[,1] # first column
class(df[,1])
class(df$X)

df[1:10,1]

# sort of map-reduce
res <- do.call(rbind, lapply(1:10, function(i) {
  # do stuff
  # ...
  # summarise results
  data.frame(ID = i, Mean = rnorm(1))
}))
res

# more complex than tables: multi-value cells
random_vector <- function(...) {
  l <- sample(1:10, 1)
  rnorm(l)
}

df$Z <- sapply(1:nrow(df), random_vector)
head(df, 4)
class(df$Z)


#### basic types: lists ####

l <- list(1:10, sample(LETTERS, 5), list(matrix(rnorm(10), ncol = 2)))
l[[1]]
l[[2]]
l[[3]]

# names useful again
l <- list(A = 1:10, B = sample(LETTERS, 5), C = list(matrix(rnorm(10), ncol = 2)))
names(l) <- LETTERS[1:length(l)]
l[['C']]

# simple class representation
obj <- list(File = 'theworld.tif',
            PROJ = 'WGS84',
            Resolution = c(20, 20))

obj2 <- list(File = 'europe.tif',
             PROJ = 'UTM30N',
             Resolution = c(10, 10))

get_file <- function(o) {
  o[['File']]
}

get_resolution <- function(o) {
  o[['Resolution']]
}


# how R is doing it
data <- data.frame(A = runif(100))
data$B <- data$A + rnorm(nrow(data), sd = 0.01)

model <- lm(A ~ B, data)
str(model)  
class(model)
length(model)

