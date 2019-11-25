

#### Functional Programming ####

# num
x <- 1:10
# type: num[] -> num[]
foo <- function(x) {
  x + 1
}

# functions as objects
class(x)
class(min)
class(foo)

l <- list(min, max, mean)
fooApply <- function(f) {
  f(1:10)
}
sapply(l, class)
sapply(l, fooApply)


Map(foo, x)
# lambda: unnamed function
Map(function(x) x + 1, x)

# type: num -> num
filter_uneven <- function(x) {
  if (x %% 2 == 1) 
    x
  else 
    NA
}

# num, num -> num
Reduce(function(head, tail) c(head, filter_uneven(tail)), x = x)
# function = num -> bool
Filter(function(x) x %% 2 == 1, x)

# Reduce is very similar to do.call and can be useful to parse lapply output
(myList = lapply(x, foo))
Reduce(c, myList)

# function factory
# type: num -> (num -> num)
factory <- function(x) {
  function(y) x + y 
}

plusOne <- factory(1)
plusOne(x)



#### Parallel Programming ####

rm(list = ls())

library(snowfall)

x <- 1:10000

isPrime <- function(x) {
  x %in% 1:2 | all(sapply(2:(x - 1), function(y) x %% y != 0))
}

# sequential
system.time(primes1 <- x[sapply(x, isPrime)])

M <- matrix(1:100, nrow = 10, ncol = 10)

sfInit(parallel = TRUE, cpus = 8)
sfExport(list = c('x', 'isPrime', 'M'))

# parallel: sorted
system.time(primes2 <- x[sfSapply(x, isPrime)])
# parallel: load balanced
system.time(primes3 <- x[unlist(sfClusterApplyLB(x, isPrime))])
# parallel: random order
x_random <- sample(x)
system.time(primes4 <- sort(x_random[sfSapply(x_random, isPrime)]))
system.time(primes5 <- sort(x_random[unlist(sfClusterApplyLB(x_random, isPrime))]))

all.equal(primes1, primes2)
all.equal(primes1, primes3)
all.equal(primes1, primes4)
all.equal(primes1, primes5)


# parallel: extended

df_in <- data.frame(ID = 1:1000, Attribute = rnorm(1000))
sfExport('df_in')

results <- do.call(rbind, sfClusterApplyLB(x, function(i) {
  
  row <- df_in[i,]
  id <- row$ID
  a <- row$Attribute
  
  dist <- rnorm(1000, mean = a)
  
  data.frame(ID = id, Output = mean(dist))
}))

sfStop()

hist(results$Output)

## foreach package ##

library(foreach)
library(doParallel)

registerDoParallel(cores=12)
system.time(
    primesForeach <- foreach (i=x, .combine=c, .multicombine=TRUE, .inorder=TRUE, .maxcombine=10000) %dopar%
    {
        isPrime(i)
    }
)

all.equal(primes1, x[primesForeach])

# With a progress bar
library(doSNOW)

cl <- makeCluster(12)
registerDoSNOW(cl)
pb <- txtProgressBar(max = length(x), style = 3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))
system.time(
    primesForeachSNOW <- foreach(i=x, .combine=c, .multicombine=TRUE, .inorder=TRUE, .maxcombine=10000,
                    .options.snow = opts) %dopar%
    {
        isPrime(i)
    }
)
close(pb)
stopCluster(cl)

all.equal(primes1, x[primesForeachSNOW])

## pbapply package ##

library(pbapply)

system.time(primesPb1 <- x[pbsapply(x, isPrime)])
all.equal(primes1, primesPb1)

system.time(primesPb2 <- x[pbsapply(x, isPrime, cl=12)])
all.equal(primes1, primesPb2)
