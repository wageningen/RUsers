# Example: correlation analysis of a whole lot of covariates
# Generate some random data

Iter = 1000
Covariates = sapply(1:Iter, rnorm, n=Iter)
# We want to know how each covariate correlates with each other covariate! Also using Kendall's Rho.
# We want to do this:
cor(Covariates, method="pearson")
# Except with method="kendall". Don't try that at home, it will crash your R!

# We can try with a single column (takes 10s on my machine):
system.time(cor(Covariates[,1], Covariates[,-1], method="kendall"))

library(pbapply)
pbsapply(1:ncol(Covariates), function(idx) cor(Covariates[,idx], Covariates[,-idx], method="kendall")) # Oh dear, it will take a few hours!
pbsapply(1:ncol(Covariates), function(idx) cor(Covariates[,idx], Covariates[,-idx], method="kendall"), cl=parallel::detectCores()) # Now it will take half an hour!

## Enter Spark
# Define where to find Spark installation
Sys.setenv(SPARK_HOME="/usr/hdp/current/spark2-client/", SPARK_MAJOR_VERSION=2)
# Load SparkR. Docs: https://spark.apache.org/docs/latest/sparkr.html
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Start Spark session
# Start just a local session (ask to run on current machines with all cores)
sparkR.session()
# Status can be seen on: http://localhost:4040

# In theory, we should be able to start a cluster session.
# However, this is disabled on VITO as it requires connecting to our VM, which is firewalled.
#sparkR.session(master="yarn", deployMode="client", sparkConfig = list(spark.dynamicAllocation.enabled=TRUE, spark.shuffle.service.enabled=TRUE))

ResultList = spark.lapply(1:ncol(Covariates), function(idx) cor(Covariates[,idx], Covariates[,-idx], method="kendall"))
Result = simplify2array(ResultList, FALSE)

sparkR.session.stop()

## See SparkR-VITO.R for actually running it on the VITO cluster


## Addendum: compare Big-O notations (curse of dimensionality) of Pearson vs Kendall
CorFunction = function(Iter, method="pearson"){system.time(cor(sapply(1:Iter, rnorm, n=Iter), method=method))}
PearsonTimings = sapply(2:300, CorFunction)
PearsonTimings = data.frame(id=1:ncol(PearsonTimings), elapsed=PearsonTimings["elapsed",])
KendallTimings = sapply(2:100, CorFunction, method="kendall")
KendallTimings = data.frame(id=1:ncol(KendallTimings), elapsed=KendallTimings["elapsed",])

plot(elapsed~id, KendallTimings, xlim=c(0, max(nrow(PearsonTimings), nrow(KendallTimings))))
points(elapsed~id, PearsonTimings, col="blue")
lines(fitted(lm(elapsed~I(id^4), data=KendallTimings))) # O(n^4)
lines(fitted(lm(elapsed~I(id^2), data=PearsonTimings))) # O(n^2)
