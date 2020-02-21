# NOTE: the file *MUST* end with .R (uppercase!), otherwise it's not recognised as an R script
# Run this from the terminal with bash ./spark-submit-vito.sh

# No need to set any environment variables, spark-submit takes care of that
library(SparkR)

# Driver-side data
Iter = 1000
Covariates = sapply(1:Iter, rnorm, n=Iter)

# Register with the application master; again settings are preset by spark-submit
sparkR.session()

# Anything running on spark.lapply() runs on an executor
ResultList = spark.lapply(1:ncol(Covariates), function(idx) cor(Covariates[,idx], Covariates[,-idx], method="kendall"))
# Result is a list, get a matrix
Result = simplify2array(ResultList, FALSE)
# As the driver is also on the cluster, we need to communicate over files
# Write into a directory that both the cluster and our user can read
write.csv(Result, "/data/users/Public/greatemerald/spark-example-result.csv")

# Make sure to cleanly quit so the Application Master can claim success
sparkR.session.stop()
