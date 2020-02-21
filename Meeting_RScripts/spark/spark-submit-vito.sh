#!/bin/sh
# Log into the cluster first!
kinit
# spark-submit wrapper for running the Spark-VITO.R script on VITO cluster
SPARK_HOME=/usr/hdp/current/spark2-client/ SPARK_MAJOR_VERSION=2 spark-submit --master yarn --deploy-mode cluster --conf spark.shuffle.service.enabled=true --conf spark.dynamicAllocation.enabled=true SparkR-VITO.R
