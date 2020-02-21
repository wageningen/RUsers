library(aws.ec2)
rootkey <- as.data.frame(t(read.table(text=readLines("./rootkey.csv",warn = FALSE), sep = "=", row.names = 1)), stringsAsFactors = FALSE)
Sys.setenv("AWS_ACCESS_KEY_ID" = rootkey$AWSAccessKeyId,
           "AWS_SECRET_ACCESS_KEY" = rootkey$AWSSecretKey,
           "AWS_DEFAULT_REGION" = "eu-central-1")

# Check http://www.louisaslett.com/RStudio_AMI/ for the right AMI
ami <- "ami-059a2456bd2027e31" # Frankfurt
#ami <- "ami-0e9e5245fffe34a3e" # London
#describe_images(ami)

# Check your VPC and Security Group settings
## use existing security group or create a new one
subnets <- describe_subnets()
## security group
my_sg <- describe_sgroups()[[Position(function(sgroup){sgroup$groupName=="r-ec2-sg"},describe_sgroups())]]
if (is.null(my_sg)) {
  my_sg <- create_sgroup("r-ec2-sg", "Allow my IP", vpc = subnets[[1L]])
}

## use existing ip address or create a new one
ips <- describe_ips()
if (!length(ips)) {
  ips[[1L]] <- allocate_ip("vpc")
}

# create an SSH keypair
my_keypair <- describe_keypairs()$`r-ec2-example`
if (is.null(my_keypair)) {
  my_keypair <- create_keypair("r-ec2-example")
}
pem_file <- tempfile(fileext = ".pem")
cat(my_keypair$keyMaterial, file = pem_file)

# Launch the instance using appropriate settings
instances <- run_instances(image = ami, 
                   type = "t2.micro", # <- you might want to change this
                   min = 1,
                   max = 2,
                   subnet = subnets[[1L]], 
                   sgroup = my_sg,
                   keypair = my_keypair,
                   userdata = base64enc::base64encode(charToRaw(("#!/bin/bash\nR -e \"install.packages(c('future'), repos='http://cran.rstudio.com/')\""))))
Sys.sleep(5L) # wait for instance to boot

# associate IP address with instance
instance_ips <- lapply(instances,function(instance){get_instance_public_ip(instance)})
#if (is.na(instance_ip)) {
#  instance_ip <- associate_ip(instances, ips[[1L]])$publicIp
#}
# authorize access from this IP
try(authorize_ingress(my_sg))
try(authorize_egress(my_sg))

# Actual work
library(future)
cl <- makeClusterPSOCK(
  ## Public IPs of EC2 instances
  instance_ips,
  port = "random",
  ## User name (always 'ubuntu') (because that's how it's hardcoded in the ami)
  user = "ubuntu",
  ## Use private SSH key registered with AWS
  rshcmd = c("<rstudio-ssh>", "<putty-plink>", "<ssh>"),
  rshopts = c(
    "-o", "StrictHostKeyChecking=no",
    "-o", "IdentitiesOnly=yes",
    "-i", pem_file # the ssh key-pair saved before
  ),
  dryrun = FALSE,
  revtunnel = TRUE,
  homogeneous = FALSE,
  verbose =TRUE
)
plan(remote, workers=cl)

f = list()
a = 1:10000
i = 1
my_var <- future({5*3})
while (i<10000) {
  ind = i:(i+999)
  b = a[ind]
  f[[length(f)+1]] <- future({
    b^2+4*b+12
  })
  i = max(ind)+1
}
b
value(f)
# clean up
## stop and terminate the instance
stop_instances(instances)
terminate_instances(instances)

## revoke access from this IP to security group
try(revoke_ingress(my_sg))
try(revoke_egress(my_sg))

## delete keypair
delete_keypair(my_keypair)

## release IP address
release_ip(ips[[1L]])

