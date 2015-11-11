# http://blogs.uoregon.edu/rclub/2015/06/02/easy-massively-parallel-r-on-uos-aciss-cluster/
# https://nsaunders.wordpress.com/2015/04/01/configuring-the-r-batchjobs-package-for-torque-batch-queues/

# http://aciss-computing.uoregon.edu/2013/09/04/how-to-submission-queues/

###################################
# First time: install BatchJobs
###################################
library(devtools)
install_github("tudo-r/BatchJobs")
library(BatchJobs)

###########################################################
# configure BatchJobs (do this on your local machine, and then use sftp to send .Batchjobs.R and simple.tmpl to your working directory on ACISS)
###########################################################
# set up batchjobs configuration for this project to override the global settings
batch.conf <- readLines("/Library/Frameworks/R.framework/Versions/3.1/Resources/library/BatchJobs/etc/BatchJobs_global_config.R")
batch.conf[1] <- "cluster.functions = makeClusterFunctionsTorque('simple.tmpl')"
## to run debug function for BatchJobs, change the configuration file:
# batch.conf[1] <- "cluster.functions = makeClusterFunctionsInteractive()"
# batch.conf[7] <- "debug = TRUE"

writeLines(batch.conf, ".BatchJobs.R", sep="\n") # write this file to the current working directory

# from https://raw.githubusercontent.com/tudo-r/BatchJobs/master/examples/cfTorque/simple.tmpl
simple <- "#PBS -N <%= job.name %>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%= log.file %>
#PBS -l walltime=<%= resources$walltime %>,nodes=<%= resources$nodes %>,vmem=<%= resources$memory %>M
## remove this line if your cluster does not support arrayjobs
#PBS -t 1-<%= arrayjobs %>

## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore '<%= rscript %>' /dev/stdout
"

writeLines(simple, "simple.tmpl", sep="\n")

###########################################################
# get files ready for ACISS
###########################################################
save(contexts, file = "contexts")
#save(radon.1, file = "r_obj/radon1")
#load(file = "r_obj/radon1")
# copy utt_orth_phon_KEY.txt to server
# copy contexts to server (the R file saved above)
# copy dict_all3_updated.txt to server
# copy simple.tmpl
# copy .BatchJobs.R


###########################################################
# the model (run this in R on ACISS)
###########################################################
library(BatchJobs)
# getConfig()

# install.packages("dplyr", "tidyr", "doParallel")
library(dplyr); library(tidyr); library(doParallel); library(devtools)
sessionInfo() # to check

starts <- 1:12


batch_function <- function(starts){
  library(dplyr)
  library(tidyr)
  library(devtools)
  
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")
  
  # note that key should have the context columns already (from lists, human coding, or topic modeling, etc.)
  key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
  df <- key
  if(nrow(df) == 0) stop("df didn't load")
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  cols <- ncol(dict)
  if(nrow(dict) == 0) stop("dict didn't load")
    
  iter <- 12 # the number of times to generate random samples

  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, .combine = rbind, .packages=c("dplyr", "tidyr", "devtools") ) %dopar% par_function(df=df, dict=dict)
  # save(r, file="bootstrap_results.data")
}


# create a registry
id <- "bootstrap"
reg <- makeRegistry(id = id)

# map function and data to jobs and submit
ids  <- batchMap(reg, batch_function, starts)
done <- submitJobs(reg, resources = list(nodes = 10, ppn=iter))

showStatus(reg)

results <- data.frame(V1=NULL)
nodes <- list.files(paste0(id, "-files/jobs"))
for(i in 1:length(nodes)){
  if (i < 10){
    load(paste0(id, "-files/jobs/0", i, "/", i, "-result.RData"))
  } else {
    load(paste0(id, "-files/jobs/", i, "/", i, "-result.RData"))
  }
  results <- rbind(results, result)
}
# save(results, file="batchresults") # not working
