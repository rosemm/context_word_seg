# http://blogs.uoregon.edu/rclub/2015/06/02/easy-massively-parallel-r-on-uos-aciss-cluster/
# https://nsaunders.wordpress.com/2015/04/01/configuring-the-r-batchjobs-package-for-torque-batch-queues/

library(devtools)
install_github("tudo-r/BatchJobs")
library(BatchJobs)

list.files(all.files=T)

# typed in console:
cp /Library/Frameworks/R.framework/Versions/3.1/Resources/library/BatchJobs/etc/BatchJobs_global_config.R /Users/TARDIS/Documents/STUDIES/context_word_seg/.Batchjobs.R
# edit the first line of the resulting file (.Batchjobs.R) to look like this:    cluster.functions = makeClusterFunctionsTorque("simple.tmpl")


