####################################################################################
# how does corpus size affect precision and recall?
####################################################################################
df <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")

start.columns <- ncol(df)

# Add columns for each "context", with increasing number of utterances. 
# It doesn't matter which utterances are selected for each column since they'll all get shuffled anyway during the bootstrapping process
N.sizes <- 30 # how many corpus sizes to try
min.utt <- 100
max.utt <- nrow(df) - 100 # a sample that includes all of the utterances in the corpus won't have any variability from sample to sample
sizes <- round(seq(from=min.utt, to=max.utt, length.out=N.sizes), 0)
# add a column to the dataframe for each  corpus size to try
for(s in 1:N.sizes){
  df[[paste0("N.utts", sizes[s])]] <- c( rep(1, sizes[s]), rep(0, nrow(df)-sizes[s]) )
}

write.table(df, file="contexts_SizeSim.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# send the above text file to ACISS and use it as the contexts file for a bootstrap analysis
 
####################################################################################
# how does the shape of the distribution affect MI dist, precision, and recall?
####################################################################################