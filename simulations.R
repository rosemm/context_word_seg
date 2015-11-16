####################################################################################
# how does corpus size affect precision and recall?
####################################################################################

# make a set of "contexts" from 100 utterances to (nearly) the full size of the corpus
df <- contexts_by_size(N.sizes=30)

write.table(df, file="contexts_SizeSim.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# send the above text file to ACISS and use it as the contexts file for a bootstrap analysis
 
####################################################################################
# how does the shape of the distribution affect MI dist, precision, and recall?
####################################################################################
# read in the functions written for this analysis
source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")

# make a skewed corpus and save to text file
corpus.skew <- make_corpus(dist="skewed", size=1000)
write.table(corpus.skew, file="utt_orth_phon_KEY_SKEW.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# use that corpus to generate a size sim contexts file
df.skew <- contexts_by_size(read.table("utt_orth_phon_KEY_SKEW.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") , 
                            N.sizes=30)
write.table(df.skew, file="contexts_SizeSimSKEW.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")


# make a uniform corpus and save to text file
corpus.unif <- make_corpus(dist="unif", size=1000)
write.table(corpus.unif, file="utt_orth_phon_KEY_UNIF.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# use that corpus to generate a size sim contexts file
df.unif <- contexts_by_size(read.table("utt_orth_phon_KEY_UNIF.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char =""), 
                            N.sizes=30)
write.table(df.skew, file="contexts_SizeSimUNIF.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
