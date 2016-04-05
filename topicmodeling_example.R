holmes <- readLines("/Users/TARDIS/DropBox/holmes.txt")
data <- read.csv("/Users/TARDIS/DropBox/poliblogs2008.csv", row.names=1)

# install.packages("stm", "SnowballC")
library(stm)
# prep
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta) # removes infrequent terms depending on user-set parameter lower.thresh (the minimum number of documents a word needs to appear in in order for the word to be kept within the vocabulary)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# estimation
# Metadata covariates for topical prevalence allow the observed metadata to affect the frequency with which a topic is discussed. 
# Covariates in topical content allow the observed metadata to affect the word rate use within a given topic–that is, how a particular topic is discussed.
# A topical content variable allows for the vocabulary used to talk about a particular topic to vary.
poliblogPrevFit <- stm(out$documents, # the documents
                       out$vocab, # the words
                       K = 20, # 20 topics
                       prevalence =~ rating + s(day), # Topical prevalence captures how much each topic contributes to a document. (variable “day” to be estimated with a spline)
                       max.em.its = 75, # set to run for a maximum of 75 EM iterations
                       data = out$meta, # all the variables
                       init.type = "Spectral")  # As with all mixed-membership topic models, the posterior is intractable and non-convex, which creates a multimodal estimation problem that can be sensitive to initialization. Put differently, the answers the estimation procedure comes up with may depend on starting values of the parameters (e.g., the distribution over words for a particular topic). There are two approaches to dealing with this that the STM package facilitates. The first is to use a specific intitialization based on the method of moments, which is deterministic and globally consistent under reasonable conditions (Roberts et al. Forthcoming). This is known as a spectral initialization.10 In practice we have found this intialization to be very helpful. This can be chosen by setting init.type = "Spectral" in the stm function. We use this option in the above example. This means that no matter the seed that is set, the same results will be generated. However, it currently does not scale to extremely large vocabularies (uncommon in most applications) in which case alternative initializations are available
# searchK() uses a data-driven approach to selecting the number of topics

# results
labelTopics(poliblogPrevFit, c(3, 7, 20))
plot.stm(,type = "labels") 
# findThoughts function will print the documents highly associated with each topic