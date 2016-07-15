votes <- select(df_HJ_raw, -utt, -orth, -phon)

##########################################
# number of utterances per context
##########################################
counts <- sort(colSums(votes, na.rm = TRUE), decreasing = TRUE)
count.data <- data.frame(n.utts=counts, 
                         context=factor(names(counts), levels=names(counts)))
# over the median number of utterances?
count.data$over.med <- ifelse(count.data$n.utts > median(counts), "yes", "no") 

ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="Number of votes for each context") +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("graphs/HJ/votesper_context.png", width=16, height=5, units="in")
ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="log(Number of votes)") +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("graphs/HJ/votesper_context_logtrans.png", width=16, height=5, units="in")

################################################
# utterances actually assigned to each context
################################################
utterances <- select(df_HJ_bin, -utt, -orth, -phon)
counts <- sort(colSums(utterances, na.rm = TRUE), decreasing = TRUE)
count.data <- data.frame(n.utts=counts, 
                         context=factor(names(counts), levels=names(counts)))
# over the median number of utterances?
count.data$over.med <- ifelse(count.data$n.utts > median(counts), "yes", "no") 

ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="Number of utterances assigned to each context") +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("graphs/HJ/uttsper_context.png", width=16, height=5, units="in")
ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="log(Number of utterances assigned)") +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("graphs/HJ/uttsper_context_logtrans.png", width=16, height=5, units="in")
