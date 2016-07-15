
master_doc_count <- context_cats %>%
  select(utt, category) %>%
  count(utt, category ) %>%
  spread(key=category, value=n, fill = 0) %>%
  separate(col=utt, into=c("file", "UttNum"), sep="_" , remove=F) %>%
  arrange(file, as.numeric(UttNum) ) 

df_HJ_raw <- left_join(df, select(master_doc_count, -file, -UttNum), by="utt" ) 
write.table(df_HJ_raw, file="context_codes/human_judgments/contexts_HJ_raw.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_raw')

master_doc_prop <- master_doc_count %>% 
  ungroup() %>%
  dplyr::select(-utt, -file, -UttNum) %>%
  mutate(total=rowSums(., na.rm=FALSE)) %>% 
  mutate_each(funs(./total)) 
master_doc_prop$utt <- master_doc_count$utt  # to add back in the non-numeric utt column
master_doc_prop$total <- NULL  # delete column

df_HJ <- left_join(df, master_doc_prop, by="utt" ) 
write.table(df_HJ, file="context_codes/human_judgments/contexts_HJ.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ')

votes <- master_doc_count[, 4:ncol( master_doc_count )]

##########################################
# number of utterances per context
##########################################
counts <- sort(colSums(master_doc_count[,4:ncol(master_doc_count)]),decreasing = TRUE)
count.data <- data.frame(n.utts=counts, 
                         context=factor(names(counts), levels=names(counts)))
# over the median number of utterances?
count.data$over.med <- ifelse(count.data$n.utts > median(counts), "yes", "no") 

ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="Number of utterances tagged") +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("plots/HJ/uttsper_context.png", width=16, height=5, units="in")
ggplot(count.data, aes(x=context, y=n.utts, fill=over.med)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label=n.utts), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("yes" = "darkgreen","no" = "grey")) +
  labs(x=NULL, y="log(Number of utterances tagged)") +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0)) 
ggsave("plots/HJ/uttsper_context_logtrans.png", width=16, height=5, units="in")
