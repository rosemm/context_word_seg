
master_doc_count <- HJ_contexts %>%
  ungroup() %>% 
  dplyr::select(utt, category, coder) %>%
  count(utt, category, coder) %>%
  spread(key=category, value=n, fill = 0) %>%
  dplyr::select(-coder) %>%
  group_by(utt) %>% 
  summarize_all( sum ) %>% 
  separate(col=utt, into=c("file", "UttNum"), sep="_" , remove=FALSE) %>%
  arrange(file, as.numeric(UttNum) ) %>% 
  dplyr::select(-file, -UttNum)

df_HJ_raw <- left_join(df, master_doc_count, by="utt" ) %>% 
  filter( !grepl(x=utt, pattern="hi.*") ) # remove this child, since the transcripts are so short
write.table(df_HJ_raw, file=file.path("context_codes", "human_judgments", "contexts_HJ_raw.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_raw')

master_doc_prop <- master_doc_count %>% 
  ungroup() %>%
  mutate_at(vars(-utt), funs(./5)) 

df_HJ_prop <- left_join(df, master_doc_prop, by="utt" ) %>% 
  filter( !grepl(x=utt, pattern="hi.*") ) # remove this child, since the transcripts are so short
write.table(df_HJ_prop, file=file.path("context_codes", "human_judgments", "contexts_HJ_prop.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_prop')
