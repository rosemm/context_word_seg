
master_doc_count <- context_cats %>%
  select(utt, category) %>%
  count(utt, category ) %>%
  spread(key=category, value=n, fill = 0) %>%
  separate(col=utt, into=c("file", "UttNum"), sep="_" , remove=F) %>%
  arrange(file, as.numeric(UttNum) ) %>% 
  select(-file, -UttNum)

df_HJ_raw <- left_join(df, master_doc_count, by="utt" ) 
write.table(df_HJ_raw, file=file.path("context_codes", "human_judgments", "contexts_HJ_raw.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_raw')

master_doc_prop <- master_doc_count %>% 
  ungroup() %>%
  dplyr::select(-utt, -file, -UttNum) %>%
  mutate(total=rowSums(., na.rm=FALSE)) %>% 
  mutate_each(funs(./total)) 
master_doc_prop$utt <- master_doc_count$utt  # to add back in the non-numeric utt column
master_doc_prop$total <- NULL  # delete column

df_HJ_prop <- left_join(df, master_doc_prop, by="utt" ) 
write.table(df_HJ_prop, file=file.path("context_codes", "human_judgments", "contexts_HJ_prop.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_prop')


