library('ProjectTemplate')
load.project()

# add WL context for each word to dictionary
c <- gather(contexts, key="context", value="word") %>%
  filter(word!="")
c <- left_join(c, dict, by="word") 

c.missed <- filter(c, grepl(x=word, pattern="_", fixed=T)) # grab the bigrams
search_in <- df$orth
for(i in 1:nrow(c.missed)){
  search_for <- c.missed[i, ]$word
  count <- length(grep(x=search_in, pattern=search_for)) 
  c.missed[i, ]$freq.orth <- ifelse(count==0, NA, count)
}
rm(i, search_for, search_in) # cleaning up the workspace

c <- rbind(c, c.missed)
c <- filter(c, !is.na(freq.orth) & freq.orth > 0) %>% 
  select(context, word, freq=freq.orth) %>%
  arrange(desc(freq))
c$word.num <- 1:nrow(c) # adding a number identifier for each word (in desc freq order), to control order of plotting

con.freq <- c   %>%
  group_by(context) %>%
  summarize(mean=mean(freq), max=max(freq), min=min(freq), median=median(freq))

ggplot(c, aes(x=as.factor(word.num), y=freq, fill=context)) + 
  geom_bar(position=position_dodge(), stat="identity", show.legend=F) +
  facet_wrap(~context, scales="free", ncol=4) +
  theme(text = element_text(size=20), axis.ticks = element_blank(), axis.text.x = element_blank() ) +
  labs(x=NULL, y=NULL)
ggsave(filename="plots/wordlists/key_word_freq_by_context.png", width=12, height=5, units="in")