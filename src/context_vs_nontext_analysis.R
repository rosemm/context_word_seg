library(ProjectTemplate)
load.project()

# colors <- c("#984ea3", "#377eb8", "#e41a1c") # red, blue, purple
colors <- c("#FD8D3C", "#7FCDBB", "#1D91C0") # orange, light blue, dark blue
names(colors) <- c("CJ", "STM", "WL")

exclude <- ds_results %>% 
  filter(N.utts < 100)  %>% 
  dplyr::select(method, context, measure, context_est) %>% 
  unique() %>% 
  tidyr::spread(key=measure, value=context_est) 

# exclude contexts with fewer than 100 utterances
ds_boot_tests <-  dplyr::filter(ds_boot_tests, !context %in% exclude$context)

# exclude contexts with fewer than 100 utterances
ds_results <- dplyr::filter(ds_results, !context %in% exclude$context) 

cm_boot_tests <- cm_boot_tests %>%
  # exclude contexts with fewer than 100 utterances
  dplyr::filter(!context %in% exclude$context) 

# exclude contexts with fewer than 100 utterances
cm_results <- cm_results %>% 
  dplyr::filter( !context %in% exclude$context)

cm_results$method <- factor(cm_results$method, levels=c("WL", "STM", "HJ"), labels=c("WL", "STM", "CJ"))
ds_results$method <- factor(ds_results$method, levels=c("WL", "STM", "HJ"), labels=c("WL", "STM", "CJ"))
cm_boot_tests$method <- factor(cm_boot_tests$method, levels=c("WL", "STM", "HJ"), labels=c("WL", "STM", "CJ"))

# descriptives ---------------------------------------------------------------------

ds_results %>%  
  plot_context_vs_nontext(outcome="TTR", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%  
  plot_context_vs_nontext(outcome="prop.most.freq",
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%
  plot_context_vs_nontext(outcome="mean.words.per.utt", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%
  plot_context_vs_nontext(outcome="prop.one.word.utt", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%  
  plot_context_vs_nontext(outcome="TTR",
                          Z.score=TRUE,
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%  
  plot_context_vs_nontext(outcome="prop.most.freq",
                          Z.score=TRUE,
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%
  plot_context_vs_nontext(outcome="mean.words.per.utt", 
                          Z.score=TRUE,
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

ds_results %>%
  plot_context_vs_nontext(outcome="prop.one.word.utt", 
                          Z.score=TRUE,
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

# save plots of Z scores as bars and points
plot.z.data <- ds_results %>% 
  dplyr::select(method, context, measure, Z_est, N.utts) %>% 
  unique() %>% 
  dplyr::filter(measure %in% c("TTR", "prop.most.freq", "mean.words.per.utt", "prop.one.word.utt")) 
for(i in unique(plot.z.data$measure)){
    p.bar <- plot.z.data %>% 
      dplyr::filter(measure == i) %>% 
      ggplot(aes(y=Z_est, x=reorder(context, N.utts), fill=method)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + 
      geom_hline(yintercept = 0, lty=2) + 
      facet_wrap(~ method, ncol=1, scales="free") + 
      theme_classic() + 
      scale_fill_manual(values = colors) + 
      theme(text = element_text(size=20), axis.ticks = element_blank()) +
      labs(x=NULL, y="Standard deviations from null mean") + 
      ylim(c(-10, 10)) + 
      theme(axis.text.x = element_text(angle=40, vjust=1, hjust=1))
    p.pnt <- plot.z.data %>% 
      dplyr::filter(measure == i) %>% 
      ggplot(aes(x=Z_est, y=reorder(context, N.utts), fill=method)) + 
      geom_point(aes(color=method), size=4, show.legend = FALSE) + 
      geom_vline(xintercept = 0, lty=2) + 
      facet_wrap(~ method, ncol=1, scales="free") + 
      theme_classic() + 
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks=seq(-10, 10, 2), limits=c(-10, 10)) + 
      theme(text = element_text(size=20), axis.ticks = element_blank()) +
      labs(y=NULL, x="Standard deviations from null mean") 
    print(p.bar)
    print(p.pnt)
    # remove periods, which interfer with LaTex recognizing file extensions
    i.save <- gsub(x=i, pattern=".", replacement = "", fixed=TRUE) 
    ggsave(plot=p.bar, filename = paste("descriptives", i.save, "barplot.png", sep="_"), path=file.path("graphs", "context_vs_nontext"), width=8, height = 11, units="in")
    ggsave(plot=p.pnt, filename = paste("descriptives", i.save, "point.png", sep="_"), path=file.path("graphs", "context_vs_nontext"), width=8, height = 11, units="in")
}

# segmentability ---------------------------------------------------------------------
cm_results %>%
  plot_context_vs_nontext(outcome="token_f.score", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

cm_results %>%
  plot_context_vs_nontext(outcome="token_precision", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

cm_results %>%
  plot_context_vs_nontext(outcome="token_recall", 
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

cm_results %>%
  plot_context_vs_nontext(outcome="token_f.score", 
                          Z.score=TRUE, 
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

cm_results %>%
  plot_context_vs_nontext(outcome="token_precision", 
                          Z.score=TRUE, 
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

cm_results %>%
  plot_context_vs_nontext(outcome="token_recall", 
                          Z.score=TRUE, 
                          methods_wrap=TRUE,
                          xlabs=TRUE, save.to=file.path("graphs", "context_vs_nontext"))

# save plots of Z scores as bars and points
plot.z.data <- cm_results %>% 
  dplyr::select(model, method, context, measure, Z_est, N.utts) %>% 
  unique() %>% 
  dplyr::filter(measure == "token_f.score") 
for(i in unique(plot.z.data$model)){
  for(j in unique(plot.z.data$measure)){
      p.bar <- plot.z.data %>% 
        dplyr::filter(model == i, measure == j) %>% 
        ggplot(aes(y=Z_est, x=reorder(context, N.utts), fill=method)) + 
        geom_bar(stat = "identity", show.legend = FALSE) + 
        geom_hline(yintercept = 0, lty=2) + 
        facet_wrap(~ method, ncol=1, scales="free") + 
        theme_classic() + 
        scale_fill_manual(values = colors) + 
        theme(text = element_text(size=20), axis.ticks = element_blank()) +
        labs(x=NULL, y="Standard deviations from null mean") + 
        ylim(c(-10, 10)) + 
        theme(axis.text.x = element_text(angle=40, vjust=1, hjust=1))
      p.pnt <- plot.z.data %>% 
        dplyr::filter(model == i, measure == j) %>% 
        ggplot(aes(x=Z_est, y=reorder(context, N.utts), fill=method)) + 
        # geom_bar(stat = "identity", show.legend = FALSE) + 
        geom_point(aes(color=method), size=4, show.legend = FALSE) + 
        geom_vline(xintercept = 0, lty=2) + 
        facet_wrap(~ method, ncol=1, scales="free") + 
        theme_classic() + 
        scale_color_manual(values = colors) +
        theme(text = element_text(size=20), axis.ticks = element_blank()) +
        labs(y=NULL, x="Standard deviations from null mean") + 
        scale_x_continuous(breaks=seq(-10, 10, 2), limits=c(-10, 10)) 
      print(p.bar)
      print(p.pnt)
      # remove periods, which interfer with LaTex recognizing file extensions
      i.save <- gsub(x=i, pattern=".", replacement = "", fixed=TRUE) 
      j.save <- gsub(x=j, pattern=".", replacement = "", fixed=TRUE) 
      ggsave(plot=p.bar, filename = paste( i.save, j.save, "barplot.png", sep="_"), path=file.path("graphs", "context_vs_nontext"), width=8, height = 11, units="in")
      ggsave(plot=p.pnt, filename = paste( i.save, j.save, "point.png", sep="_"), path=file.path("graphs", "context_vs_nontext"), width=8, height = 11, units="in")
  }
}

# --------------------------------------------------------------------------------------
# some plots showing how corpus descriptives relate to 
# the degree to which context sub-corpora distinguish themselves from random sub-coropra
cm_boot_tests %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=N.utts, y=Z_est, group=model)) + 
  geom_point(aes(color=method), size=3) + 
  geom_smooth(method="lm", color="black", se=FALSE) + 
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) + 
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Number of utterances in sub-corpus", y="Segmentability\nstandardized on null distributions")
ggsave("Fscores_Nutts.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

cm_boot_tests %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=Z_mean.words.per.utt, y=Z_est, group=model)) + 
  geom_point(aes(color=method), size=3) + 
  geom_smooth(method="lm", color="black", se=FALSE) + 
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) + 
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Mean utterance length\nstandardized on null distributions", y="Segmentability\nstandardized on null distributions")
ggsave("Fscores_Zmeanwordsperutt.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

cm_boot_tests %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=Z_prop.one.word.utt, y=Z_est, group=model)) + 
  geom_point(aes(color=method), size=3) + 
  geom_smooth(method="lm", color="black", se=FALSE) + 
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) + 
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Proportion of one-word utterances\nstandardized on null distributions", y="Segmentability\nstandardized on null distributions")
ggsave("Fscores_Zproponewordutt.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

cm_boot_tests %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=Z_TTR, y=Z_est, group=model)) + 
  geom_point(aes(color=method), size=3) + 
  geom_smooth(method="lm", color="black", se=FALSE) + 
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) + 
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Type-token ratio\nstandardized on null distributions", y="Segmentability\nstandardized on null distributions")
ggsave("Fscores_ZTTR.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

cm_boot_tests %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=Z_prop.most.freq, y=Z_est, group=model)) + 
  geom_point(aes(color=method), size=3) + 
  geom_smooth(method="lm", color="black", se=FALSE) + 
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) + 
  facet_wrap(~ model) + 
  theme_bw() + 
  theme(text = element_text(size=20)) + 
  labs(x="Porportion highest frequency syllable\nstandardized on null distributions", y="Segmentability\nstandardized on null distributions")
ggsave("Fscores_Zpropmostfreq.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

# --------------------------------------------------------------------------------------
# some plots showing how corpus descriptives relate to 
# the degree to which context sub-corpora distinguish themselves from random sub-coropra
# using all nontext samples, not just context sub-corpora
cm_results %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=N.utts, y=value, group=model)) + 
  geom_point(alpha=.05, position = position_jitter(width=100, height=0)) + 
  geom_smooth(color="grey") + 
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 80)) +
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Number of utterances in sub-corpus", y="Segmentability\n(token F-score)")
ggsave("Fscores_Nutts_nontexts.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")

cm_results %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  ggplot(aes(x=N.utts, y=value, group=model)) + 
  geom_point(alpha=.05, position = position_jitter(width=100, height=0)) + 
  geom_smooth(color="grey") + 
  geom_point(aes(y=context_est, color=method), size=2) +
  scale_color_manual(values = colors, name="Approach to\ndefining context") + 
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 80)) +
  facet_wrap(~ model) + 
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  labs(x="Number of utterances in sub-corpus", y="Segmentability\n(token F-score)")
ggsave("Fscores_Nutts_nontexts_with_context_est.png", path = file.path("graphs", "context_vs_nontext"), width = 8, height = 4, units="in")
