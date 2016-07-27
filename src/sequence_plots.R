library(ProjectTemplate)
load.project()

WL <- df_WL %>% 
  mutate_each(funs(ifelse(. > 0, 1, 0)), -utt, -orth, -phon) # for word list analysis only, make all contexts either 1 or 0 (smoothing over 1.5's from expand_windows)
# remove underscores from WL orth column
WL$orth <- gsub(x=WL$orth, pattern="_", replacement=" ")

STM.bin <- df_STM_bin
LDA.bin <- df_LDA_bin
HJ.bin <- df_HJ_bin

seq_plots(WL, "WL")
seq_plots(HJ.bin, "HJ")
seq_plots(LDA.bin, "LDA")
seq_plots(STM.bin, "STM")
