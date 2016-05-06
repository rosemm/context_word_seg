
threshold_plots <- function(df.model, thresholds, method, save.to, ...){
  
  stopifnot(require(ggplot2), require(dplyr), require(tidyr))
  
  if(toupper(method) == "HJ"){
    colnames(df.model)[4:ncol(df.model)] <- paste0("topic_", colnames(df.model)[4:ncol(df.model)])
  }
  
  df.threshold <- gather(df.model, key="topic", value="loading", starts_with("topic_")) %>% 
    na.omit() %>% 
    as.tbl() # for speed
  
  for(threshold in thresholds){
    df.threshold[[paste0("thresh_",threshold)]] <- ifelse(df.threshold$loading > threshold, 1, 0)
  }
  
  df.threshold <- df.threshold %>% 
    select(-loading) %>% 
    gather(key="threshold", value="include", starts_with("thresh_")) %>% 
    spread(key=topic, value=include) %>% 
    extract(col=threshold, into="threshold", regex="_(0*[.]*[[:digit:]]+)", convert=TRUE) 
  
  df.threshold$num_topics <- rowSums(select(df.threshold, starts_with("topic_")), na.rm=T)
  
  plot.data <- df.threshold %>% 
    group_by(threshold) %>% 
    summarize(mean_num_topics=mean(num_topics, na.rm=T), 
              perc_zero_topic=length(num_topics[num_topics==0])/length(num_topics),
              perc_one_topic=length(num_topics[num_topics==1])/length(num_topics),
              perc_one_or_two_topic=length(num_topics[num_topics < 3 & num_topics > 0])/length(num_topics),
              perc_two_topic=length(num_topics[num_topics==2])/length(num_topics),
              perc_more_topic=length(num_topics[num_topics>2])/length(num_topics)) %>% 
    gather(key="measure", value="value", -threshold)
  plot.data$measure <- factor(plot.data$measure, 
                              levels = c("mean_num_topics", "perc_zero_topic", "perc_one_topic", "perc_one_or_two_topic", "perc_two_topic", "perc_more_topic"),
                              labels = c("mean_num_topics", "zero contexts", "one context", "one or two contexts", "two contexts", "more than two contexts"))
  
  method <- toupper(method)
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  ggplot(filter(plot.data, measure=="mean_num_topics"), aes(x=threshold, y=value)) + 
    geom_line() + 
    scale_y_continuous(breaks=0:12) +
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    geom_line(y=1, linetype=2) +
    labs(y=NULL) +
    ggtitle(paste0(method, ": mean number of contexts per utterance"))
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_means_", additional_args ,".png"), width=8, height=8, units="in")
  
  ggplot(filter(plot.data, measure!="mean_num_topics"), aes(x=threshold, y=value, color=measure)) + 
    geom_line() + 
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    labs(y="Percent of total utterances") +
    ggtitle(paste0(method, ": number of contexts per utterance"))
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_perc_", additional_args ,".png"), width=8, height=8, units="in")
}

apply_threshold <- function(df.model, threshold){
  
  stopifnot(require(dplyr), require(tidyr))
  
  df.bin <- df.model %>% 
    gather(key="topic", value="loading", -utt, -orth, -phon)
  df.bin$include <- ifelse(df.bin$loading > threshold, 1, 0)
  df.bin <- df.bin %>% 
    select(-loading) 
  
  # any contexts that are all 0's (i.e. any contexts which have no utterances assigned)?
  topics.occurring <- unique(filter(df.bin, include==1)$topic)
  message("\nDropping ", length(unique(df.bin$topic)) - length(topics.occurring), " contexts because they are not above threshold on any utterance.\n")
  
  df.bin <- df.bin %>%
    filter(topic %in% topics.occurring) %>% 
    spread(key=topic, value=include)
  
  return(df.bin)
}

# agreement analysis functions
cat_agreement <- function(cat.codes){
  
  stopifnot(length(cat.codes) == 2, require(dplyr), require(tidyr), require(vcd))
  
  message("Assessing agreement between ", paste(c(names(cat.codes)[1],names(cat.codes)[2]), collapse=" and "))
  
  cat1 <- cat.codes[[1]]
  cat2 <- cat.codes[[2]]
  
  cat1.long <- gather(cat1, key="cat1", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  cat2.long <- gather(cat2, key="cat2", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  
  freqs <- full_join(cat1.long, cat2.long, by="utt")
  
  freqs <- freqs %>% 
    count(cat1, cat2)
  
  xt <- xtabs(n ~ cat1 + cat2, data=freqs) # cross-tabs
  names(attr(xt, "dimnames")) <- c(names(cat.codes)[1],names(cat.codes)[2])
  
  chi.sq <- summary(xt) # chi-squared test of independence
  v <- assocstats(xt) # cramers v
  
  return(list(xt, chi.sq, v))
}

logistic_regressions <- function(all.methods, outcome_method, predictor_method, save.to, ...){
  stopifnot(require(dplyr), require(tidyr))
  
  dvs <- all.methods %>% 
    select(starts_with(outcome_method)) %>% 
    as.list()

  predictors <- all.methods %>% 
    select(starts_with(predictor_method)) %>% 
    as.matrix()
  
  # for naming the plots  
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  models <- list()
  summaries <- list()
  plot.data <- data.frame(est=NULL, se=NULL, method=NULL, context=NULL, sig=NULL, outcome=NULL)
  for(i in 1:length(dvs)){
    dv <- dvs[[i]]
    message(names(dvs)[i])
    model <- glm(dv ~ 0 + predictors,  # not estimating an intercept, so we get the expected value for each predictor
                family=binomial(link = "logit"))
    # save the models
    models[[names(dvs)[i]]] <- model
    # save the model summaries
    summaries[[names(dvs)[i]]] <- summary(model)
    # save the coefficient estimates and se's
    p <- summary(model)$coefficients %>% 
      as.data.frame() %>% 
      select(1,2,4)
    colnames(p) <- c("est", "se", "pval")
    p$context <- row.names(p)
    row.names(p) <- NULL
    if( grepl(x=predictor_method, pattern="STM|LDA") ) { # for topic modeling results
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](topic.*)")
    } else {
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](.*)")
    }
    p$context <- factor(p$context, levels=unique(p$context))
    p$sig <- ifelse(p$pval < .05, "sig", "not sig")
    p$pval <- NULL
    p$outcome <- names(dvs)[i]
    plot.data <- rbind(plot.data, p)
    
    plot <- ggplot(p, aes(x=est, y=reorder(context, est), color=sig)) +
      geom_point(show.legend = F) +
      scale_color_manual(values=c("sig"="black", "not sig"="grey")) +
      geom_errorbarh(data=filter(p, sig=="sig"), aes(xmin = est-(2*se), xmax = est+(2*se)), height = .2, show.legend = F) +
      labs(y=NULL, x="Logistic regression coefficients (2SE error bars)", title=names(dvs)[i])
    ggsave(plot, filename=paste0(save.to, "/logisticreg_", names(dvs)[i],"_from_", predictor_method, additional_args ,".png"), width=8, height=8, units="in")
  
  } # end for loop
  
  output <- list(models=models, summaries=summaries, plot.data=plot.data)
  return(output)
}



read_batch <- function(dir, quiet=TRUE){
  # dir="nontexts_WL-files"
  jobs <- list.files(paste0(dir, "/jobs"))
  for(j in jobs){
    load(paste0(dir, "/jobs/", j, "/", as.numeric(j), "-result.RData")) # makes an object named "result"
    if(!is.null(result)){
      if(as.numeric(j)==1){
        results <- result
      } else results <- rbind(results, result)
      if(!quiet) message(j)
    } # end of if !is.null(result) statement
  } # end of for loop
  return(results)
}

process_batch_results <- function(id, dir, combine=c("rbind", "list")){
  # only works in the original working directory where BatchJobs was run (i.e. on ACISS)
  results <- data.frame(V1=NULL)
  id <- paste0(dir, "/", id)
  
  nodes <- list.files(paste0(id, "-files/jobs"))
  if(length(nodes)==0) stop("No completed jobs available. Check dir and id to make sure they're correct.")
  empty_jobs <- NULL
  
  if(combine=="list"){
    results <- loadResults(loadRegistry(paste0(id, "-files"), adjust.paths=TRUE, work.dir=dir))
  } else if(combine=="rbind"){
    for(i in 1:length(nodes)){
      load(paste0(id, "-files/jobs/", nodes[i], "/", as.numeric(nodes[i]), "-result.RData"))
      results <- rbind(results, result)
      if(is.null(result)) empty_jobs <- c(empty_jobs, nodes[i])
    }
  }
  if(!is.null(empty_jobs)) warning(paste("The following jobs were empty:", paste(empty_jobs, collapse=", ")))
  return(results)
  # saveRDS(results, file=paste0("batchresults_WL.rds") )
}

clean_batch_results <- function(results){
  # combine and organize results
  
  # EXAMPLE:
  # # on aciss:
  # # system('rm boot_results.RData')
  # results <- list(SizeSkew=loadResults(reg.size.skew), 
  #                 SizeUnif=loadResults(reg.size.unif),
  #                 TTRskew=loadResults(reg.ttr.skew),
  #                 TTRunif=loadResults(reg.ttr.unif))
  # save(results, file="boot_results.RData")  
  # # copy via sftp to local machine
  # load("bootstrap_results_Mar2016/boot_results.RData") # this is generated on ACISS using loadResults() and then saved and sftp to local machine
  # names(results)
  # 
  # # cleaning on local machine:
  # sim.results.list <- clean_batch_results(results)
  # sim.results <- sim.results.list[[1]]
  
  MIs <- vector("list", length(names(results))); names(MIs) <- names(results) # empty storage variable
  TPs <- vector("list", length(names(results))); names(TPs) <- names(results)  # empty storage variable
  stat.results <- vector("list", length(names(results))); names(stat.results) <- names(results)  # empty storage variable
  for(exp in names(results)){ # exp is the name of the experiment (e.g. SizeSkew)
    for(j in 1:length(names(results[[exp]]))){ # for each job
      if(length(results[[exp]][[j]]) > 0){ # only continue if there are any nodes within this job
        for(n in 1:length(results[[exp]][[j]])){ # for each node
          if(!is.error(results[[exp]][[j]][[n]])){
            if(is.data.frame(results[[exp]][[j]][[n]]$stat.results)){ # only read in results if they exist
              this.result <- results[[exp]][[j]][[n]]$stat.results
              this.result$exp <- exp # add experiment name to results dataframe 
              stat.results[[exp]] <- rbind(stat.results[[exp]], this.result)
            }
            if(!is.null(results[[exp]][[j]][[n]]$MIs)){ # only read in results if they exist
              label <- ifelse(grepl(x=exp, pattern="TTR"), paste0("TTR", as.character(round(this.result$TTR, 2))), as.character(this.result$N.utts))
              this.MIs  <- results[[exp]][[j]][[n]]$MIs
              if(is.list(this.MIs)) this.MIs <- this.MIs[[1]]
              MIs[[exp]][[label]] <- this.MIs
            }
            if(!is.null(results[[exp]][[j]][[n]]$TPs)) { # only read in results if they exist
              label <- ifelse(grepl(x=exp, pattern="TTR"), paste0("TTR", as.character(round(this.result$TTR, 2))), as.character(this.result$N.utts))
              this.TPs  <- results[[exp]][[j]][[n]]$TPs
              if(is.list(this.TPs)) this.TPs <- this.TPs[[1]]
              TPs[[exp]][[label]] <- this.TPs
            }
          }
        } # end n for loop
      } 
    } # end j for loop
  } # end e for loop
  # combine the stat.results from all experiments into one dataframe
  sim.results <- stat.results[[1]]
  for(e in 2:length(names(stat.results))){
    sim.results <- rbind(sim.results, stat.results[[e]])
  }
  library(tidyr); library(dplyr)
  sim.results <- sim.results %>%
    mutate(cutoff=85) %>%
    unite(criterion, stat, cutoff, sep="", remove=F) %>%
    extract(exp, into="dist", regex="([[:alnum:]]{4}$)", remove=FALSE) %>%
    gather(measure, value, recall:precision) %>%
    arrange(nontext)
  sim.results$dist <- tolower(sim.results$dist)
  
  return(list(sim.results=sim.results, MIs=MIs, TPs=TPs))
}

plot_context_vs_nontext <- function(context_results, nontext_results, global_results, outcome, annotate=NULL, xlabs=FALSE, save.to, ...){
  
  stopifnot(require(ggplot2), require(dplyr), require(tidyr))
  
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  facet <- length(levels(context_results[, colnames(context_results)==outcome])) > 1 # is there is more than one level for outcome?
  if(facet){
    colnames(context_results)[colnames(context_results)=="value"] <- "outcome"
    colnames(nontext_results)[colnames(nontext_results)=="value"] <- "outcome"
    if(!is.null(global_results)){
      colnames(global_results)[colnames(global_results)=="value"] <- "outcome"
    }
  } else {
    colnames(context_results)[colnames(context_results)==outcome] <- "outcome"
    colnames(nontext_results)[colnames(nontext_results)==outcome] <- "outcome"
    if(!is.null(global_results)){
      colnames(global_results)[colnames(global_results)==outcome] <- "outcome"
    }
  }
  
  if(!is.null(annotate)){
    colnames(context_results)[colnames(context_results)==annotate] <- "annotate"
  }
  
    
  colors <- c("#D53E4F", "#66C2A5", "#3288BD", "#F46D43")
  names(colors) <- unique(context_results$method.short)
  for(m in unique(context_results$method.short)){
    con.data <- filter(context_results, method.short==as.character(m))
    non.data <- filter(nontext_results, method.short==as.character(m))
    if(!is.null(annotate)){
      lab.data <- con.data %>% 
        select(method, method.short, context, outcome, annotate) %>% 
        unique()
    }
    
    p <- ggplot(non.data, aes(x=reorder(context, N.utts), y=outcome)) + 
      geom_boxplot() +
      geom_point(data=con.data, color=colors[[as.character(m)]], size=4, show.legend=FALSE) + 
      theme(text = element_text(size=30), axis.ticks = element_blank(), axis.text.x = element_blank()) +
      labs(x=NULL, y=NULL, title=paste(m, outcome)) 
    if(!is.null(global_results)) p <- p +  geom_hline(data=global_results, aes(yintercept=outcome), linetype = 2, size=1.5)
    if(facet) p <- p + facet_wrap(~measure)
    if(!is.null(annotate)) p <- p + geom_text(data=lab.data, aes(label=annotate,x=reorder(context, N.utts), y=outcome), size=4)
    if(xlabs) p <- p + theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0))
    ggsave(p, filename=paste0(save.to, "/", outcome, "_", m, "_", additional_args ,".png"), width=8, height=8, units="in")
  }
}


plot_seg_results <- function(seg.results, title=NULL, boxplot=TRUE, scatterplot=FALSE, by=c("syl", "contexts")){
  # add break by N.syl option (check whether 1, 2, + syllable words are getting segmented correctly)
  plot <- ggplot(seg.results, aes(x=seg.result, y=freq.segd)) 
  if(boxplot) plot <- plot + geom_boxplot()
  if(scatterplot & by=="syl") plot <- plot + geom_point(aes(color=syl.bins), alpha=.7, size=4, position = position_jitter(w = .3, h = 0)) 
  if(scatterplot & by=="contexts") plot <- plot + geom_point(aes(color=context), alpha=.7, size=4, position = position_jitter(w = .3, h = 0)) 
  plot <- plot + 
    scale_y_log10() + labs(y="Log10(frequency)", x=NULL, title=title) + 
    scale_x_discrete(limits=c("miss", "hit")) + 
    coord_flip() +
    theme(text = element_text(size=30))
  return(plot)
}

results_descriptives <- function(data, criterion=c("MI85", "TP85"), context="global"){
  N.syl <- data$streams$N.syl
  words.tokens <- length(data$streams$orth.stream)
  words.types <- length(unique(data$streams$orth.stream))
  N.utt <- data$N.utterances
  N.seed.words <- ifelse(context=="global", NA, length(grep(context, data$TP85$seg.results$context, fixed=TRUE)))
  
  if(criterion=="MI85") N.segd.units <- median(data$MI85$seg.results$N.segd.units)
  if(criterion=="TP85") N.segd.units <- median(data$TP85$seg.results$N.segd.units)
  
  descriptives <- data.frame(context=context,  
                             N.utt=N.utt,
                             N.word.tokens=words.tokens,
                             N.word.types=words.types,
                             N.syl=N.syl,
                             criterion=criterion,
                             N.segd.units=N.segd.units,
                             N.hits=NA,
                             N.misses=NA,
                             N.false.alarms=NA)
  descriptives$N.hits <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[2],
                                ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[2], NA))
  descriptives$N.misses <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[3],
                                  ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[3], NA))
  descriptives$N.false.alarms <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[1],
                                        ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[1], NA))
  
  descriptives <- dplyr::mutate(descriptives, 
                                seg.diff = N.word.tokens - N.segd.units, 
                                trend = ifelse(seg.diff > 0, "under", ifelse(seg.diff < 0, "over", NA)),
                                prop.diff = abs(seg.diff)/N.word.tokens)
  
  return(descriptives)
}

corpus_decriptives <- function(corpus, data, contexts, dict){
  ##############################
  # what's the frequency of each word, and is it a seed word?
  freqs.table <- sort(table(data$streams$orth.stream), decreasing = TRUE)
  freqs <- data.frame(orth=names(freqs.table), freq=freqs.table)
  freqs$orth <- as.character(freqs$orth)
  
  word.contexts <- gather(contexts, key=context, value=orth) %>%
    filter(grepl(pattern="[[:alpha:]]+", x=orth))
  
  freqs <- left_join(freqs, word.contexts, by="orth")
  freqs$freq <- as.numeric(freqs$freq)
  dict$word <- as.character(dict$word)
  dict <- dict %>%
    select(word, phon, N.syl)
  freqs <- left_join(freqs, dict, by=c("orth" = "word") ) %>%
    arrange(-freq)
  
  
  ##############################
  # how many syls per utterance?
  key <- corpus[ , 1:3]
  key$phon.stream <- gsub(pattern="-", x=key$phon, replacement=" ")
  
  temp <- key %>%
    separate(col=phon.stream, into=paste0("syl", 1:30), extra='merge', sep=" ")
  # clean out empty columns
  deleted <- 0
  for(c in seq(from=ncol(temp), to=1)){
    temp[[c]] <- gsub(pattern="^[[:space:]]*$", x=temp[[c]], replacement=NA)
    nas <- length(which(is.na(temp[ , c])))
    if(nas == nrow(temp)) temp[[c]] <- NULL ; deleted <- deleted + 1
  }
  if(deleted==0) stop("too many syllables per utterance to split it all!")
  
  syls.per.utt <- temp %>% 
    gather(key=key, value=value, starts_with("syl")) %>%
    na.omit() %>%
    count(utt) %>%
    rename(N.syls=n)
  
  key <- left_join(key, syls.per.utt, by="utt")
  
  syls.per.utt <- list()
  syls.per.utt$N.utts <- nrow(key)
  syls.per.utt$Mean.length.utt <- mean(key$N.syls)
  syls.per.utt$SD.length.utt <- sd(key$N.syls)
  syls.per.utt$N.syls.per.utt <- table(key$N.syls)
  syls.per.utt$Perc.syls.per.utt <- round(table(key$N.syls) / nrow(key), 3)
  
  syl.freqs <- freqs %>%
    group_by(N.syl) %>%
    summarize(freq.tot=sum(freq), freq.mean=mean(freq), freq.sd=sd(freq), N.types=n(), freq.se = freq.sd/sqrt(N.types))
  
  summary <- data.frame(freq1st.word = arrange(freqs, -freq)$orth[1],
                        freq2nd.word = arrange(freqs, -freq)$orth[2],
                        freq3rd.word = arrange(freqs, -freq)$orth[3],
                        freq1st.freq = arrange(freqs, -freq)$freq[[1]],
                        freq2nd.freq = arrange(freqs, -freq)$freq[[2]],
                        freq3rd.freq = arrange(freqs, -freq)$freq[[3]],
                        freq1syl.tot  = syl.freqs$freq.tot[[1]],
                        freq2syl.tot  = syl.freqs$freq.tot[[2]],
                        freq3syl.tot  = syl.freqs$freq.tot[[3]],
                        freq4syl.tot  = syl.freqs$freq.tot[[4]],
                        freq1syl.mean = syl.freqs$freq.mean[[1]],
                        freq2syl.mean = syl.freqs$freq.mean[[2]],
                        freq3syl.mean = syl.freqs$freq.mean[[3]],
                        freq4syl.mean = syl.freqs$freq.mean[[4]],
                        freq1syl.sd   = syl.freqs$freq.sd[[1]],
                        freq2syl.sd   = syl.freqs$freq.sd[[2]],
                        freq3syl.sd   = syl.freqs$freq.sd[[3]],
                        freq4syl.sd   = syl.freqs$freq.sd[[4]],
                        freq1syl.se   = syl.freqs$freq.se[1],
                        freq2syl.se   = syl.freqs$freq.se[2],
                        freq3syl.se   = syl.freqs$freq.se[3],
                        freq4syl.se   = syl.freqs$freq.se[4])
  
  return(list(word.freq=freqs, syls.per.utt=syls.per.utt, syl.summary=summary)) 
}

network_plot <- function(data=NULL, context=NULL, title=""){
  if(!is.null(data)){
    unique.phon.pairs <- data$unique.phon.pairs
    # need to give it a data frame containing a symbolic edge list in the first two columns. Additional columns are considered as edge attributes. 
    bsk.network<-graph.data.frame(unique.phon.pairs, directed=T) # edge list, with additional attributes
    bad.edges <- E(bsk.network)[E(bsk.network)$MI < quantile(unique.phon.pairs$MI, .85)] #identify those edges with MI below the cutoff for segmentation
    bsk.network<-delete.edges(bsk.network, bad.edges) #exclude them from the graph
    
    V(bsk.network)$size <- table(data$streams$phon.stream) # size nodes by frequency 
    message(paste("max freq", max(V(bsk.network)$size)))
    V(bsk.network)$size <- V(bsk.network)$size/(.03*max(V(bsk.network)$size)) # size nodes by frequency 
    bad.vs <- V(bsk.network)[V(bsk.network)$size < .5] #identify those nodes with freq below 5
    bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph
    
    # For example we can separate some vertices (people) by color:
    V(bsk.network)$color<-ifelse(V(bsk.network)$name=="'bIg", 'darkgreen', 'lightgrey') #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
    E(bsk.network)$width <- E(bsk.network)$MI/5
    E(bsk.network)$arrow.width <- .3
  } else if(!is.null(context)){
    # context <- votes
    context$utt <- 1:nrow(context)
    context <- as.tbl(context) %>% 
      gather(key, value, -utt) %>% 
      filter(value > 0) %>% 
      arrange(utt) %>% 
      group_by(utt) %>% 
      do({
        summarize(., words=paste(rep(key, value), collapse=";"))
      }) %>% 
      separate(col=words, into=paste0("word", 10:50), sep=";", extra ="drop") 
    
    # drop columns with all NAs
    drop <- NULL
    for(i in 1:ncol(context)){
      if(!any(!is.na(context[,i]))) {
        drop <- c(drop, i)
      }
    }
    context <- context[,-drop] # drop columns with all NAs
    
    t <- context %>% 
      ungroup() %>% 
      select(-utt) 
    edges <- data.frame(context1=NULL, context2=NULL)
    for(i in 1:(ncol(t)-1)){
      for(j in (i+1):ncol(t)){
        # print(j) # to check that the counting is working right :) 
        temp.edges <- t[, c(i, j)]
        colnames(temp.edges) <- c("context1", "context2")
        edges <- rbind(edges, temp.edges)
        head(t[, c(i, j)]); j=j+1
      }  
    } # end for loops
    # clean up
    edges$context1 <- gsub(x=edges$context1, pattern="NA", replacement=NA)
    edges$context2 <- gsub(x=edges$context2, pattern="NA", replacement=NA)
    edges <- na.omit(edges)
    
    library(igraph)
    bsk.network <- graph.data.frame(edges, directed=F)
  }
  
  plot(bsk.network, vertex.label=ifelse(V(bsk.network)$name=="'tV", "'tV", ""))
  return(plot.igraph(bsk.network, vertex.label="", main=title))
}

add_stars <- function(tests){
  # add stars for significance to a table with p values
  tests$stars <- ifelse(tests$p.val < .001, "***",
                  ifelse(tests$p.val < .01, "**",
                         ifelse(tests$p.val < .05, "*", 
                                ifelse(tests$p.val < .1, "+", ""))))
  return(tests)
}