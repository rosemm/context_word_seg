#' Creates a boxplot showing bootstrapped null distirbutions with context estimates as colored dots.
#' 
#' Longer description and notes.
#' 
#' @author Rose Hartman
#' 
#' @param argument_name description of the argument
#' @param argument_name description of the argument
#' @param argument_name description of the argument
#' @param argument_name description of the argument
#' @param argument_name description of the argument
#' 
#' @return None.
#' 
#' @examples
#' plot_context_vs_nontext(context_all_freq, nontext_results_freq, global_results_freq, outcome="precision", xlabs=TRUE, save.to="plots/context_vs_nontext", consider.freq=TRUE)
#' 
#' @export
plot_context_vs_nontext <- function(context, nontext, global, outcome, Z.score=FALSE, methods.to.use=c("WL", "LDA", "STM", "HJ"), annotate=NULL, xlabs=FALSE, save.to, ...){
  
  additional_args <- as.data.frame(list(...))
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  # is there is more than one level for outcome (i.e. more than one measure to plot)?
  # if so, facet=TRUE
  facet <- length(levels(context[, colnames(context)==outcome])) > 1 
  if(facet){
    colnames(context)[colnames(context)=="value"] <- "outcome"
    colnames(nontext)[colnames(nontext)=="value"] <- "outcome"
    if(!is.null(global)){
      colnames(global)[colnames(global)=="value"] <- "outcome"
    }
  } else {
    colnames(context)[colnames(context)==outcome] <- "outcome"
    colnames(nontext)[colnames(nontext)==outcome] <- "outcome"
    if(!is.null(global)){
      colnames(global)[colnames(global)==outcome] <- "outcome"
    }
  }
  
  if(!is.null(annotate)){
    colnames(context)[colnames(context)==annotate] <- "annotate"
  }
  
  if(Z.score) {
    nontext <- nontext %>% 
      group_by(method, context)
    
    nontext.sum <- nontext %>% 
      summarize(nontext.mean = mean(outcome), 
                nontext.sd = sd(outcome))
    
    context <- context %>% 
      left_join(nontext.sum, by=c("method", "context")) %>% 
      mutate(outcome=(outcome - nontext.mean)/nontext.sd)
    
    nontext <- nontext %>%
      mutate(outcome=scale(outcome))
  }
  
  
  colors <- c("#D53E4F", "#67000d", "#006d2c", "#66C2A5", "#3288BD", "#F46D43")
  names(colors) <- unique(context$method.short)
  
  for(m in unique(context$method.short)){
    con.data <- dplyr::filter(context, method.short==as.character(m))
    non.data <- dplyr::filter(nontext, method.short==as.character(m))
    if(!is.null(annotate)){
      lab.data <- con.data %>% 
        dplyr::select(method, method.short, context, outcome, annotate) %>% 
        unique()
    }
    
    if(Z.score){
      p <- ggplot(non.data, aes(x=outcome)) + 
        geom_histogram() +
        geom_vline(data=con.data, aes(xintercept=outcome), color=colors[[as.character(m)]], size=4, show.legend=FALSE) + 
        theme(axis.text.y = element_blank())
    } else{
      p <- ggplot(non.data, aes(x=reorder(context, N.utts), y=outcome)) + 
        geom_boxplot() +
        geom_point(data=con.data, color=colors[[as.character(m)]], size=4, show.legend=FALSE) + 
        theme(axis.text.x = element_blank())
      if(xlabs) p <- p + theme(axis.text.x = element_text(angle=330, vjust=1, hjust=0))
      if(!is.null(global)) p <- p +  geom_hline(data=global, aes(yintercept=outcome), linetype = 2, size=1.5) 
    }
    
    p <- p + theme(text = element_text(size=30), axis.ticks = element_blank()) +
      labs(x=NULL, y=NULL, title=paste(m, outcome)) 
    if(facet) p <- p + facet_wrap(~measure)
    if(!is.null(annotate)) p <- p + geom_text(data=lab.data, aes(label=annotate,x=reorder(context, N.utts), y=outcome), size=4)
    
    
    print(p)
    
    ggsave(p, filename=paste0(save.to, "/", outcome, "_", m, "_", additional_args ,".png"), width=8, height=8, units="in")
  } # end for loop for method
}
