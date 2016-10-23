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
plot_context_vs_nontext <- function(results, outcome, xlabs=FALSE, Z.score=FALSE, methods_wrap=FALSE, save.to, ...){
  additional_args <- as.data.frame(list(...))
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  if(Z.score) additional_args <- paste0(additional_args, "Z")
  
  # computational modeling results will have model column, but descriptives will not
  # add a dummy model column if it's missing
  if(!"model" %in% colnames(results)) results$model <- "descriptives"
  
  # just use the current outcome(s)
  o.data <- results %>% 
    filter(measure %in% outcome)
  
  # is there is more than one level for outcome (i.e. more than one measure to plot)?
  # if so, facet=TRUE
  facet <- length(outcome) > 1 
  
  if(!facet) {
    results <- dplyr::filter(o.data, measure %in% outcome)
  }
  
  for(c in as.character(unique(o.data$model))){
    
    c.data <- o.data %>% 
      dplyr::filter(model == c)
    
    methods <- as.character(unique(c.data$method))
    colors <- c("#984ea3", "#377eb8", "#e41a1c", "#D53E4F", "#67000d", "#006d2c", "#66C2A5", "#3288BD", "#F46D43")
    names(colors) <- methods
  
    for(m in 1:length(methods)){
      
      if(methods_wrap){
        # use facet_wrap to display all methods in one plot?
        plot.data <- c.data
      } else {
        plot.data <- c.data %>% 
          dplyr::filter(method == methods[m])
      }
      
      if(Z.score){
        plot.data <- plot.data %>% 
          dplyr::select(-value, -context_est) %>% 
          dplyr::rename(value=Z_value, context_est=Z_est)
      }
        
      if(Z.score){
        p <- ggplot(plot.data, aes(x=value)) + 
          geom_histogram(bins=30, fill=gray(.9), color="black") +
          geom_vline(aes(xintercept=context_est, color=method), size=2, show.legend=FALSE)
      } else {
        p <- ggplot(plot.data, aes(x=context, y=value)) + 
          geom_boxplot(fill=gray(.9)) +
          geom_point(aes(y=context_est, color=method), size=4, show.legend=FALSE)
      }
       
      p <- p  + 
        theme_classic() + 
        scale_color_manual(values=colors) + 
        theme(text = element_text(size=30), axis.ticks = element_blank()) + 
        labs(x=NULL, y=NULL)
      
      if(xlabs & !Z.score) {
        p <- p + theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))
      } else 
      
      if(facet) {
        if(methods_wrap){
          p <- p + facet_grid(method ~ measure)
        } else {
          p <- p + facet_wrap(~ measure)
        }
      } else if(methods_wrap){
        p <- p + facet_wrap(~ method)
      }  
      
      if(methods_wrap){
        this.method <- ""
        width <- 12
        height <- 6
      } else {
        this.method <- methods[m]
        width <- 8
        height <- 8
      }
      
      p <- p + ggtitle(paste(c, "\n", this.method, outcome))
      
      # remove extra periods from the file name as it interfers with LaTex's ability to identify the file extension
      outcome <- gsub(x=outcome, pattern=".", replacement = "", fixed=TRUE)
      
      print(p)
      ggsave(p, filename=paste0(save.to, "/", c, "_", outcome, "_", this.method, "_", additional_args ,".png"), width=width, height=height, units="in")
      
      if(methods_wrap) m <- length(methods) # jump to the last iteration of the for loop
    }
  }
}
