#' @title Save plots showing boostrapped trajectories for resampled estimates
#' 
#' @description Saves plots that show n bootstrapped trajectories for estimates from a resampling procedure. 
#' Includes horizontal reference lines for the observed estimate (mean of all resampled estimates) and a corridor of stability +-tol from that estimate.
#' 
#' @param samples The vector of samples from the resampling procedure
#' @param n The number of lines (bootstrapped trajectories) to plot
#' @param save.to directory for saving plots. If NULL, no plots will be saved.
#' @param measure a character string describing the estimates. This will be included in the file names (if saving plots) and as the label on the y-axis of the plots. 
#' @param note a character string providing any additional information that should be saved in the file name
#' @param plot.mean produce a plot showing the bootstrapped trajectories for the mean?
#' @param plot.sig.cutoffs produce a plot showing the bootstrapped trajectories for the 97.5th and 2.5th percentiles (the cutoffs for significance at alpha=.05 for two-tailed tests)?
#' @param tol the half width of the desired stability corridor. To not plot the stability corridor, set tol=0.
#' 
#' @export
resampling_convergence <- function(samples, n=100, save.to=NULL, measure=NULL, note=NULL, plot.mean=TRUE, plot.sig.cutoffs=TRUE, tol=1){
  stopifnot(require(dplyr), require(tidyr), require(ggplot2))
  
  message("save to...", file.path(getwd(), save.to))
  
  # first just save a histogram of the distribution
  p1 <- ggplot(data.frame(samples), aes(x=samples))+
    geom_histogram(bins=50) + 
    theme_classic() + 
    labs(x=measure)
  if(!is.null(save.to)){
    message("saving...", paste(measure, note, "hist.png", sep="_"))
    ggsave(plot=p1, path=save.to, filename=paste(measure, note, "hist.png", sep="_"), width=8, height = 8, units = "in")
  }
  
  # take the vector of samples and make n copies by taking random samples from it ("case resampling")
  # each copy will contribute to one line on the graph
  samples_df <- replicate(n, base::sample(x=samples, size=length(samples), replace=TRUE)) %>% 
    as.data.frame() %>% 
    tidyr::gather("key", "value") 
  
  # calculate the actual mean, 97.5 and 02.5 cutoffs for the observed data
  # these values are used as horizontal reference lines in p2 and p3
  # tol is the acceptable tolerance, i.e. the half width of the corridor of stability
  # Schönbrodt, F. D., & Perugini, M. (2013). At what sample size do correlations stabilize?. Journal of Research in Personality, 47(5), 609-612.
  plot_references <- samples %>% 
    data.frame() %>% 
    summarize(mean=mean(.),
              P.975=quantile(x=., probs=.975),
              P.025=quantile(x=., probs=.025)) %>% 
    mutate(mean.hi  = mean + tol,  mean.lo  = mean - tol,
           P.975.hi = P.975 + tol, P.975.lo = P.975 - tol,
           P.025.hi = P.025 + tol, P.025.lo = P.025 - tol)
  
  if(plot.mean){
    samples_df <- samples_df %>% 
      dplyr::group_by(key) %>% 
      # get the running mean within each group (copy of the samples vector)
      # https://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html
      dplyr::mutate(mean = dplyr::cummean(value),
                    iteration = 1:n())
    p2 <- ggplot(samples_df, aes(x=iteration, y=mean, group=key)) + 
      geom_line(alpha=.3) + 
      geom_hline(data=plot_references, aes(yintercept=mean), color="black") + 
      geom_hline(data=plot_references, aes(yintercept=mean.hi), color="black", linetype=2) + 
      geom_hline(data=plot_references, aes(yintercept=mean.lo), color="black", linetype=2) +
      theme_classic() + 
      labs(y = measure) 
    if(!is.null(save.to)){
      message("saving...", paste(measure, note, "mean_lines.png", sep="_"))
      ggsave(plot=p2, path=save.to, filename=paste(measure, note, "mean_lines.png", sep="_"), width=8, height = 8, units = "in")
    }
    
  } # end if statement
  
  if(plot.sig.cutoffs){
    samples_df <- samples_df %>% 
      dplyr::group_by(key) %>% 
      mutate(iteration=1:n(), P.975=NA, P.025=NA) %>%
      do({
        for(i in 39:nrow(.)){
          # start counting at 39 since it's not possible to get an exact two-tailed test at alpha=.05 for fewer than 39 iterations
          # Dufour, J. M., & Kiviet, J. F. (1998). Exact inference methods for first-order autoregressive distributed lag models. Econometrica, 79-104.
          .$P.975[i] <- quantile(x=.$value[1:i], probs=.975)
          .$P.025[i]  <- quantile(x=.$value[1:i], probs=.025) 
        }
        return(.)
      })
    
    p3_df <- samples_df %>% 
      tidyr::gather(key="sig", value="sig.value", P.975:P.025) %>% 
      tidyr::unite(key_sig, key, sig, remove=FALSE) 
    
    p3 <- ggplot(p3_df, aes(x=iteration, y=sig.value, group=key_sig, color=sig)) + 
      geom_line(alpha=.3) +
      geom_hline(data=plot_references, aes(yintercept=P.975), color="black") + 
      geom_hline(data=plot_references, aes(yintercept=P.975.hi), color="black", linetype=2) + 
      geom_hline(data=plot_references, aes(yintercept=P.975.lo), color="black", linetype=2) + 
      geom_hline(data=plot_references, aes(yintercept=P.025), color="black") + 
      geom_hline(data=plot_references, aes(yintercept=P.025.hi), color="black", linetype=2) + 
      geom_hline(data=plot_references, aes(yintercept=P.025.lo), color="black", linetype=2) + 
      theme_classic() + 
      labs(y = measure) 
    if(!is.null(save.to)){
      message("saving...", paste(measure, note, "sig_lines.png", sep="_"))
      ggsave(plot=p3, path=save.to, filename=paste(measure, note, "sig_lines.png", sep="_"), width=8, height = 8, units = "in")
    }
  } # end if statement
  return(samples_df)
}
