searchK <- function(documents, vocab, K, init.type = "Spectral", 
                    N=floor(.1*length(documents)), proportion=.5, 
                    heldout.seed=NULL,
                    M=10, 
                    iter=1,...) {

  g <- expand.grid(K=K, iter=1:iter) %>% # get each value of K for each iteration
    group_by(iter, K) %>%
    do({
      message(paste0("\nprocessing iteration number", .$iter, "...\n"))
      #Make a heldout dataset
      heldout <- make.heldout(documents, vocab, N=N, proportion=proportion, 
                              seed=heldout.seed)
      #run stm
      model <- stm(documents=heldout$documents, vocab=heldout$vocab,
                   K=.$K, init.type=init.type,...)
      iter.g <- data.frame(K=.$K, iter=.$iter)
      #calculate values to return
      if( !"content" %in% names(list(...)) ) {  # only calculate exclusivity for models without content covariates
        iter.g$exclus <-mean(unlist(exclusivity(model, M=M, frexw=.7)))
        iter.g$semcoh <-mean(unlist(semanticCoherence(model, heldout$documents, M)))
      }
      iter.g$heldout <-eval.heldout(model, heldout$missing)$expected.heldout    
      iter.g$residual <-checkResiduals(model,heldout$documents)$dispersion
      iter.g$bound <-max(model$convergence$bound)
      iter.g$lbound <-max(model$convergence$bound) + lfactorial(model$settings$dim$K)
      iter.g$em.its <-length(model$convergence$bound)  
      return(iter.g)
    })

  if( "content" %in% names(list(...)) ) {
    warning("Exclusivity calculation only designed for models without content covariates", call.=FALSE)
  }
  
  toreturn <- list(results=g, call=match.call(expand.dots=TRUE))
  class(toreturn)<- "searchK"
  return(toreturn)
}