#' Simulates evolution of DNA of a population
#'
#' Produce a set of DNA sequences by simulation a population with multiple generations allowing mutation and recombination
#'
#' Details: Simulated multiple generations
#' @param ancestors A list of DNA sequences with which to start the population. Include the same sequence multiple times to achieve a target ratio.
#' @export 

sim_pop <- function(ancestors, n_gen = NULL, n_pop = NULL){
  if (is.null(n_gen)) {n_gen <- Inf}
  if (is.null(n_pop)) {n_pop <- Inf}

  if (n_gen == Inf & n_pop == Inf){
    stop('Either n_gen or n_pop must be specified and at least one must be finite')
  }
  c_gen <- 0
  c_pop <- length(ancestors)
  while (c_pop < n_pop & c_gen < n_gen){
    the_pop <- character(length(ancestors)*2)
    the_pop[1:length(ancestors)] <- ancestors
    the_pop[(length(ancestors)+1):(2*length(ancestors))] <- ancestors
    ancestors <- the_pop
  }
  return(ancestors)
}

