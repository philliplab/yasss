#' The uniform mutator
#'
#' Mutates a sequence giving each base an equal chance to experience a random mutation
#'
#' @param patent A character string containing the DNA sequence of the parent
#' @param mu A single number between 0 and 1 specifying the per base mutation rate
#' @export

mutator_uniform_fun <- function(parent, mu){
  result <- list(parent = parent,
                 child = parent,
                 mutation_stats = list(n_mut = 0),
                 mu = mu)
  return(result)
}
