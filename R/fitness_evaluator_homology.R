#' The homology based fitness_evaluator
#'
#' Assigns each sequences a fitness based on the homology to provided sequences
#'
#' @return A list with a single element called fitness.
#' @param the_seq All the sequences to which fitnesses must be assigned.
#' @param comparators The sequences to which the input sequences must be compared
#' @param h2fs A function that remaps the homology results between the input sequences and the comparators to a fitness score.
#' @export

fitness_evaluator_homology_fun <- function(the_seq, comparators, h2fs){
  stopifnot(class(the_seq) == "character")
  fitness_score <- runif(length(the_seq))
  return(list(fitness_score = fitness_score,
              the_seq = the_seq))
}
