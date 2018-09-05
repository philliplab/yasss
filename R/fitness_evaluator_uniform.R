#' The uniform fitness_evaluator
#'
#' Assigns each sequences a fitness uniformly sampled from (0,1)
#'
#' @return A list with a single element called fitness.
#' @param the_seq All the sequences to which fitnesses must be assigned.
#' @export

fitness_evaluator_uniform_fun <- function(the_seq){
  stopifnot(class(the_seq) == "character")
  fitness_score <- runif(length(the_seq))
  return(list(fitness_score = fitness_score,
              the_seq = the_seq))
}
