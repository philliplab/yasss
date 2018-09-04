#' Assign fitness to sequences
#'
#' Given a fitness evaluator, assign all the sequences in the last generation of a genealogy a fitness
#'
#' @return A genealogy
#' @param genealogy A genealogy
#' @param fitness_evaluator A list with two elements: fun is the character string naming a fitness evaluator and args is a list of arguments that fun needs.
#' @export

assign_fitness <- function(genealogy, fitness_evaluator){
  args <- fitness_evaluator$args
  args$the_seq <- genealogy$the_seq
  fit_fun <- get(fitness_evaluator$fun)
  x <- do.call(fit_fun, args)
  #TODO: remove this line if there are performance issues
  stopifnot(all(x$the_seq == genealogy$the_seq))
  genealogy$fitness <- x$fitness

  return(genealogy)
}

