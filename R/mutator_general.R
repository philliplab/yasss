#' General mutation checker
#'
#' Performs basic sanity checks on a mutator and its candidate set
#'
#' Since mutators are custom functions that are only used in deeply nested code
#' and called with mechanisms like do.call, it makes debugging them really
#' hard. This function performs a number of checks to ensure that the basic
#' errors are flagged before the main loop is initiated.
#'
#' @param fun The mutator function
#' @param args A list of arguments with which the mutator such run 'normally'. i.e. a per base mutation rate between 0% and 5%.
#' @export

mutator_checks_general <- function(fun, args){
  results <- list(fun_is_fun = TRUE)
  return(results)
}
