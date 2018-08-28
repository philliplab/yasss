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
  fun_is_fun <- class(fun) == "function"
  fun_has_parent_arg <- "parent" %in% names(formals(fun))
  fun_has_mu_arg <- "mu" %in% names(formals(fun))
  
  parent <- 'ACGTAC'
  tmp_args <- c(list(parent = parent), args)
  x <- do.call(fun, tmp_args)

  output_is_list <- class(x) == 'list'

  results <- list(fun_is_fun = fun_is_fun,
                  fun_has_parent_arg = fun_has_parent_arg,
                  fun_has_mu_arg = fun_has_mu_arg,
                  output_is_list = output_is_list)
  return(results)
}
