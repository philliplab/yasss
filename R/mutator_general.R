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

  all_mu_greater_equal_0 <- all(args$mu >= 0)
  all_mu_less_equal_1 <- all(args$mu <= 1)

  parent <- 'ACGTAC'
  tmp_args <- c(list(parent = parent), args)
  x <- do.call(fun, tmp_args)

  output_is_list <- class(x) == 'list'
  output_has_parent <- 'parent' %in% names(x)
  output_has_child <- 'child' %in% names(x)
  output_has_mutation_stats <- 'mutation_stats' %in% names(x)
  output_has_mu <- 'mu' %in% names(x)

  #NOTE: Might have to update this to handle indels
  # Because might want to use this to track insertion positions
  output_parent_correct <- parent == x$parent
  output_child_char <- class(x$child) == 'character'
  output_one_child <- length(x$child) == 1

  result <- list(fun_is_fun = fun_is_fun,
                 fun_has_parent_arg = fun_has_parent_arg,
                 fun_has_mu_arg = fun_has_mu_arg,
                 output_is_list = output_is_list)
  return(result)
}
