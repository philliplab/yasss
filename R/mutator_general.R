#' General mutation checker
#'
#' Performs basic sanity checks on a mutator and its candidate argument set.
#'
#' Since mutators are custom functions that are only used in deeply nested code
#' and called with mechanisms like do.call, it makes debugging them really
#' hard. This function performs a number of checks to ensure that the basic
#' errors are flagged before the main loop is initiated.
#'
#' @param fun The mutator function
#' @param args A list of arguments with which the mutator such run 'normally'.
#' i.e. a per base mutation rate between 0% and 5%. This will be used to call
#' the mutator on dummy data during the checks.
#' @export

mutator_checks_general <- function(fun, args){
  result <- list()
  result$fun_is_character <- class(fun) == "character"
  fun <- get(fun)
  result$fun_is_getable <- class(fun) == "function"
  result$fun_has_parent_arg <- "parent" %in% names(formals(fun))
  result$fun_has_mu_arg <- "mu" %in% names(formals(fun))

  result$all_mu_greater_equal_0 <- all(args$mu >= 0)
  result$all_mu_less_equal_1 <- all(args$mu <= 1)

  parent <- 'ACGTAC'
  tmp_args <- c(list(parent = parent), args)
  x <- try(do.call(fun, tmp_args), silent = TRUE)

  result$mutator_runs <- !('try-error' %in% class(x))

  if (result$mutator_runs) {
    result$output_is_list <- class(x) == 'list'
    result$output_has_parent <- 'parent' %in% names(x)
    result$output_has_child <- 'child' %in% names(x)
    result$output_has_mutation_stats <- 'mutation_stats' %in% names(x)
    result$mutation_stats_has_n_mut <- 'n_mut' %in% names(x$mutation_stats)
    result$output_has_mu <- 'mu' %in% names(x)

    #NOTE: Might have to update this to handle indels
    # Because might want to use this to track insertion positions
    result$output_parent_correct <- parent == x$parent
    result$output_child_char <- class(x$child) == 'character'
    result$output_one_child <- length(x$child) == 1
  }

  return(result)
}
