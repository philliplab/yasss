#' Assign fitness to sequences
#'
#' Given a fitness evaluator, assign all the sequences in the last generation of a genealogy a fitness
#'
#' @return A genealogy
#' @param genealogy A genealogy
#' @param fitness_evaluator A list with two elements: fun is the character string naming a fitness evaluator and args is a list of arguments that fun needs.
#' @export

assign_fitness <- function(genealogy, fitness_evaluator){

  last_generation <- genealogy %>% filter(gen_num == max(gen_num))
  all_non_last_generation <- genealogy %>% filter(gen_num != max(gen_num))

  args <- fitness_evaluator$args
  args$the_seq <- last_generation$the_seq
  fit_fun <- get(fitness_evaluator$fun)
  x <- do.call(fit_fun, args)
  #TODO: remove this line if there are performance issues
  stopifnot(all(x$the_seq == last_generation$the_seq))
  last_generation$fitness_score <- x$fitness

  genealogy <- rbind(all_non_last_generation, last_generation)

  return(genealogy)
}

#' General fitness_evaluator checker
#'
#' Performs basic sanity checks on a fitness_evaluator and its candidate argument set.
#'
#' Since fitness_evaluators are custom functions that are only used in deeply nested code
#' and called with mechanisms like do.call, it makes debugging them really
#' hard. This function performs a number of checks to ensure that the basic
#' errors are flagged before the main loop is initiated. This function is also used extensively in the testing of the fitness_evaluators.
#'
#' @param fun The fitness_evaluator function
#' @param args A single list of arguments for the fitness_evaluator that are reasonable. This will be used to call the evaluator on dummy data during the checks.
#' @export

check_fitness_evaluator <- function(fun, args){
  result <- list()
  result$fun_is_character <- class(fun) == "character"
  fun <- get(fun)
  result$fun_is_getable <- class(fun) == "function"
  return(result)
}


#mutator_checks_general <- function(fun, args){
#  result <- list()
#  result$fun_is_fun <- class(fun) == "function"
#  result$fun_has_parent_arg <- "parent" %in% names(formals(fun))
#  result$fun_has_mu_arg <- "mu" %in% names(formals(fun))
#
#  result$all_mu_greater_equal_0 <- all(args$mu >= 0)
#  result$all_mu_less_equal_1 <- all(args$mu <= 1)
#
#  parent <- 'ACGTAC'
#  tmp_args <- c(list(parent = parent), args)
#  x <- try(do.call(fun, tmp_args), silent = TRUE)
#
#  result$mutator_runs <- !('try-error' %in% class(x))
#
#  if (result$mutator_runs) {
#    result$output_is_list <- class(x) == 'list'
#    result$output_has_parent <- 'parent' %in% names(x)
#    result$output_has_child <- 'child' %in% names(x)
#    result$output_has_mutation_stats <- 'mutation_stats' %in% names(x)
#    result$mutation_stats_has_n_mut <- 'n_mut' %in% names(x$mutation_stats)
#    result$output_has_mu <- 'mu' %in% names(x)
#
#    #NOTE: Might have to update this to handle indels
#    # Because might want to use this to track insertion positions
#    result$output_parent_correct <- parent == x$parent
#    result$output_child_char <- class(x$child) == 'character'
#    result$output_one_child <- length(x$child) == 1
#  }
#
#  return(result)
#}
