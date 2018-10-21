#' Checks an arg_set (skeleton only)
#'
#' @export

check_arg_set <- function(arg_set, the_seq = NULL){
  result <- list()
  result[['is_list']] <- class(arg_set) == 'list'

  # ancestor
  result[['has_ancestors']] <- 'ancestors' %in% names(arg_set)
  if (result[['has_ancestors']]){
    result[['ancestors_are_character']] <- class(arg_set$ancestors) == 'character'
    if (result[['ancestors_are_character']]){
      result[['ancestors_same_length']] <- min(nchar(arg_set$ancestors)) == max(nchar(arg_set$ancestors))
    }
  }

  # r0
  result[['has_r0']] <- 'r0' %in% names(arg_set)
  if (result[['has_r0']]){
    result[['r0_is_numeric']] <- class(arg_set$r0) %in% c('numeric', 'integer')
    result[['r0_is_length_one']] <- length(arg_set$r0) == 1
    if (result[['r0_is_numeric']] & result[['r0_is_length_one']]){
      result[['r0_is_positive_integer']] <- floor(arg_set$r0) == ceiling(arg_set$r0) & arg_set$r0 > 0
    }
  }

  # n_gen
  result[['has_n_gen']] <- 'n_gen' %in% names(arg_set)
  if (result[['has_n_gen']]){
    result[['n_gen_is_numeric']] <- class(arg_set$n_gen) %in% c('numeric', 'integer')
    result[['n_gen_is_length_one']] <- length(arg_set$n_gen) == 1
    if (result[['n_gen_is_numeric']] & result[['n_gen_is_length_one']]){
      result[['n_gen_is_positive_integer']] <- floor(arg_set$n_gen) == ceiling(arg_set$n_gen) & arg_set$n_gen > 0
    }
  }

  # n_pop
  result[['has_n_pop']] <- 'n_pop' %in% names(arg_set)
  if (result[['has_n_pop']]){
    result[['n_pop_is_numeric']] <- class(arg_set$n_pop) %in% c('numeric', 'integer')
    result[['n_pop_is_length_one']] <- length(arg_set$n_pop) == 1
    if (result[['n_pop_is_numeric']] & result[['n_pop_is_length_one']]){
      result[['n_pop_is_positive_integer']] <- floor(arg_set$n_pop) == ceiling(arg_set$n_pop) & arg_set$n_pop > 0
    }
  }

  # mutator
  mutator_result <- mutator_checks_general(arg_set$mutator$fun, arg_set$mutator$args)
  for (i in 1:length(mutator_result)){
    names(mutator_result)[[i]] <- paste('mutator', names(mutator_result)[[i]], sep = '_')
  }
  result <- c(result, mutator_result)

  # fitness_evaluator
  fe_result <- check_fitness_evaluator(the_seq, arg_set$fitness_evaluator$fun, arg_set$fitness_evaluator$args)
  for (i in 1:length(fe_result)){
    names(fe_result)[[i]] <- paste('fe', names(fe_result)[[i]], sep = '_')
  }
  if (!is.null(the_seq)){
    result <- c(result, fe_result)
  }
#check_fitness_evaluator <- function(the_seq, fun, args){
#  result <- list()
#  result$fun_is_character <- class(fun) == "character"
#  fun <- get(fun)
#  result$fun_is_getable <- class(fun) == "function"
#  result$has_the_seq_arg <- "the_seq" %in% names(formals(fun))
#
##  the_seq <- c('AAAAAA', 'CCCCCC', 'GGGGGG')
#  tmp_args <- args
#  tmp_args$the_seq <- the_seq
#  x <- try(do.call(fun, tmp_args), silent = TRUE)
#  result$fitness_evaluator_runs <- !('try-error' %in% class(x))
#
#  if (result$fitness_evaluator_runs){
#    specific_result <- check_fitness_evaluator_result(x, tmp_args)
#    result <- c(result, specific_result)
#  }
#
#  return(result)
#}

  return(result)
}
#  r0 <- tryCatch(round(as.numeric(r0), 0),
#                       warning=function(w) return(list(round(as.numeric(r0), 0), w))
#                       )
#  if (class(r0) == 'list') {
#    stop(YASSS_ERR_MSG[['GEN_SIZE_VALID']])
#  } else if (r0 < 1 | r0 > 1e6){
#    stop(YASSS_ERR_MSG[['GEN_SIZE_VALID']])
#  }
#
#  if (is.null(n_gen)) {n_gen <- Inf}
#  if (is.null(n_pop)) {n_pop <- Inf}
#
#  if (n_gen == Inf & n_pop == Inf){
#    stop(YASSS_ERR_MSG[['N_GEN_N_POP_INF_NULL']])
#  }
#  if (n_gen < 1 | n_pop < 1){
#    stop(YASSS_ERR_MSG[['N_GEN_N_POP_LESS_ONE']])
#  }
#  if (any(grepl('X', ancestors))){
#    stop(YASSS_ERR_MSG[['NO_X_IN_ANCESTOR']])
#  }


#' Checks an arg_collection (skeleton only)
#'
#' @export

check_arg_collection <- function(arg_collection){
  result <- list()
  result[['is_list']] <- class(arg_collection) == 'list'
  return(result)
}
