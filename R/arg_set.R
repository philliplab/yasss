#' Checks an arg_set (skeleton only)
#'
#' @param arg_set The arg_set to test
#' @param required_fitness Should the required_fitness element be present in the arg_sets?
#' @param verbose Prints out result of each check
#' @export

check_arg_set <- function(arg_set, the_seq = NULL, required_fitness = TRUE, verbose = FALSE){
  result <- list()
  result[['is_list']] <- class(arg_set) == 'list'

  # ancestors
  result[['has_ancestors']] <- 'ancestors' %in% names(arg_set)
  if (result[['has_ancestors']]){
    result[['ancestors_are_character']] <- class(arg_set$ancestors) == 'character'
    if (result[['ancestors_are_character']]){
      result[['ancestors_same_length']] <- min(nchar(arg_set$ancestors)) == max(nchar(arg_set$ancestors))
    }
  }
  
  # label
  result[['has_label']] <- 'label' %in% names(arg_set)
  if (result[['has_label']]){
    result[['label_are_character']] <- class(arg_set$label) == 'character'
    result[['label_is_length_one']] <- length(arg_set$label) == 1
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
  if (!is.null(the_seq)){
    fe_result <- check_fitness_evaluator(the_seq, arg_set$fitness_evaluator$fun, arg_set$fitness_evaluator$args)
    for (i in 1:length(fe_result)){
      names(fe_result)[[i]] <- paste('fe', names(fe_result)[[i]], sep = '_')
    }
    result <- c(result, fe_result)
  }

  # required_fitness
  if (required_fitness){
    result[['has_required_fitness']] <- 'required_fitness' %in% names(arg_set)
    if (result[['has_required_fitness']]){
      result[['required_fitness_is_numeric']] <- class(arg_set$required_fitness) %in% c('numeric', 'integer')
      result[['required_fitness_is_length_one']] <- length(arg_set$required_fitness) == 1
      if (result[['required_fitness_is_numeric']] & result[['required_fitness_is_length_one']]){
        result[['required_fitness_between_zero_one']] <- arg_set$required_fitness >= 0 & arg_set$required_fitness <= 1
      }
    }
  }

  if ('ps_rate' %in% names(arg_set)){
    result[['ps_rate_numeric']] <- class(arg_set$ps_rate) == 'numeric'
    if (result[['ps_rate_numeric']]){
      result[['ps_rate_between_0_1']] <- arg_set$ps_rate >= 0 & arg_set$ps_rate < 1
    }
  }

  if (verbose){
    print(result)
  }

  return(result)
}

#' Checks an arg_collection (skeleton only)
#'
#' @param arg_collection The arg_collection to test
#' @param required_fitness Should the required_fitness element be present in the arg_sets?
#' @param verbose Prints out result of each check
#' @export

check_arg_collection <- function(arg_collection, required_fitness = TRUE, verbose = FALSE){
  result <- list()
  result[['is_list']] <- class(arg_collection) == 'list'

  all_arg_sets_valid <- TRUE
  set_labels <- NULL
  for (arg_set in arg_collection){
    c_arg_set_result <- check_arg_set(arg_set, required_fitness = required_fitness, verbose = verbose)
    all_arg_sets_valid <- all_arg_sets_valid & all(unlist(c_arg_set_result))
    set_labels <- c(set_labels, arg_set$label)
  }
  result[['all_arg_sets_valid']] <- all_arg_sets_valid 

  result[['unnamed']] <- is.null(names(arg_collection))

  result[['labels_unique']] <- length(set_labels) == length(unique(set_labels))

  return(result)
}
