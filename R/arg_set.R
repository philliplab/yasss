#' Checks an arg_set (skeleton only)
#'
#' @export

check_arg_set <- function(arg_set){
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

  # n_pop

  # mutator

  # fitness_evaluator

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
