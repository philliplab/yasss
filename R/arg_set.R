#' Checks an arg_set (skeleton only)
#'
#' @export

check_arg_set <- function(arg_set){
  result <- list()
  result[['is_list']] <- class(arg_set) == 'list'
  return(result)
}

#' Checks an arg_collection (skeleton only)
#'
#' @export

check_arg_collection <- function(arg_collection){
  result <- list()
  result[['is_list']] <- class(arg_collection) == 'list'
  return(result)
}
