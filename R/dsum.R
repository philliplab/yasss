#' Check a dsum list
#'
#' Checks that a dsum list adheres to the specifications
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param dsum The dsum to check
#' previous result will be drawn to check the prerequisites.
#' @export

check_dsum <- function(dsum){
  result <- list()
  result[['is_a_list']] <- class(dsum) == 'list'
  return(result)
}
    
