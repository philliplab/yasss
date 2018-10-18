#' Check a dsum list
#'
#' Checks that a dsum list adheres to the specifications
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param dsum The dsum to check
#' @param identifiers If TRUE, then the dsum will be checked to ensure that
#' the identifier elements are also present.
#' @export

check_dsum <- function(dsum, identifiers = FALSE){
  result <- list()
  result[['is_a_list']] <- class(dsum) == 'list'
  col_names <- c('avg_hd', 'perc', 'dens')
  identifier_col_names <- c('sim_id', 'label', 'sampling')
  for (col_name in col_names){
    result_label <- paste(col_name, 'exists', sep = ' ')
    result[[result_label]] <- col_name %in% names(dsum)
  }
  only_valid_columns <- TRUE
  for (element_name in names(dsum)){
    only_valid_columns <- only_valid_columns & element_name %in% c(col_names, identifier_col_names)
  }
  result[['only_valid_columns']] <- only_valid_columns
  return(result)
}



