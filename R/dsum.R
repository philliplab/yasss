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
    result_label <- paste(col_name, 'exists', sep = '_')
    result[[result_label]] <- col_name %in% names(dsum)
  }

  if (identifiers){
    for (col_name in identifier_col_names){
      result_label <- paste(col_name, 'exists', sep = '_')
      result[[result_label]] <- col_name %in% names(dsum)
    }

    # sim_id
    result[['sim_id_length_one']] <- length(dsum$sim_id) == 1

    result[['sim_id_integer']] <- class(dsum$sim_id) %in% c('numeric', 'integer')
    if (result[['sim_id_integer']] & result[['sim_id_length_one']]){
      result[['sim_id_integer']] <- floor(dsum$sim_id) == ceiling(dsum$sim_id)
    } else {
      result[['sim_id_integer']] <- FALSE
    }
  
    # label
    result[['label_length_one']] <- length(dsum$label) == 1

    # sampling

  }

  # avg_hd

  # perc

  # density

  only_valid_columns <- TRUE
  for (element_name in names(dsum)){
    only_valid_columns <- only_valid_columns & element_name %in% c(col_names, identifier_col_names)
  }
  result[['only_valid_columns']] <- only_valid_columns



  return(result)
}



