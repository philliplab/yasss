#' Checks that a list tracking the fitness_processing_metrics is valid
#'
#' @param fitness_processing_metrics The fitness_processing_metrics list to check
#' @export

check_fitness_processing_metrics <- function(fitness_processing_metrics, verbose = FALSE){
  result <- list()
  c_fpm <- fitness_processing_metrics
  result[['is_list']] <- class(c_fpm) == 'list'

  # sim_id
  result[['has_sim_id']] <- 'sim_id' %in% names(c_fpm)
  result[['sim_id_length_one']] <- length(c_fpm$sim_id) == 1

  result[['sim_id_integer']] <- class(c_fpm$sim_id) %in% c('numeric', 'integer')
  if (result[['sim_id_integer']] & result[['sim_id_length_one']]){
    result[['sim_id_integer']] <- floor(c_fpm$sim_id) == ceiling(c_fpm$sim_id)
  } else {
    result[['sim_id_integer']] <- FALSE
  }
  
  # label
  result[['has_label']] <- 'label' %in% names(c_fpm)
  result[['label_length_one']] <- length(c_fpm$label) == 1

  # sampling
  result[['has_sampling']] <- 'sampling' %in% names(c_fpm)
  result[['sampling_length_one']] <- length(c_fpm$sampling) == 1
  result[['sampling_valid']] <- c_fpm$sampling %in% c('fit_threshold', 'size_matched_sampling', 'none')

  # input_seqs
  result[['has_input_seqs']] <- 'input_seqs' %in% names(c_fpm)
  result[['input_seqs_length_one']] <- length(c_fpm$input_seqs) == 1

  result[['input_seqs_integer']] <- class(c_fpm$input_seqs) %in% c('numeric', 'integer')
  if (result[['input_seqs_integer']] & result[['input_seqs_length_one']]){
    result[['input_seqs_integer']] <- floor(c_fpm$input_seqs) == ceiling(c_fpm$input_seqs)
  } else {
    result[['input_seqs_integer']] <- FALSE
  }

  # output_seqs
  result[['has_output_seqs']] <- 'output_seqs' %in% names(c_fpm)
  result[['output_seqs_length_one']] <- length(c_fpm$output_seqs) == 1

  result[['output_seqs_integer']] <- class(c_fpm$output_seqs) %in% c('numeric', 'integer')
  if (result[['output_seqs_integer']] & result[['output_seqs_length_one']]){
    result[['output_seqs_integer']] <- floor(c_fpm$output_seqs) == ceiling(c_fpm$output_seqs)
  } else {
    result[['output_seqs_integer']] <- FALSE
  }

  if (verbose){
    print(result)
  }

  return(result)
}
