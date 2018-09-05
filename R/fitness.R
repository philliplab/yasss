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
  last_generation$fitness_score <- x$fitness_score

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
  result$has_the_seq_arg <- "the_seq" %in% names(formals(fun))

  the_seq <- c('AAAAAA', 'CCCCCC', 'GGGGGG')
  tmp_args <- args
  tmp_args$the_seq <- the_seq
  x <- try(do.call(fun, tmp_args), silent = TRUE)
  result$fitness_evaluator_runs <- !('try-error' %in% class(x))

  if (result$fitness_evaluator_runs){
    specific_result <- check_fitness_evaluator_result(x, tmp_args)
    result <- c(result, specific_result)
  }

  return(result)
}

#' General checks for the result of a fitness evaluator
#'
#' General checks that should be satisfied by all fitness_evaluators. No checks specific to the mechanism by which the fitness_score was computed.
#'
#' @return A list with a series of boolean elements indicating whether a specific check passed or failed.
#' @param fitness_evaluation The result returned by a fitness_evaluator
#' @param input_args The arguments that were used to all the fitness_evaluator

check_fitness_evaluator_result <- function(fitness_evaluation, input_args){

  result <- list()
  result$output_is_list <- class(fitness_evaluation) == 'list'
  result$has_the_seq <- "the_seq" %in% names(fitness_evaluation)
  result$has_fitness_score <- "fitness_score" %in% names(fitness_evaluation)
  result$the_seq_length <- length(fitness_evaluation$the_seq) == length(input_args$the_seq)
  result$the_seq_order <- all(fitness_evaluation$the_seq == input_args$the_seq)
  result$fitness_score_numeric <- class(fitness_evaluation$fitness_score) == "numeric"
  result$fitness_score_non_na <- (!any(is.na(fitness_evaluation$fitness_score))) &
                                 (!any(is.nan(fitness_evaluation$fitness_score)))

  return(result)
}

#' Retrieves the fit offspring with fit ancestry
#'
#' Selects all the individuals from the last generation in a genealogy that are themselves fit and whose entire ancestry was also fit.
#'
#' @param genealogy The genealogy from which the fit individuals must be selected
#' @param fitness_score The minimun fitness score an individual must have to have survived.
#' @export

get_fit_offspring <- function(genealogy, fitness_score){
  f_genealogy <- as.data.frame(matrix(nrow = nrow(genealogy), ncol = length(names(genealogy))))
  names(f_genealogy) <- names(genealogy)
  
  for (i in names(f_genealogy)){
    class(f_genealogy[,i]) <- class(genealogy[,i])
  }
  total_fit <- 0
  row_tracker <- 0
  fit_parents <- NULL

  for (c_gen_num in sort(unique(genealogy$gen_num))){
    next_fit_parents <- NULL
    c_genea <- genealogy %>% filter(gen_num == c_gen_num)
    for (c_id in c_genea$id){
      is_fit <- FALSE
      row_tracker <- row_tracker + 1
      c_indiv <- c_genea %>% filter(id == c_id)
      if (c_indiv$fitness_score > fitness_score){
        if (is.na(c_indiv$parent_id)){
          is_fit <- TRUE
        } else {
          is_fit <- c_indiv$parent_id %in% fit_parents
        }
      }
      if (is_fit){
        total_fit <- total_fit + 1
        f_genealogy[total_fit,] <- genealogy[row_tracker,]
        next_fit_parents <- c(next_fit_parents, c_indiv$id)
      }
    }
    fit_parents <- next_fit_parents
  }
  return(f_genealogy[1:total_fit,])
}
