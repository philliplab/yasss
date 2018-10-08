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

check_fitness_evaluator <- function(the_seq, fun, args){
  result <- list()
  result$fun_is_character <- class(fun) == "character"
  fun <- get(fun)
  result$fun_is_getable <- class(fun) == "function"
  result$has_the_seq_arg <- "the_seq" %in% names(formals(fun))

#  the_seq <- c('AAAAAA', 'CCCCCC', 'GGGGGG')
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
#' @param implementation Which implementation to use. 'df' for data.frame. 'Rvec' for R based vectors.
#' @export

get_fit_offspring <- function(genealogy, fitness_score, implementation = 'df'){
  if (implementation == 'df'){
    gfo_internal_Rdf(genealogy, fitness_score)
  } else if (implementation == 'Rvec') {
    gfo_internal_Rvec(genealogy, fitness_score)
  }
}

#' Internal function implementing get_fit_offspring using data.frames

gfo_internal_Rdf <- function(genealogy, fitness_score){
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
  if (total_fit > 0){
    return(f_genealogy[1:total_fit,])
  } else {
    return(f_genealogy[0,])
  }
}

#' Internal function implementing get_fit_offspring using vectors

gfo_internal_Rvec <- function(genealogy, fitness_score){

  if (FALSE){
    names(c_genea)
    genealogy <- c_genea
    x <- gfo_internal_Rvec(c_genea, 0.985)
  }

  genealogy <- genealogy %>% arrange(gen_num, id)

  i_gen_num         <- genealogy$gen_num
  i_id              <- genealogy$id
  i_parent_id       <- genealogy$parent_id
  i_the_seq         <- genealogy$the_seq
  i_n_mut           <- genealogy$n_mut
  i_recomb_pos      <- genealogy$recomb_pos
  i_recomb_replaced <- genealogy$recomb_replaced
  i_recomb_partner  <- genealogy$recomb_partner
  i_recomb_muts     <- genealogy$recomb_muts
  i_fitness_score   <- genealogy$fitness_score

  genealogy_size <- length(i_gen_num)

  o_gen_num         <- numeric(genealogy_size)
  o_id              <- numeric(genealogy_size)
  o_parent_id       <- numeric(genealogy_size)
  o_the_seq         <- character(genealogy_size)
  o_n_mut           <- numeric(genealogy_size)
  o_recomb_pos      <- numeric(genealogy_size)
  o_recomb_replaced <- character(genealogy_size)
  o_recomb_partner  <- numeric(genealogy_size)
  o_recomb_muts     <- numeric(genealogy_size)
  o_fitness_score   <- numeric(genealogy_size)

  c_gen_num <- 0
  total_fit <- 0
  fit_parents <- NULL
  fit_gen <- NULL

  i <- 2
  for (i in 1:genealogy_size){
    if (i_gen_num[i] == 0){
      if (i_fitness_score[i] >= fitness_score){
        total_fit <- total_fit + 1
        o_gen_num[total_fit]         <- i_gen_num[i]
        o_id[total_fit]              <- i_id[i]
        o_parent_id[total_fit]       <- i_parent_id[i]
        o_the_seq[total_fit]         <- i_the_seq[i]
        o_n_mut[total_fit]           <- i_n_mut[i]
        o_recomb_pos[total_fit]      <- i_recomb_pos[i]
        o_recomb_replaced[total_fit] <- i_recomb_replaced[i]
        o_recomb_partner[total_fit]  <- i_recomb_partner[i]
        o_recomb_muts[total_fit]     <- i_recomb_muts[i]
        o_fitness_score[total_fit]   <- i_fitness_score[i]
        fit_gen <- c(fit_gen, i_id[i])
        next
      }
    }
    if (i_gen_num[i] != c_gen_num){
      c_gen_num <- c_gen_num + 1
      stopifnot(i_gen_num[i] == c_gen_num)
      fit_parents <- fit_gen
      fit_gen <- NULL
    }
    if (i_fitness_score[i] >= fitness_score){
      if (i_parent_id[i] %in% fit_parents){
        total_fit <- total_fit + 1
        o_gen_num[total_fit]         <- i_gen_num[i]
        o_id[total_fit]              <- i_id[i]
        o_parent_id[total_fit]       <- i_parent_id[i]
        o_the_seq[total_fit]         <- i_the_seq[i]
        o_n_mut[total_fit]           <- i_n_mut[i]
        o_recomb_pos[total_fit]      <- i_recomb_pos[i]
        o_recomb_replaced[total_fit] <- i_recomb_replaced[i]
        o_recomb_partner[total_fit]  <- i_recomb_partner[i]
        o_recomb_muts[total_fit]     <- i_recomb_muts[i]
        o_fitness_score[total_fit]   <- i_fitness_score[i]
        fit_gen <- c(fit_gen, i_id[i])
        next
      }    
    }
  }

  o_gen_num         <- o_gen_num[1:total_fit]
  o_id              <- o_id[1:total_fit]
  o_parent_id       <- o_parent_id[1:total_fit]
  o_the_seq         <- o_the_seq[1:total_fit]
  o_n_mut           <- o_n_mut[1:total_fit]
  o_recomb_pos      <- o_recomb_pos[1:total_fit]
  o_recomb_replaced <- o_recomb_replaced[1:total_fit]
  o_recomb_partner  <- o_recomb_partner[1:total_fit]
  o_recomb_muts     <- o_recomb_muts[1:total_fit]
  o_fitness_score   <- o_fitness_score[1:total_fit]

  if (total_fit > 0){
    return(data.frame(gen_num         = o_gen_num,
                      id              = o_id,
                      parent_id       = o_parent_id,
                      the_seq         = o_the_seq,
                      n_mut           = o_n_mut,
                      recomb_pos      = o_recomb_pos,
                      recomb_replaced = o_recomb_replaced,
                      recomb_partner  = o_recomb_partner,
                      recomb_muts     = o_recomb_muts,
                      fitness_score   = o_fitness_score,
                      stringsAsFactors = FALSE
                      )
    )
  } else {
    return(genealogy[0,])
  }
}
