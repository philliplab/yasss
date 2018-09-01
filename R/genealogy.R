#' Constructs a genealogy
#'
#' Initializes a new genealogy from the ancestor sequences.
#'
#' @param ancestors Character vector containing the sequences of the ancestors of the genealogy.
#' @export

make_genealogy <- function(ancestors){
  genealogy <- data.frame(gen_num = 0,
                          id = 1:length(ancestors),
                          parent_id = -1,
                          the_seq = ancestors,
                          n_mut = NA_real_,
                          recomb_pos = NA_real_,
                          recomb_replaced = NA_character_,
                          recomb_partner = NA_real_,
                          recomb_muts = NA_real_,
                          fitness_score = NA_real_,
                          stringsAsFactors = FALSE
                          )
  return(genealogy)
}

#' Check genealogy
#'
#' Checks a genealogy for correctness by ensuring that all the columns are present and that they do not violate any of the structural requirements (like referencing a parent that does not exist)
#'
#' @return A list with TRUE or FALSE indicating whether the related check passed.
#'
#' @param genealogy The genealogy data.frame that must be checked.
#' @export

check_genealogy <- function(genealogy){
  results <- list()
  results$is_data.frame <- class(genealogy) == 'data.frame'
  results$has_gen_num <- 'gen_num' %in% names(genealogy)
  results$has_id <- 'id' %in% names(genealogy)
  results$has_parent_id <- 'parent_id' %in% names(genealogy)
  results$has_the_seq <- 'the_seq' %in% names(genealogy)
  results$has_n_mut <- 'n_mut' %in% names(genealogy)
  results$has_recomb_pos <- 'recomb_pos' %in% names(genealogy)
  results$has_recomb_replaced <- 'recomb_replaced' %in% names(genealogy)
  results$has_recomb_partner <- 'recomb_partner' %in% names(genealogy)
  results$has_recomb_muts <- 'recomb_muts' %in% names(genealogy)
  results$has_fitness_score <- 'fitness_score' %in% names(genealogy)
  results$number_of_columns <- length(names(genealogy)) == 10
  results$column_order <- all(names(genealogy) == c("gen_num", "id", "parent_id", "the_seq", "n_mut", "recomb_pos", "recomb_replaced", "recomb_partner", "recomb_muts", "fitness_score"))
  return(results)
}
