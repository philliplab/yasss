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
  # class
  results$is_data.frame <- class(genealogy) == 'data.frame'
  
  # columns
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
  if (results$number_of_columns) {
    results$column_order <- all(names(genealogy) == c("gen_num", "id",
                                                      "parent_id", "the_seq",
                                                      "n_mut", "recomb_pos",
                                                      "recomb_replaced",
                                                      "recomb_partner",
                                                      "recomb_muts",
                                                      "fitness_score"))
  } else {
    results$column_order <- FALSE
  }

  # gen_num
  if (results$has_gen_num){
    results$gen_num_not_missing <- !any(is.na(genealogy$gen_num) |
                                        is.nan(genealogy$gen_num) |
                                        is.null(genealogy$gen_num))

    if (results$gen_num_not_missing){
      results$gen_num_naturals <- all(genealogy$gen_num %in% 0:max(genealogy$gen_num)) &
                                  all(0:max(genealogy$gen_num) %in% genealogy$gen_num)
    } else {
      results$gen_num_naturals <- FALSE
    } # if (results$gen_num_not_missing & results$has_gen_num)

  } else {
    results$gen_num_not_missing <- FALSE
    results$gen_num_naturals <- FALSE
  } # results$has_gen_num

  # id
  if (results$has_id){
    results$id_not_missing <- !any(is.na(genealogy$id) |
                                        is.nan(genealogy$id) |
                                        is.null(genealogy$id))

    if (results$id_not_missing){
      results$id_gt_zero <- all(genealogy$id > 0)
      
      results$id_no_duplicates_within_gen <- TRUE
      for (c_gen in unique(genealogy$gen_num)){
        c_genea <- subset(genealogy, gen_num == c_gen)
        results$id_no_duplicates_within_gen <- results$id_no_duplicates_within_gen &
          length(c_genea$id) == length(unique(c_genea$id)) &
          all(sort(c_genea$id) == sort(unique(c_genea$id)))
      }

    } else {
      results$id_gt_zero <- FALSE
      results$id_no_duplicates_within_gen <- FALSE
    } # if (results$id_not_missing)

  } else {
    results$id_gt_zero <- FALSE
    results$id_no_duplicates_within_gen <- FALSE
  } # results$has_id

  # parent_id
  if (results$has_parent_id){
    c_genea <- subset(genealogy, is.na(parent_id) | is.nan(parent_id) | is.null(parent_id))
    results$parent_id_after_gen_zero_not_missing <- all((unique(c_genea$gen_num) == 0) & 
                                                        (length(unique(c_genea$gen_num)) == 1))
    if (results$parent_id_after_gen_zero_not_missing){
      c_genea <- genealogy[genealogy$gen_num > 0,]
      if (nrow(c_genea) > 0){
        results$parent_id_gt_zero <- all(c_genea$parent_id > 0)
      } else {
        results$parent_id_gt_zero <- TRUE
      } # if (nrow(c_genea) > 0)
    } else {
      results$parent_id_gt_zero <- FALSE
    } #if (results$parent_id_after_gen_zero_not_missing)

  } else {
    results$parent_id_after_gen_zero_not_missing <- FALSE
    results$parent_id_gt_zero <- FALSE
  } # if (result$has_parent_id)

  #genealogy_expector(m_genea, false_list = c('parent_id_after_gen_zero_not_missing'))
  return(results)
}
