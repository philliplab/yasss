#' Constructs a genealogy
#'
#' Initializes a new genealogy from the ancestor sequences.
#'
#' @param ancestors Character vector containing the sequences of the ancestors
#' of the genealogy.
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
#' Checks a genealogy for correctness by ensuring that all the columns are
#' present and that they do not violate any of the structural requirements
#' (like referencing a parent that does not exist)
#'
#' @return A list with TRUE or FALSE indicating whether the related check passed.
#'
#' @param genealogy The genealogy data.frame that must be checked.
#' @export

check_genealogy <- function(genealogy){
  results <- list()

  results <- check_genealogy_structure(genealogy, results)

  results <- check_genealogy_gen_num(genealogy, results)

  results <- check_genealogy_id(genealogy, results)
  
  results <- check_genealogy_parent_id(genealogy, results)

  results <- check_genealogy_the_seq(genealogy, results)

  #genealogy_expector(m_genea, false_list = c('parent_id_after_gen_zero_not_missing'))
  return(results)
}

#' Check the column names in a genealogy
#'
#' Checks that all the required column (and none but these columns) are present
#' in a genealogy.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_structure <- function(genealogy, results = list()){

  prerequisites <- NULL
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$is_data.frame <- FALSE
    results$has_gen_num <- FALSE
    results$has_id <- FALSE
    results$has_parent_id <- FALSE
    results$has_the_seq <- FALSE
    results$has_n_mut <- FALSE
    results$has_recomb_pos <- FALSE
    results$has_recomb_replaced <- FALSE
    results$has_recomb_partner <- FALSE
    results$has_recomb_muts <- FALSE
    results$has_fitness_score <- FALSE
    results$number_of_columns <- FALSE
    results$column_order <- FALSE
    results$all_structure <- FALSE
    return(results)
  } else {
    # class
    results$is_data.frame <- class(genealogy) == 'data.frame'
    
    # column names
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

    # number of columns
    results$number_of_columns <- length(names(genealogy)) == 10

    # order of columns
    if (!results$number_of_columns) {
      results$column_order <- FALSE
      results$all_structure <- FALSE
      return(results)
    } else {
      results$column_order <- all(names(genealogy) == c("gen_num", "id",
                                                        "parent_id", "the_seq",
                                                        "n_mut", "recomb_pos",
                                                        "recomb_replaced",
                                                        "recomb_partner",
                                                        "recomb_muts",
                                                        "fitness_score"))
      results$all_structure <- results$column_order
    } # else of if (!results$number_of_columns) 
  } # else of if (prerequisites_not_met)
  return(results)
}

#' Check the gen_num column in a genealogy
#'
#' Checks that gen_num is not missing, is a natural number and contains all
#' numbers between zero and the max gen_num.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_gen_num <- function(genealogy, results = list()){

  prerequisites <- "has_gen_num"
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$gen_num_not_missing <- FALSE
    results$gen_num_naturals <- FALSE
    results$all_gen_num <- FALSE
    return(results)
  } else {
    # gen_num
    results$gen_num_not_missing <- !any(is.na(genealogy$gen_num) |
                                        is.nan(genealogy$gen_num) |
                                        is.null(genealogy$gen_num))

    if (!results$gen_num_not_missing){
      results$gen_num_naturals <- FALSE
      results$all_gen_num <- FALSE
      return(results)
    } else {
      results$gen_num_naturals <- all(genealogy$gen_num %in% 0:max(genealogy$gen_num)) &
                                  all(0:max(genealogy$gen_num) %in% genealogy$gen_num)
      results$all_gen_num <- results$gen_num_naturals
    } # else of if (results$gen_num_not_missing)
  }

  return(results)
}

#' Check the id column in a genealogy
#'
#' Checks that gen_num is not missing, is greater than zero and is an integer.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_id <- function(genealogy, results = list()){

  prerequisites <- "has_id"
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$id_gt_zero <- FALSE
    results$id_no_duplicates_within_gen <- FALSE
    results$id_is_integer <- FALSE
    results$id_not_missing <- FALSE
    results$all_id <- FALSE
    return(results)
  } else {
  # id
    results$id_not_missing <- !any(is.na(genealogy$id) |
                                        is.nan(genealogy$id) |
                                        is.null(genealogy$id))

    if (!results$id_not_missing){
      results$id_gt_zero <- FALSE
      results$id_no_duplicates_within_gen <- FALSE
      results$id_is_integer <- FALSE
      results$all_id <- FALSE
      return(results)
    } else {

      results$id_gt_zero <- all(genealogy$id > 0)
      
      results$id_no_duplicates_within_gen <- TRUE
      for (c_gen in unique(genealogy$gen_num)){
        c_genea <- subset(genealogy, gen_num == c_gen)
        results$id_no_duplicates_within_gen <- results$id_no_duplicates_within_gen &
          length(c_genea$id) == length(unique(c_genea$id)) &
          all(sort(c_genea$id) == sort(unique(c_genea$id)))
      }

      results$id_is_integer <- all(floor(genealogy$id) == ceiling(genealogy$id))
      results$all_id <- results$id_gt_zero & 
        results$id_no_duplicates_within_gen & 
        results$id_is_integer
    } # else of if (!results$id_not_missing)
  } # else of if (prerequisites_not_met)
  return(results)
}

#' Check the parent_id column in a genealogy
#'
#' Checks that parent_id is not missing, is a valid integer and that it
#' references a parent that exists.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_parent_id <- function(genealogy, results = list()){

  prerequisites <- c("has_parent_id", "has_gen_num", "gen_num_not_missing")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$parent_id_after_gen_zero_not_missing <- FALSE
    results$parent_id_gt_zero <- FALSE
    results$all_parent_ids_present <- FALSE
    results$all_parent_id <- FALSE
    return(results)
  } else {
    c_genea <- subset(genealogy, is.na(parent_id) | is.nan(parent_id) | is.null(parent_id))
    results$parent_id_after_gen_zero_not_missing <- all((unique(c_genea$gen_num) == 0) & 
                                                        (length(unique(c_genea$gen_num)) == 1))
    if (!results$parent_id_after_gen_zero_not_missing){
      results$parent_id_gt_zero <- FALSE
      results$all_parent_ids_present <- FALSE
      results$all_parent_id <- FALSE
      return(results)
    } else {

      if (!(max(genealogy$gen_num) > 0)){
        results$parent_id_gt_zero <- TRUE
        results$all_parent_ids_present <- TRUE
        results$all_parent_id <- TRUE
        return(results)
      } else {
        c_genea <- genealogy[genealogy$gen_num > 0,]
        results$parent_id_gt_zero <- all(c_genea$parent_id > 0)
      
        results$all_parent_ids_present <- TRUE
        for (c_gen in 1:max(genealogy$gen_num)){
          c_all_parent_ids_present <- all(genealogy[genealogy$gen_num == c_gen, 'parent_id'] %in%
                                       genealogy[genealogy$gen_num == (c_gen - 1), 'id'])
          results$all_parent_ids_present <- results$all_parent_ids_present &
                                            c_all_parent_ids_present
        } # for
        results$all_parent_id <- results$parent_id_gt_zero &
                                 results$all_parent_ids_present
      } # else of if (max(genealogy$gen_num) > 0)

    } # else of if (!results$parent_id_after_gen_zero_not_missing)

  } # else of if (prerequisites_not_met)
  return(results)
}

#' Check the the_seq column in a genealogy
#'
#' Checks that the_seq contains only valid nucleic letters.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_the_seq <- function(genealogy, results = list()){

  prerequisites <- c("has_the_seq")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$the_seq_not_missing <- FALSE
    results$the_seq_valid_letters <- FALSE
    results$all_the_seq <- FALSE
    return(results)
  } else {
    results$the_seq_not_missing <- !(any(is.na(genealogy$the_seq)) | 
                                     any(is.nan(genealogy$the_seq)) | 
                                     any(is.null(genealogy$the_seq)) |
                                     any(genealogy$the_seq == ''))
    if (!results$the_seq_not_missing){
      results$the_seq_valid_letters <- FALSE
      results$all_the_seq <- FALSE
      return(results)
    } else {
      all_lets <- unique(strsplit(paste(genealogy$the_seq, collapse = ''), '')[[1]])
      results$the_seq_valid_letters <- all(all_lets %in% c('A', 'C', 'G', 'T'))
      results$all_the_seq <- results$the_seq_valid_letters
    }
  } # if (prerequisites_not_met)

  return(results)
}

#' Check the n_mut column in a genealogy
#'
#' Checks that n_mut contains only valid nucleic letters.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param results The list to which the results will be added and from which
#' previous results will be drawn to check the prerequisites.
#' @export

check_genealogy_n_mut <- function(genealogy, results = list()){

  prerequisites <- c("has_the_seq")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (results[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    results$the_seq_not_missing <- FALSE
    results$the_seq_valid_letters <- FALSE
    return(results)
  } else {
    results$the_seq_not_missing <- !(any(is.na(genealogy$the_seq)) | 
                                     any(is.nan(genealogy$the_seq)) | 
                                     any(is.null(genealogy$the_seq)) |
                                     any(genealogy$the_seq == ''))
    if (!results$the_seq_not_missing){
      results$the_seq_valid_letters <- FALSE
      return(results)
    } else {
      all_lets <- unique(strsplit(paste(genealogy$the_seq, collapse = ''), '')[[1]])
      results$the_seq_valid_letters <- all(all_lets %in% c('A', 'C', 'G', 'T'))
    }
  } # if (prerequisites_not_met)

  return(results)
}

