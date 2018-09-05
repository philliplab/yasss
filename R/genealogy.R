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
  result <- list()

  result <- check_genealogy_structure(genealogy, result)

  result <- check_genealogy_gen_num(genealogy, result)

  result <- check_genealogy_id(genealogy, result)
  
  result <- check_genealogy_parent_id(genealogy, result)

  result <- check_genealogy_the_seq(genealogy, result)

  result <- check_genealogy_n_mut(genealogy, result)

  return(result)
}

#' Check the column names in a genealogy
#'
#' Checks that all the required column (and none but these columns) are present
#' in a genealogy.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_structure <- function(genealogy, result = list()){

  prerequisites <- NULL
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$is_data.frame <- FALSE
    result$has_gen_num <- FALSE
    result$has_id <- FALSE
    result$has_parent_id <- FALSE
    result$has_the_seq <- FALSE
    result$has_n_mut <- FALSE
    result$has_recomb_pos <- FALSE
    result$has_recomb_replaced <- FALSE
    result$has_recomb_partner <- FALSE
    result$has_recomb_muts <- FALSE
    result$has_fitness_score <- FALSE
    
    result$class_gen_num <- FALSE
    result$class_id <- FALSE
    result$class_parent_id <- FALSE
    result$class_the_seq <- FALSE
    result$class_n_mut <- FALSE
    result$class_recomb_pos <- FALSE
    result$class_recomb_replaced <- FALSE
    result$class_recomb_partner <- FALSE
    result$class_recomb_muts <- FALSE
    result$class_fitness_score <- FALSE
    
    result$number_of_columns <- FALSE
    result$column_order <- FALSE
    result$all_structure <- FALSE
    return(result)
  } else {
    # class
    result$is_data.frame <- class(genealogy) == 'data.frame'
    
    # column names
    result$has_gen_num <- 'gen_num' %in% names(genealogy)
    result$class_gen_num <- class(genealogy$gen_num) %in% c('numeric', 'integer')
    
    result$has_id <- 'id' %in% names(genealogy)
    result$class_id <- class(genealogy$id) %in% c('numeric', 'integer')
    
    result$has_parent_id <- 'parent_id' %in% names(genealogy)
    result$class_parent_id <- class(genealogy$parent_id) %in% c('numeric', 'integer')
    
    result$has_the_seq <- 'the_seq' %in% names(genealogy)
    result$class_the_seq <- class(genealogy$the_seq) == 'character'
    
    result$has_n_mut <- 'n_mut' %in% names(genealogy)
    result$class_n_mut <- class(genealogy$n_mut) == 'numeric'
    
    result$has_recomb_pos <- 'recomb_pos' %in% names(genealogy)
    result$class_recomb_pos <- class(genealogy$recomb_pos) == 'numeric'
    
    result$has_recomb_replaced <- 'recomb_replaced' %in% names(genealogy)
    result$class_recomb_replaced <- class(genealogy$recomb_replaced) == 'character'
    
    result$has_recomb_partner <- 'recomb_partner' %in% names(genealogy)
    result$class_recomb_partner <- class(genealogy$recomb_partner) == 'numeric'
    
    result$has_recomb_muts <- 'recomb_muts' %in% names(genealogy)
    result$class_recomb_muts <- class(genealogy$recomb_muts) == 'numeric'
    
    result$has_fitness_score <- 'fitness_score' %in% names(genealogy)
    result$class_fitness_score <- class(genealogy$fitness_score) == 'numeric'

    
    
    
    




    # number of columns
    result$number_of_columns <- length(names(genealogy)) == 10

    # order of columns
    if (!result$number_of_columns) {
      result$column_order <- FALSE
      result$all_structure <- FALSE
      return(result)
    } else {
      result$column_order <- all(names(genealogy) == c("gen_num", "id",
                                                        "parent_id", "the_seq",
                                                        "n_mut", "recomb_pos",
                                                        "recomb_replaced",
                                                        "recomb_partner",
                                                        "recomb_muts",
                                                        "fitness_score"))
      result$all_structure <- (result$is_data.frame &
                                result$has_gen_num &
                                result$class_gen_num &
                                result$has_id &
                                result$class_id &
                                result$has_parent_id &
                                result$class_parent_id &
                                result$has_the_seq &
                                result$class_the_seq &
                                result$has_n_mut &
                                result$class_n_mut &
                                result$has_recomb_pos &
                                result$class_recomb_pos &
                                result$has_recomb_replaced &
                                result$class_recomb_replaced &
                                result$has_recomb_partner &
                                result$class_recomb_partner &
                                result$has_recomb_muts &
                                result$class_recomb_muts &
                                result$has_fitness_score &
                                result$class_fitness_score &
                                result$column_order)
    } # else of if (!result$number_of_columns) 
  } # else of if (prerequisites_not_met)
  return(result)
}

#' Check the gen_num column in a genealogy
#'
#' Checks that gen_num is not missing, is a natural number and contains all
#' numbers between zero and the max gen_num.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_gen_num <- function(genealogy, result = list()){

  prerequisites <- "has_gen_num"
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$gen_num_not_missing <- FALSE
    result$gen_num_naturals <- FALSE
    result$all_gen_num <- FALSE
    return(result)
  } else {
    # gen_num
    result$gen_num_not_missing <- !any(is.na(genealogy$gen_num) |
                                        is.nan(genealogy$gen_num) |
                                        is.null(genealogy$gen_num))

    if (!result$gen_num_not_missing){
      result$gen_num_naturals <- FALSE
      result$all_gen_num <- FALSE
      return(result)
    } else {
      result$gen_num_naturals <- all(genealogy$gen_num %in% 0:max(genealogy$gen_num)) &
                                  all(0:max(genealogy$gen_num) %in% genealogy$gen_num)
      result$all_gen_num <- result$gen_num_naturals
    } # else of if (result$gen_num_not_missing)
  }

  return(result)
}

#' Check the id column in a genealogy
#'
#' Checks that gen_num is not missing, is greater than zero and is an integer.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_id <- function(genealogy, result = list()){

  prerequisites <- "has_id"
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$id_gt_zero <- FALSE
    result$id_no_duplicates_within_gen <- FALSE
    result$id_is_integer <- FALSE
    result$id_not_missing <- FALSE
    result$all_id <- FALSE
    return(result)
  } else {
  # id
    result$id_not_missing <- !any(is.na(genealogy$id) |
                                        is.nan(genealogy$id) |
                                        is.null(genealogy$id))

    if (!result$id_not_missing){
      result$id_gt_zero <- FALSE
      result$id_no_duplicates_within_gen <- FALSE
      result$id_is_integer <- FALSE
      result$all_id <- FALSE
      return(result)
    } else {

      result$id_gt_zero <- all(genealogy$id > 0)
      
      result$id_no_duplicates_within_gen <- TRUE
      for (c_gen in unique(genealogy$gen_num)){
        c_genea <- subset(genealogy, gen_num == c_gen)
        result$id_no_duplicates_within_gen <- result$id_no_duplicates_within_gen &
          length(c_genea$id) == length(unique(c_genea$id)) &
          all(sort(c_genea$id) == sort(unique(c_genea$id)))
      }

      result$id_is_integer <- all(floor(genealogy$id) == ceiling(genealogy$id))
      result$all_id <- result$id_gt_zero & 
        result$id_no_duplicates_within_gen & 
        result$id_is_integer
    } # else of if (!result$id_not_missing)
  } # else of if (prerequisites_not_met)
  return(result)
}

#' Check the parent_id column in a genealogy
#'
#' Checks that parent_id is not missing, is a valid integer and that it
#' references a parent that exists.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_parent_id <- function(genealogy, result = list()){

  prerequisites <- c("has_parent_id", "has_gen_num", "gen_num_not_missing")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$parent_id_after_gen_zero_not_missing <- FALSE
    result$parent_id_gt_zero <- FALSE
    result$all_parent_ids_present <- FALSE
    result$all_parent_id <- FALSE
    return(result)
  } else {
    c_genea <- subset(genealogy, is.na(parent_id) | is.nan(parent_id) | is.null(parent_id))
    result$parent_id_after_gen_zero_not_missing <- all((unique(c_genea$gen_num) == 0) & 
                                                        (length(unique(c_genea$gen_num)) == 1))
    if (!result$parent_id_after_gen_zero_not_missing){
      result$parent_id_gt_zero <- FALSE
      result$all_parent_ids_present <- FALSE
      result$all_parent_id <- FALSE
      return(result)
    } else {

      if (!(max(genealogy$gen_num) > 0)){
        result$parent_id_gt_zero <- TRUE
        result$all_parent_ids_present <- TRUE
        result$all_parent_id <- TRUE
        return(result)
      } else {
        c_genea <- genealogy[genealogy$gen_num > 0,]
        result$parent_id_gt_zero <- all(c_genea$parent_id > 0)
      
        result$all_parent_ids_present <- TRUE
        for (c_gen in 1:max(genealogy$gen_num)){
          c_all_parent_ids_present <- all(genealogy[genealogy$gen_num == c_gen, 'parent_id'] %in%
                                       genealogy[genealogy$gen_num == (c_gen - 1), 'id'])
          result$all_parent_ids_present <- result$all_parent_ids_present &
                                            c_all_parent_ids_present
        } # for
        result$all_parent_id <- result$parent_id_gt_zero &
                                 result$all_parent_ids_present
      } # else of if (max(genealogy$gen_num) > 0)

    } # else of if (!result$parent_id_after_gen_zero_not_missing)

  } # else of if (prerequisites_not_met)
  return(result)
}

#' Check the the_seq column in a genealogy
#'
#' Checks that the_seq contains only valid nucleic letters.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_the_seq <- function(genealogy, result = list()){

  prerequisites <- c("has_the_seq")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$the_seq_not_missing <- FALSE
    result$the_seq_valid_letters <- FALSE
    result$all_the_seq <- FALSE
    return(result)
  } else {
    result$the_seq_not_missing <- !(any(is.na(genealogy$the_seq)) | 
                                     any(is.nan(genealogy$the_seq)) | 
                                     any(is.null(genealogy$the_seq)) |
                                     any(genealogy$the_seq == ''))
    if (!result$the_seq_not_missing){
      result$the_seq_valid_letters <- FALSE
      result$all_the_seq <- FALSE
      return(result)
    } else {
      all_lets <- unique(strsplit(paste(genealogy$the_seq, collapse = ''), '')[[1]])
      result$the_seq_valid_letters <- all(all_lets %in% c('A', 'C', 'G', 'T'))
      result$all_the_seq <- result$the_seq_valid_letters
    }
  } # if (prerequisites_not_met)

  return(result)
}

#' Check the n_mut column in a genealogy
#'
#' Checks that n_mut contains only valid nucleic letters.
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param genealogy The genealogy to check.
#' @param result The list to which the result will be added and from which
#' previous result will be drawn to check the prerequisites.
#' @export

check_genealogy_n_mut <- function(genealogy, result = list()){

  prerequisites <- c("has_n_mut", "all_the_seq", "all_parent_id")
  prerequisites_not_met <- FALSE
  for (i in names(prerequisites)){
    if (result[[i]] == FALSE){
      prerequisites_not_met <- TRUE
    }
  }

  if (prerequisites_not_met){
    result$n_mut_not_missing <- FALSE
    result$n_mut_is_integer <- FALSE
    result$n_mut_calc <- FALSE
    result$all_n_mut <- FALSE
    result$n_mut_first_gen_NA <- FALSE
    return(result)
  } else {
    # The ancestors are not compared to anything, so n_mut is NA
    first_gen <- genealogy %>% filter(gen_num == min(gen_num))
    result$n_mut_first_gen_NA <- all(is.na(first_gen$n_mut))
    
    non_first_gen <- genealogy %>% filter(gen_num != min(gen_num))
    if (nrow(non_first_gen) == 0){
      result$n_mut_not_missing <- TRUE
      result$n_mut_is_integer <- TRUE
      result$n_mut_calc <- TRUE
      result$all_n_mut <- TRUE
      result$n_mut_first_gen_NA <- TRUE
    } else {
      result$n_mut_not_missing <- !(any(is.na(non_first_gen$n_mut)) | 
                                    any(is.nan(non_first_gen$n_mut)) | 
                                    any(is.null(non_first_gen$n_mut)))
  
      if (!result$n_mut_not_missing){
        result$n_mut_is_integer <- FALSE
        result$all_n_mut <- FALSE
        result$n_mut_calc <- FALSE
        return(result)
      } else {
        result$n_mut_is_integer <- all(floor(non_first_gen$n_mut) == ceiling(non_first_gen$n_mut))
        for (i in 1:nrow(genealogy)){
          result$n_mut_calc <- TRUE
          if (!is.na(genealogy[i,'parent_id'])){
#            compare the_seq to the_seq of parent
            c_the_seq <- genealogy[i,'the_seq']
            c_gen_num <- genealogy[i,'gen_num']
            c_parent_id <- genealogy[i, 'parent_id']
            c_n_mut <- genealogy[i, 'n_mut']
            p_the_seq <- genealogy %>% filter(gen_num == (c_gen_num - 1) &
                                              id == c_parent_id)
            p_the_seq <- p_the_seq$the_seq
            if (length(p_the_seq) != 1){
              result$n_mut_calc <- FALSE
              break
            }
            if (nchar(p_the_seq) != nchar(c_the_seq)){
              result$n_mut_calc <- FALSE
              break
            }
            result$n_mut_calc <- stringdist(c_the_seq, p_the_seq, method = 'hamming') == c_n_mut
          }
        }
        result$all_n_mut <- result$n_mut_is_integer & 
                            result$n_mut_calc & 
                            result$n_mut_first_gen_NA
      } # else of if(!result$n_mut_not_missing)
    } # else of if(nrow(non_first_gen) == 0)
  } # else of if (prerequisites_not_met)

  return(result)
}

