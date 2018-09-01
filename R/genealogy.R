#' Constructs a genealogy
#'
#' Makes a genealogy data structure and (not yet) checks that it is correct
#'
#' @param ancestors If no information other than the initial ancestors are available, just pass in the character vector with the ancestors and a genealogy will be intialized.
#' @export

make_genealogy <- function(ancestors){
  genealogy <- data.frame(gen_num = c_gen,
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
