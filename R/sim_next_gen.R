#' Simulate the next generation
#'
#' Simulates the next generation by calling a mutator on each parent
#'
#' @return A genealogy data structure. TODO: link to general documentation on a genealogy. GH issue #6
#'
#' @param genealogy A genealogy data structure holding all the previous generations. TODO: link to general documentation. GH issue #6.
#' @param gen_size The size of each generation. Currently only allowed to be a
#' single integer. Default value is 2.
#' @param mutator A list with two elements fun and args specifying the function that mutates parents into their offspring and the list of arguments said function requires. 
#' @param gen_num The generation number. Defaults to 1.
#' @export

sim_next_gen <- function(genealogy, gen_size, mutator, gen_num = 1){
#  genealogy <- data.frame(gen_num = numeric(0),
#                          id = numeric(0),
#                          parent_id = numeric(0),
#                          the_seq = character(0),
#                          n_mut = character(0),
#                          recomb_pos = numeric(0),
#                          recomb_replaced = character(0),
#                          recomb_partner = numeric(0),
#                          recomb_muts = numeric(0),
#                          fitness_score = numeric(0),
#                          stringsAsFactors = FALSE
#                          )
  genealogy_names <- c("gen_num", "id", "parent_id", "the_seq", "n_mut", 
                       "recomb_pos", "recomb_replaced", "recomb_partner", 
                       "recomb_muts", "fitness_score")
  #TODO: Formalize this this checking for a genealogy's structure
  stopifnot(all(genealogy_names == names(genealogy)))

  total_offspring <- 0
  result <- list()
  parents <- genealogy[genealogy$gen_num == (gen_num - 1),]
  for (i in 1:nrow(parents)){
    for (j in 1:gen_size){
      total_offspring <- total_offspring + 1
      mut_arg <- mutator$arg
      mut_arg$parent <- parents[i, 'the_seq']
      mut_fun <- get(mutator$fun)
      mut_result <- do.call(mut_fun, mut_arg)
      child <- mut_result$child
      c_genealogy <- data.frame(gen_num = gen_num,
                                id = total_offspring,
                                parent_id = i,
                                the_seq = child,
                                n_mut = mut_result$mutation_stats$n_mut,
                                recomb_pos = NA_real_,
                                recomb_replaced = NA_character_,
                                recomb_partner = NA_real_,
                                recomb_muts = NA_real_,
                                fitness_score = NA_real_,
                                stringsAsFactors = FALSE)
      genealogy <- rbind(genealogy, c_genealogy, stringsAsFactors = FALSE)
    }
  }
  return(genealogy)
}

