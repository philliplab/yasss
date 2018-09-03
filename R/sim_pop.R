#' Simulates evolution of DNA of a population
#'
#' Produce a set of DNA sequences by simulation a population with multiple generations allowing mutation and recombination
#'
#' Details: Simulated multiple generations TODO - expand this
#'
#' @return A genealogy data structure. TODO: link to general documentation on a genealogy. GH issue #6.
#'
#' @param ancestors A list of DNA sequences with which to start the population. Include the same sequence multiple times to achieve a target ratio.
#' @param gen_size The size of each generation (as a multiple of the size of the parent generation). Currently only allowed to be a single integer. Default value is 2.
#' @param n_gen The number of generations to simulate.
#' @param n_pop Stop the simulation when the population size exceeds this number.
#' @param mutator A list with two elements fun and args specifying the function that mutates parents into their offspring and the list of arguments said function requires. 
#' @export 

sim_pop <- function(ancestors, 
                    gen_size = 2, n_gen = NULL, n_pop = NULL,
                    mutator = list(fun = mutator_uniform_fun,
                                   args = list(mu = 0.01))){

  gen_size <- tryCatch(round(as.numeric(gen_size), 0),
                       warning=function(w) return(list(round(as.numeric(gen_size), 0), w))
                       )
  if (class(gen_size) == 'list') {
    stop("gen_size must be between 1 and 1e6")
  } else if (gen_size < 1 | gen_size > 1e6){
    stop("gen_size must be between 1 and 1e6")
  }

  if (is.null(n_gen)) {n_gen <- Inf}
  if (is.null(n_pop)) {n_pop <- Inf}

  if (n_gen == Inf & n_pop == Inf){
    stop('Either n_gen or n_pop must be specified and at least one must be finite')
  }
  if (n_gen < 1 | n_pop < 1){
    stop('Neither n_gen nor n_pop may be set to less than one')
  }
  
  parents <- ancestors

  c_gen <- 0
  c_pop <- length(ancestors)

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

  while ((c_pop < n_pop) & (c_gen < n_gen)){
    c_gen <- c_gen + 1
    
#    new_generation <- sim_next_gen(genealogy[genealogy$gen_num == (c_gen-1),], 
#                                   gen_size = gen_size,
#                                   mutator, gen_num = c_gen)
#
    new_generation <- sim_next_gen(genealogy, 
                                   gen_size = gen_size,
                                   mutator, gen_num = c_gen)

    # insert recomb here (act on new_generation)

    # insert fitness here (act on new_generation)

#    genealogy <- rbind(genealogy, new_generation)
    genealogy <- new_generation

    #TODO: Think about this - how will the fitness bit affect this next line?
    c_pop <- sum(genealogy$gen_num == c_gen)
  }
  return(genealogy)
}

