#' Simulates evolution of DNA of a population
#'
#' Produce a set of DNA sequences by simulation a population with multiple generations allowing mutation and recombination
#'
#' Details: Simulated multiple generations TODO - expand this
#'
#' @return A genealogy data structure. TODO: link to general documentation on a genealogy. GH issue #6.
#'
#' @param ancestors A list of DNA sequences with which to start the population. Include the same sequence multiple times to achieve a target ratio.
#' @param r0 The number of offspring each molecule produces. Currently restricted to being an integer, but this will become a more complex construct in future versions (GH issue #17).
#' @param n_gen The number of generations to simulate.
#' @param n_pop Stop the simulation when the population size exceeds this number.
#' @param mutator A list with two elements fun and args specifying the name of the function that mutates parents into their offspring and the list of arguments said function requires. 
#' @param fitness_evaluator A list with two elements fun and args, specifying the name of the function that evaluates the fitness of each sequence and the list of arguments the function requires.
#' 
#' @examples
#' 
#' # Five generations, ancestor 60 As, mutation rate 10% per base per generation
#' x <- sim_pop(ancestors = paste(rep("A", 60), collapse = ''),
#'              r0 = 2,
#'              n_gen = 5,
#'              n_pop = Inf,
#'              mutator = list(fun = "mutator_uniform_fun",
#'                             args = list(mu = 0.1)),
#'              fitness_evaluator = list(fun = "fitness_evaluator_uniform_fun",
#'                                       args = NULL))
#' # Prints sequences of current generation
#' x %>% filter(gen_num == max(gen_num)) %>% select('the_seq')
#'
#' # Plots a histogram of the number of mutations between each parent
#' # and its offspring in the last generation
#' \dontrun{
#' ggplot(subset(x, gen_num == max(gen_num)), aes(n_mut)) + 
#'   geom_histogram(binwidth = 1)
#' }
#' @export 

sim_pop <- function(ancestors, 
                    r0 = 2, n_gen = NULL, n_pop = NULL,
                    mutator = list(fun = "mutator_uniform_fun",
                                   args = list(mu = 0.01)),
                    fitness_evaluator = list(fun = "fitness_evaluator_uniform_fun",
                                             args = NULL)){

  r0 <- tryCatch(round(as.numeric(r0), 0),
                       warning=function(w) return(list(round(as.numeric(r0), 0), w))
                       )
  if (class(r0) == 'list') {
    stop("r0 must be between 1 and 1e6")
  } else if (r0 < 1 | r0 > 1e6){
    stop("r0 must be between 1 and 1e6")
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
                          parent_id = NA_real_,
                          the_seq = ancestors,
                          n_mut = NA_real_,
                          recomb_pos = NA_real_,
                          recomb_replaced = NA_character_,
                          recomb_partner = NA_real_,
                          recomb_muts = NA_real_,
                          fitness_score = NA_real_,
                          stringsAsFactors = FALSE
                          )
  genealogy <- assign_fitness(genealogy, fitness_evaluator = fitness_evaluator)

  while ((c_pop < n_pop) & (c_gen < n_gen)){
    c_gen <- c_gen + 1
    
#    new_generation <- sim_next_gen(genealogy[genealogy$gen_num == (c_gen-1),], 
#                                   r0 = r0,
#                                   mutator, gen_num = c_gen)
#
    new_generation <- sim_next_gen(genealogy, 
                                   r0 = r0,
                                   mutator, gen_num = c_gen)

    # insert recomb here (act on new_generation)

    # insert fitness here (act on new_generation)
    new_generation <- assign_fitness(new_generation, fitness_evaluator = fitness_evaluator)

#    genealogy <- rbind(genealogy, new_generation)
    genealogy <- new_generation

    #TODO: Think about this - how will the fitness bit affect this next line?
    c_pop <- sum(genealogy$gen_num == c_gen)
  }
  return(genealogy)
}

