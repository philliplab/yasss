#' Simulate and process many populations
#'
#' @param arg_collection The collection of argument sets to be used in the
#' simulations
#' @export

sim_proc_many_pops <- function(arg_collection){
  for (c_arg_set in arg_collection){

    # sim_pop
    arg_set <- c_arg_set
    if (is.null(arg_set$n_gen)){
      arg_set$n_gen <- Inf
    }
    if (is.null(arg_set$n_pop)){
      arg_set$n_pop <- Inf
    }
    sim_pop_args <- list(
      ancestors = arg_set$ancestors,
      r0 = arg_set$r0,
      n_gen = arg_set$n_gen,
      n_pop = arg_set$n_pop,
      mutator = arg_set$mutator,
      fitness_evaluator = arg_set$fitness_evaluator
                         )
    genea <- do.call(sim_pop, sim_pop_args)

    last_gen <- genea %>% filter(gen_num == max(gen_num))

    # dmat
    dmat <- stringdistmatrix(last_gen$the_seq)

    # dsum
    dsum <- summarize_dmat(dmat)

    # result packaging
  }
  return(dsum)
}

#- loop over `arg_collection`
#- loop over `n_pop`
#- call `sim_pop`
#- if `fitness_processing == none`: just add in the columns
#- if `fitness_processing == fit_unfit_pair`:
#  * `get_fit_offspring` with `fitness_threshold`
#    + add in identifying columns
#    + append into `new_genealogies` (full genealogy)
#  * size matched sampling
#    + add in identifying columns
#    + append into `new_genealogies` (full genealogy)
#- if `fitness_processing == fit_unfit_unmatched_pair`:
#  * `get_fit_offspring` with `fitness_threshold`
#    + add in identifying columns
#    + append into `new_genealogies` (full genealogy)
#  * just take the fill genealogy
#    + add in identifying columns
#    + append into `new_genealogies` (full genealogy)
#- if `output_genealogy == last_gen_only`
#  * loop over `new_genealogies`
#    + filter out `last_gen`  
#    + append `last_gen` to `output_genealogies`
#- if `output_genealogy == full`
#  * append `new_genealogies` to `output_genealogies`
#- loop over `new_genealogies`
#  * filter out `last_gen`
#  * call `summarize_dmat`
#  * add in identifiers
#  * add new dsum into `all_dsums`
#- if `output_dmat`
#  * add the dmat into `all_dmats`
