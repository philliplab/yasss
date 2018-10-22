#' Simulate and process many populations
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{dcollection}: A \code{dcollection} as checked by \code{\link{check_dcollection}}
#'   \item \code{arg_collection}: The input \code{arg_collection} as checked by
#'   \code{\link{check_arg_collection}}.
#'   \item \code{all_genealogies} [OPTIONAL]: A \code{data.frame} that contains all the
#'   genealogies, or in the case that \code{last_gen_only} was specified, only the
#'   last generations of the genealogies and all the identifier columns.
#'   \item \code{all_dmats} [OPTIONAL]: A list of lists containing the
#'   distance matrixes and their indentifiers.
#' }
#' @param arg_collection The collection of argument sets to be used in the
#' simulations
#' @export

sim_proc_many_pops <- function(arg_collection){

  x <- check_arg_collection(arg_collection)
  if (!all(unlist(x))){
    return('error in arg_collection')
  }

  result <- list(arg_collection = arg_collection)
  dcollection <- NULL
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
    dsum$sim_id <- 1
    dsum$label <- c_arg_set$label
    dsum$sampling <- 'none'
    dcollection <- list(dcollection, dsum)

    # result packaging
  }
  result$dcollection <- dcollection
  return(result)
}

#' Checks the results produced by sim_proc_many_pops
#'
#' Inspects the return value from sim_proc_many_pops to check for any
#' deviations from the specification.
#'
#' There are no direct tests of this function based on manually created
#' datasets. The reason is that there are so many steps and most of those
#' steps already have their own checks. This this function is just designed to
#' check for obvious major deviations.
#'
#' @param many_pops The return value from sim_proc_many_pops
#' @export

check_many_pops <- function(many_pops){
  result <- list()
  result[['is_list']] <- class(many_pops) == 'list'

  result[['has_dcollection']] <- 'dcollection' %in% names(many_pops)
  if (result[['has_dcollection']]){
    dcollection_check_result <- check_dcollection(many_pops[['dcollection']])
    result[['valid_dcollection']] <- all(unlist(dcollection_check_result))
    if (!all(unlist(dcollection_check_result))){
      print(dcollection_check_result)
    }
  }
  return (result)
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
