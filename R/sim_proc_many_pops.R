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
#'   \item \code{n_sims}: The input \code{n_sims} indicating the number of
#'   datasets you can expect.
#' }
#' @param arg_collection The collection of argument sets to be used in the
#' simulations
#' @param n_sims The number of times sim_pop must be called for each arg_set.
#' The total number of data sets produced will be \code{n_sims *
#' length(arg_collection)} if no fitness processing is specified and \code{n_sims *
#' length(arg_collection) * 2} if any fitness processing is specified.
#' @param output_dmat If TRUE, the distance matrices will be included in the
#' output. Uses lots of memory - default is FALSE.
#' @param max_dmat_size If more sequences than this number is present in a
#' last generation on which a distance matrix is about to be computed, then
#' \code{max_dmat_size} sequences will be randomly sampled and the distance
#' matrix will only be computed on those sequences. Default is 10000.
#' @param fitness_processing The way that the fitness scores should be used to
#'  sample from the genealogy. Valid options include:
#'  \itemize{
#'    \item none: This will produce one dataset per genealogy.
#'    \item fit_unfit_pair: This will use the threshold approach to remove all unfit
#'    individuals and their offspring. The remaining members of the last
#'    generation will be considered to be the fit member of the pair. An equal
#'    number of individuals will be sampled at random from the original
#'    genealogy to produce the unfit pair such that it is the same size as the
#'    fit pair.
#'    \item fit_unfit_unmatched_pair: Same as fit_unfit_pair except that the
#'    unfit member will not be down sampled to size match the fit member.
#' }
#' @param n_gen_with_perfect_fitness The number of initial generations that
#' must be assigned perfect fitness. If an early ancestor has low fitness and
#' is thus removed, the likelihood that the end up with an empty genealogy is
#' too large.
#' @export

#- `output_genealogy`: Should the genealogy data sets be deleted to reduce
#  memory usage? Valid options:
#  * `last_gen_only`
#  * `none`
#  * `full`

sim_proc_many_pops <- function(arg_collection, n_sims = 1, output_dmat = FALSE, max_dmat_size = 10000,
                               fitness_processing = 'none', n_gen_with_perfect_fitness = 4){

  x <- check_arg_collection(arg_collection)
  if (!all(unlist(x))){
    return('error in arg_collection')
  }

  result <- list(arg_collection = arg_collection)
  dcollection <- NULL
  all_dmats <- NULL
  for (c_arg_set in arg_collection){
    for (sim_id in 1:n_sims){

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

      genea[genea$gen_num <= n_gen_with_perfect_fitness, 'fitness_score'] <- 1

      genea$sim_id <- sim_id
      genea$label <- c_arg_set$label

      if (fitness_processing == 'none'){
        genea$sampling <- 'none'
        last_gen <- genea %>% filter(gen_num == max(gen_num))

        new_last_gens <- list(last_gen)


      } else if (fitness_processing == 'fit_unfit_pair'){
        genea$sampling <- 'size_matched_sample'
        fit_genea <- get_fit_offspring(genea, c_arg_set$required_fitness, 'Rvec')
        fit_genea$sampling <- 'fitness_restricted'

        fit_last_gen <- fit_genea %>% filter(gen_num == max(gen_num))

        n_fit_last_gen <- nrow(fit_last_gen)
        unfit_last_gen <- genea %>% filter(gen_num == max(gen_num))
        unfit_last_gen <- unfit_last_gen[sample(1:nrow(unfit_last_gen), n_fit_last_gen), ]

        new_last_gens <- list(fit_last_gen, unfit_last_gen)
      
      } else {
        stop('not implemented')
      }

      # dmat
      for (c_last_gen in new_last_gens){
        
        # parse out identifiers
        c_sim_id <- unique(c_last_gen$sim_id)
        c_label <- unique(c_last_gen$label)
        c_sampling <- unique(c_last_gen$sampling)

        if (length(c_last_gen$the_seq) > max_dmat_size){
          seqs_for_dmat <- sample(c_last_gen$the_seq, max_dmat_size)
        } else {
          seqs_for_dmat <- c_last_gen$the_seq
        }
        dmat <- stringdistmatrix(seqs_for_dmat)
        if (output_dmat){
          c_dmat <- list(dmat = dmat,
                         sim_id = c_sim_id,
                         label = c_label,
                         sampling = c_sampling)
          all_dmats <- c(all_dmats, list(c_dmat))
        }

        # dsum
        dsum <- summarize_dmat(dmat)
        dsum$sim_id <- c_sim_id
        dsum$label <- c_label
        dsum$sampling <- c_sampling
#        dcollection[[length(dcollection)+1]] <- dsum
        dcollection <- c(dcollection, list(dsum))
      }
    # result packaging
    }
  }
  result$dcollection <- dcollection
  result$n_sims <- n_sims
  if (output_dmat){
    result$all_dmats <- all_dmats
  }
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
#' @param verbose If TRUE, print details of failures
#' @export

check_many_pops <- function(many_pops, verbose = FALSE){
  result <- list()
  result[['is_list']] <- class(many_pops) == 'list'

  # n_sims
  result[['n_sims_length_one']] <- length(many_pops$n_sims) == 1

  result[['n_sims_integer']] <- class(many_pops$n_sims) %in% c('numeric', 'integer')
  if (result[['n_sims_integer']] & result[['n_sims_length_one']]){
    result[['n_sims_integer']] <- floor(many_pops$n_sims) == ceiling(many_pops$n_sims)
  } else {
    result[['n_sims_integer']] <- FALSE
  }

  # dcollection
  result[['has_dcollection']] <- 'dcollection' %in% names(many_pops)
  if (result[['has_dcollection']]){
    dcollection_check_result <- check_dcollection(many_pops[['dcollection']])
    result[['valid_dcollection']] <- all(unlist(dcollection_check_result))
    if (!all(unlist(dcollection_check_result)) & verbose){
      print(dcollection_check_result)
    }
  }

  # all_dmats
  if ('all_dmats' %in% names(many_pops)){
    result[['valid_all_dmats']] <- class(result[['all_dmats']]) == 'list'
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
