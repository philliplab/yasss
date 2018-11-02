#' Computes the linkage disequilibrium for a set of sequences
#'
#' @return A list with multiple elements:
#' \itemize{
#'   \item linkage The linkage scores for various distances and starting
#'   positions.
#'   \item cm The consensusMatrix. Only included if verbose is specified.
#' }
#'
#' @param seqs The sequences on which the linkage disequilibrium is to be
#' computed
#' @param min_prev The minimum prevalence the majority variant must attain to
#' be considered for inclusion. (This cutoff will not see much action)
#' @param max_prev Majoritity variants with prevalences higher than this will
#' not be considered.
#' @param verbose Should intermediate results be included in the return value?
#' @export

linkage_diseq <- function(seqs, min_prev = 0.2, max_prev = 0.8, verbose = FALSE){
  cm <- consensusMatrix_character(seqs)
  return(list(cm = cm))
}
