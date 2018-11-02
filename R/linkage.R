#' Computes the linkage disequilibrium for a set of sequences
#'
#' @return A list with multiple elements:
#' \itemize{
#'   \item linkage The linkage scores for various distances and starting
#'   positions.
#'   \item cmat The consensusMatrix. Only included if verbose is specified.
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
  cmat <- consensusMatrix_character(seqs)
  max_freq <- apply(cmat, 2, max)
  max_nuc <- rep('X', length(max_freq))
  for (i in 1:length(max_freq)){
    cmat_max_freq_row <- which(max_freq[i] == cmat[,i])[1]
    max_nuc[i] <- row.names(cmat)[cmat_max_freq_row]
  }

  return(list(cmat = cmat,
              max_freq = max_freq,
              max_nuc = max_nuc)
  )
}
