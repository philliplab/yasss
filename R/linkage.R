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
  n_seqs <- length(seqs)
  seq_length <- nchar(seqs[1])

  seq_matrix <- unlist(strsplit(seqs, ''))
  seq_matrix <- matrix(seq_matrix, ncol = seq_length, nrow = length(seqs),
                       byrow = TRUE)
  cmat <- consensusMatrix_character(seqs)
  max_freq <- apply(cmat, 2, max)
  max_nuc <- rep('X', length(max_freq))
  for (i in 1:length(max_freq)){
    cmat_max_freq_row <- which(max_freq[i] == cmat[,i])[1]
    max_nuc[i] <- row.names(cmat)[cmat_max_freq_row]
  }

  jot <- matrix(0, nrow = ncol(cmat)-1, ncol = ncol(cmat))

  for (i in 1:nrow(seq_matrix)){
    for (L1 in 1:(ncol(cmat)-1)){
      
      n1 <- max_nuc[L1]
      if (max_freq[L1]/n_seqs < 0.2 | max_freq[L1]/n_seqs > 0.8){
        next
      }
      if (seq_matrix[i, L1] != n1){
        next
      }

      for (L2 in (L1+1):ncol(cmat)){

        n2 <- max_nuc[L2]
        if (max_freq[L2]/n_seqs < 0.2 | max_freq[L2]/n_seqs > 0.8){
          next
        }
        if (seq_matrix[i, L2] != n2){
          next
        }

        jot[L1, L2] <- jot[L1, L2] + 1

      }
    }
  }

  return(list(cmat = cmat,
              max_freq = max_freq,
              max_nuc = max_nuc,
              jot = jot)
  )
}
