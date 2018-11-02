#' Computes a consensus matrix.
#'
#' @description Tallies characters at each position for a set of character vectors without any Bioconductor packages.
#'
#' @details
#' Designed to function like consensusMatrix from Biostrings.
#'
#' This function can handle upper and lower case input, but will only produce upper case output.
#'
#' To prevent a dependency on a bioconductor package, this is not properly set
#' up as a method for the consensusMatrix generic defined in Biostrings.
#'
#' @param x The sequence data as a set of character vectors
#' @return A matrix with columns representing positions in the sequence and rows the allowed characters in IUPAC notation of nucleotides and cells the counts of the respective character at the respective position.
#'
#' @export

consensusMatrix_character <- function(x){
  stopifnot(class(x) == 'character')
  stopifnot(max(nchar(x)) == min(nchar(x)))

  max_length <- max(sapply(x, nchar))

  seq_matrix <- unlist(strsplit(x, ''))
  seq_matrix <- matrix(seq_matrix, ncol = max_length, nrow = length(x))

  cons_matrix <- matrix(0, nrow = 18, ncol = max_length)
  row.names(cons_matrix) <- c("A", "C", "G", "T", "M", "R", "W", "S", "Y",
                              "K", "V", "H", "D", "B", "N", "-", "+", ".")
  
  for (i in 1:max_length){
    col_tab <- table(toupper(seq_matrix[,i]))
    cons_matrix[match(names(col_tab), row.names(cons_matrix)), i] <- col_tab
  }

  cons_matrix
}

