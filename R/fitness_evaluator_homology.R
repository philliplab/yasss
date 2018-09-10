#' The homology based fitness_evaluator
#'
#' Assigns each sequences a fitness based on the homology to provided sequences
#'
#' @return A list with a single element called fitness.
#' @param the_seq All the sequences to which fitnesses must be assigned.
#' @param comparators The sequences to which the input sequences must be compared
#' @param h2fs A function that remaps the homology results between the input sequences and the comparators to a fitness score.
#' @export

fitness_evaluator_homology_fun <- function(the_seq, comparators, h2fs){
  stopifnot(class(the_seq) == "character")

  dists <- NULL

  comparator_num <- 0
  c_comparator <- comparators[1]
  for (c_comparator in comparators){
    num_x <- sum(charToRaw(c_comparator) == charToRaw("X"))
    comparator_num <- comparator_num + 1
    c_dists <- data.frame(X = stringdist(c_comparator, the_seq))
    c_dists$X <- c_dists$X - num_x
    if (any(c_dists$X < 0)){
      c_dists$X[c_dists$X < 0] <- 0
    }
    c_dists$X <- c_dists$X / (nchar(c_comparator) - num_x)
    names(c_dists) <- paste("Comp_", comparator_num, sep = '')
    if (is.null(dists)){
      dists <- c_dists
    } else {
      dists <- cbind(dists, c_dists)
    }
  }

  fitness_score <- dists[,1]

  return(list(fitness_score = fitness_score,
              dists = dists,
              the_seq = the_seq))
}
