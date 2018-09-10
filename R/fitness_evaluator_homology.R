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

#  max_homologies <- max_homology(comparators)

  dists <- NULL

  comparator_num <- 0
  c_comparator <- comparators[1]
  for (c_comparator in comparators){
    comparator_num <- comparator_num + 1
    c_dists <- data.frame(X = stringdist(c_comparator, the_seq))
    names(c_dists) <- paste("Comp_", comparator_num, sep = '')
    if (is.null(dists)){
      dists <- c_dists
    } else {
      dists <- cbind(dists, c_dists)
    }
  }

  fitness_score <- dists[,1]/max(dists[,1])

  return(list(fitness_score = fitness_score,
              dists = dists,
              the_seq = the_seq))
}

#' Computes the max homology possible to a set of comparator sequences.
#'
#' Given comparator sequences, compute the max homology that can be achieved to these comparators given the conventions around the handling of X characters. See github issue #43
#'
#' @return A list with an element for each comparator. This element contains the max possible homology between a sequence and the comparator.
#' @param comparators A vector of character strings that form the comparators. Note the special restrictions placed on the usage of X.
#' @export

max_homology <- function(comparators){
  result <- list()
  for (i in 1:length(comparators)){
    num_x <- sum(charToRaw(comparators[i]) == charToRaw("X"))
    result[[comparators[i]]] <- nchar(comparators[i]) - num_x
  }
  return(result)
}
