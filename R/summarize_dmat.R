#blank page is the enemy

#' Summarize distance matrix
#'
#' Summarizes a distance matrix into an average pairwise distance (`avg_hd`),
#' a set of percentile (`perc`) and a density estimate (`dens`). No
#' identifiers are added, that is not part of this function.
#' @param dmat A distance matrix, or any set of observations from a
#' distribution really.
#' @export

summarize_dmat <- function(dmat) {
  avg_hd <- mean(dmat)
  perc <- quantile(dmat, (0:100)/100)
  dens <- density(dmat)
  return(list(avg_hd = avg_hd,
              perc = perc,
              dens = dens))
}
