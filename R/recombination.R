#' Recombines sequences in a generation
#' @param gen A data.frame from a genealogy that contains just one
#' generation
#' @export

recombine_gen <- function(gen){
  return(gen)
}

#' Recombines two sequences
#' @param target_seq The sequence that will be replaced by the recombination
#' @param recombination_partner_seq The recombination partner sequence
#' @export

recombine_seqs <- function(target_seq, recombination_partner_seq, min_dist_to_edge = 5){
  stopifnot(length(target_seq) == 1)
  stopifnot(length(recombination_partner_seq) == 1)
  stopifnot(nchar(target_seq) == nchar(recombination_partner_seq))
  stopifnot(min_dist_to_edge < nchar(target_seq)/2.2)
  stopifnot(min_dist_to_edge >= 0 & min_dist_to_edge <= nchar(target_seq))

  breakpoint <- runif(1, min_dist_to_edge, nchar(target_seq) - min_dist_to_edge)
  target_on_LHS <- ifelse(runif(1) > 0.5, TRUE, FALSE)

  if (target_on_LHS){
    recombinant_LHS <- substr(target_seq, 1, breakpoint)
    recombinant_RHS <- substr(recombination_partner_seq, breakpoint + 1, nchar(target_seq))
  } else {
    recombinant_LHS <- substr(recombination_partner_seq, 1, breakpoint)
    recombinant_RHS <- substr(target_seq, breakpoint + 1, nchar(target_seq))
  }

  recombinant <- paste(recombinant_LHS, recombinant_RHS, sep = '')
  dist_to_target_seq <- stringdist(recombinant, target_seq, method = 'hamming')

  return(list(recombinant = recombinant,
              dist_to_target_seq = dist_to_target_seq,
              target_on_LHS = target_on_LHS))
}
