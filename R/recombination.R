#' Recombines sequences in a generation
#' @param gen A data.frame from a genealogy that contains just one
#' generation
#' @param ps_rate The chance that any given sequence will be a recombinant.
#' @export

recombine_gen <- function(gen, ps_rate = 0){
  if (ps_rate == 0){
    return(gen)
  }
  if (ps_rate < 0 | ps_rate >= 1){
    stop('ps_rate must be greater than or equal to zero and strictly smaller than one')
  }
  recombinants <- which(runif(nrow(gen)) < ps_rate)

  the_seq <- gen[,'the_seq']
  recomb_partner <- gen[,'recomb_partner']
  recomb_replaced <- gen[,'recomb_replaced']
  recomb_muts <- gen[,'recomb_muts']
  recomb_pos <- gen[,'recomb_pos']

  for (i in recombinants){
    # don't allow 2 breakpoints in one sequence
    if (!is.na(gen$recomb_pos[i])){
      next
    }
    partner <- FALSE
    while (!partner){
      partner <- sample(1:nrow(gen), 1)
# Need more logic to prevent infinite loop
#      if (!is.na(gen$recomb_pos[partner])){
#        partner <- FALSE
#      }
      if (partner == i){
        partner <- FALSE
      }
    }
    target_seq <- gen[i, 'the_seq']
    recombination_partner_seq <- gen[partner, 'the_seq']
    recombinant <- recombine_seqs(target_seq = target_seq,
                                  recombination_partner_seq = recombination_partner_seq)

    the_seq[i] <- recombinant$recombinant
    recomb_partner[i] <- partner
    recomb_replaced[i] <- recombinant$recomb_replaced
    recomb_muts[i] <- recombinant$recomb_muts
    recomb_pos[i] <- recombinant$recomb_pos
  }
  gen[, 'the_seq'] <- the_seq
  gen[, 'recomb_partner'] <- recomb_partner
  gen[, 'recomb_replaced'] <- recomb_replaced
  gen[, 'recomb_muts'] <- recomb_muts
  gen[, 'recomb_pos'] <- recomb_pos
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

  breakpoint <- round(runif(1, min_dist_to_edge, nchar(target_seq) - min_dist_to_edge), 0)
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
              recomb_muts = dist_to_target_seq,
              recomb_replaced = ifelse(target_on_LHS, 'right', 'left'),
              recomb_pos = breakpoint))
}
