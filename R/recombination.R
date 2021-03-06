#' Recombines sequences in a generation
#'
#' Loops over the sequences in a generation, this function executes the
#' following steps:
#' \itemize{
#'   \item Determine if the current sequence should undergo recombination
#'   \item Select the sequence that will be the recombinant partner
#'   \item Call \code{recombine_seqs} to recombine the sequences, and 
#'   \item Populate the genealogy data.frame with all the recombination 
#' information.
#' }
#' 
#' The selection of the recombination partner is a random draw from all
#' non-recombinant sequences in the generation. This is to prevent a sequences
#' from effective under going two recombination events in a single cycle.
#'
#' @return A genealogy data.frame with the recombinants and the recombination
#' related columns populated for each recombinant sequence.
#'
#' @param gen A data.frame from a genealogy that contains just one generation
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

    unrecombined_seq <- which(is.na(recomb_partner))
    potential_partners <- unrecombined_seq[unrecombined_seq != i]
    
    if (length(potential_partners) == 0){
      # not enough unrecombined sequences to select a recombination partner
      next
    }

    if (length(potential_partners) == 1){
      partner <- potential_partners
    } else {
      partner <- sample(potential_partners, 1)
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
#'
#' Given two sequence, pick a breakpoint and form a new sequence using the
#' content to the LHS of the breakpoint of one sequence and the content to the
#' RHS of the breakpoint in the other sequence.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item recombinant The recombined sequence
#'   \item recomb_muts The number of mutations that would have to be
#'   introduced into the target_seq to make it identical to the recombinant.
#'   \item recomb_replaced Was the portion of the target sequence that is to
#'   the right or left of the break point replaced?
#'   \item recomb_pos The location of the breakpoint
#' }
#'
#' @param target_seq The sequence that will be replaced by the recombination
#' @param recombination_partner_seq The recombination partner sequence
#' @param min_dist_to_edge The closest that a recombination event can be to
#' the start or end of either sequence.
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
