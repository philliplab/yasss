context('recombine_gen')

if (FALSE){
  yasss:::restart_r()
  devtools::load_all()
  library(testthat)
}

genea <- sim_pop(ancestors = paste(rep("A", 60), collapse = ''),
                 r0 = 2,
                 n_gen = 5,
                 n_pop = Inf,
                 mutator = list(fun = "mutator_uniform_fun",
                                args = list(mu = 0.1)),
                 fitness_evaluator = list(fun = "fitness_evaluator_uniform_fun",
                                           args = NULL))

last_gen <- genea %>% filter(gen_num == max(gen_num))

genealogy_cols <-
  c("gen_num", "id", "parent_id", "the_seq", "n_mut", "recomb_pos",
    "recomb_replaced", "recomb_partner", "recomb_muts", "fitness_score")

test_that('recombine_gen works', {
  x <- recombine_gen(last_gen, ps_rate = .5)
  expect_true(class(x) == 'data.frame')
  for (i in genealogy_cols){
    expect_true(i %in% names(x), info = paste("Column ", i, " in output", sep = ''))
  }
  expect_false(all(is.na(x$recomb_pos)))
  expect_false(all(is.na(x$recomb_partner)))
  expect_false(all(is.na(x$recomb_muts)))
  expect_false(all(is.na(x$recomb_replaced)))
})

test_that('recombine_gen checks arguments correctly', {
  ERR_MSG <- 'ps_rate must be greater than or equal to zero and strictly smaller than one'
  expect_error(x <- recombine_gen(last_gen, ps_rate = -1), ERR_MSG, info = 'ps_rate = -1')
  expect_error(x <- recombine_gen(last_gen, ps_rate = 1), ERR_MSG, info = 'ps_rate = 1')
})

test_that('recombine_gen do not construct recombinants with multiple breakpoints', {
  wacko_gen <- last_gen[1:4,]
  wacko_gen$the_seq <- c(
    paste(rep('A', 60), collapse = ''),
    paste(rep('C', 60), collapse = ''),
    paste(rep('G', 60), collapse = ''),
    paste(rep('T', 60), collapse = '')
                         )
  for (i in 1:50){
    x <- recombine_gen(wacko_gen, 0.99)
    c_seq <- x$the_seq[1]
    j <- 1
    for (j in 1:length(x$the_seq)){
      c_seq <- x$the_seq[j]
      s_length <- nchar(c_seq)
      lets <- strsplit(c_seq, '')[[1]]
      if (is.na(x$recomb_pos[j])){
        expect_equal(length(table(lets)), 1)
      } else {
        expect_equal(length(table(lets)), 2)
        obs_breakpoint <- which(lets[1:(s_length-1)] != lets[2:s_length])
        expect_equal(obs_breakpoint, x$recomb_pos[j])
      }
    }
  }
})

context('recombine_seqs')

target_seq <- 'AAAAAAAAAA'
recombination_partner_seq <- 'CCCCCCCCCC'
min_dist_to_edge <- 2

test_that('recombine_seqs works', {
  x <- recombine_seqs(target_seq, recombination_partner_seq, min_dist_to_edge)
  expect_equal(class(x), 'list')
  expect_equal(length(x), 4)
  expect_true('recombinant' %in% names(x))
  expect_true('recomb_muts' %in% names(x))
  expect_true('recomb_replaced' %in% names(x))
  expect_true('recomb_pos' %in% names(x))

  expect_true(class(x$recombinant) == 'character')
  expect_true(class(x$recomb_muts) == 'numeric')
  expect_true(class(x$recomb_replaced) == 'character')
  expect_true(class(x$recomb_pos) == 'numeric')

  expect_equal(x$recomb_muts, stringdist(target_seq, x$recombinant))

  first_char <- substr(x$recombinant, 1, 1)
  last_char <- substr(x$recombinant, 10, 10)
  if (x$recomb_replaced == 'right'){
    expect_equal(first_char, 'A')
    expect_equal(last_char, 'C')
  } else {
    expect_equal(first_char, 'C')
    expect_equal(last_char, 'A')
  }

  last_lhs <- substr(x$recombinant, x$recomb_pos, x$recomb_pos)
  first_rhs <- substr(x$recombinant, x$recomb_pos+1, x$recomb_pos+1)
  expect_false(last_lhs == first_rhs)
  expect_true(x$recomb_pos >= min_dist_to_edge)
  expect_true(x$recomb_pos <= nchar(target_seq) - min_dist_to_edge)
})
