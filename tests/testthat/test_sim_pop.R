context('sim_pop')

library(yasss)

test_that("sim_pop checks arguments correctly", {
  N_GEN_N_POP_INF_NULL <- "Either n_gen or n_pop must be specified and at least one must be finite"
  expect_error(sim_pop(ancestors = c('AAA')), 
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), n_gen = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), n_pop = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), n_gen = Inf, n_pop = Inf),
               N_GEN_N_POP_INF_NULL)

  N_GEN_N_POP_LESS_ONE <- "Neither n_gen nor n_pop may be set to less than one"
  expect_error(sim_pop(ancestors = c('AAA'), n_gen = 0),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), n_gen = -5),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), n_pop = 0.4),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), n_pop = -100),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), n_gen = 0, n_pop = 0),
               N_GEN_N_POP_LESS_ONE)
})

test_that("sim_pop output in the correct format", {
  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_gen = 1)
  expect_equal(class(x), 'list')
  expect_true("seqs" %in% names(x))
})

test_that("n_gen argument of sim_pop works", {
  # Single ancestor
  x <- sim_pop(ancestors = c("AAAA"),
               n_gen = 1)
  expect_equal(length(x$seqs), 2)

  x <- sim_pop(ancestors = c("AAAA"),
               n_gen = 2)
  expect_equal(length(x$seqs), 4)

  x <- sim_pop(ancestors = c("AAAA"),
               n_gen = 3)
  expect_equal(length(x$seqs), 8)

  # Two ancestors
  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_gen = 1)
  expect_equal(length(x$seqs), 4)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_gen = 2)
  expect_equal(length(x$seqs), 8)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_gen = 3)
  expect_equal(length(x$seqs), 16)
})

test_that("n_pop argument of sim_pop works", {
  # One ancestor
  x <- sim_pop(ancestors = c("AAAA"),
               n_pop = 1)
  expect_equal(length(x$seqs), 1)

  x <- sim_pop(ancestors = c("AAAA"),
               n_pop = 2)
  expect_equal(length(x$seqs), 2)

  x <- sim_pop(ancestors = c("AAAA"),
               n_pop = 3)
  expect_equal(length(x$seqs), 4)
  
  x <- sim_pop(ancestors = c("AAAA"),
               n_pop = 33)
  expect_equal(length(x$seqs), 64)

  # Two ancestors
  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_pop = 1)
  expect_equal(length(x$seqs), 2)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_pop = 2)
  expect_equal(length(x$seqs), 2)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_pop = 3)
  expect_equal(length(x$seqs), 4)
  
  x <- sim_pop(ancestors = c("AAAA", "BBBB"),
               n_pop = 20)
  expect_equal(length(x$seqs), 32)
})

