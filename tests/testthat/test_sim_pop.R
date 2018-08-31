context('sim_pop')

library(yasss)

test_that("sim_pop checks arguments correctly", {
  N_GEN_N_POP_INF_NULL <- "Either n_gen or n_pop must be specified and at least one must be finite"
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2), 
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_gen = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_pop = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_gen = Inf, n_pop = Inf),
               N_GEN_N_POP_INF_NULL)

  N_GEN_N_POP_LESS_ONE <- "Neither n_gen nor n_pop may be set to less than one"
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_gen = 0),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_gen = -5),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_pop = 0.4),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_pop = -100),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2, n_gen = 0, n_pop = 0),
               N_GEN_N_POP_LESS_ONE)

  GEN_SIZE_VALID <- "gen_size must be between 1 and 1e6"
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = -10, n_gen = 3, n_pop = 50),
               GEN_SIZE_VALID)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 0, n_gen = 3, n_pop = 50),
               GEN_SIZE_VALID)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 0.4, n_gen = 3, n_pop = 50),
               GEN_SIZE_VALID)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 1e6+1, n_gen = 3, n_pop = 50),
               GEN_SIZE_VALID)
  expect_error(sim_pop(ancestors = c('AAA'), gen_size = 2e7, n_gen = 3, n_pop = 50),
               GEN_SIZE_VALID)
  expect_error(
    expect_warning(sim_pop(ancestors = c('AAA'), gen_size = 'many', n_gen = 3, n_pop = 50),
                   "NAs introduced by coercion"),
               GEN_SIZE_VALID)
})

test_that("sim_pop output in the correct format", {
  x <- sim_pop(ancestors = c("AAAA", "BBBB"), 
               gen_size = 2,
               n_gen = 1)
  expect_equal(class(x), 'data.frame')
  expect_true("the_seq" %in% names(x))
})

test_that("n_gen argument of sim_pop works", {
  # Single ancestor
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_gen = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_gen = 2)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 4)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_gen = 3)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 8)

  # Two ancestors
  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_gen = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 4)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_gen = 2)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 8)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_gen = 3)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 16)
})

test_that("n_pop argument of sim_pop works", {
  # One ancestor
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 1)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 2)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 3)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 4)
  
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 33)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 64)

  # Two ancestors
  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_pop = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_pop = 2)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)

  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_pop = 3)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 4)
  
  x <- sim_pop(ancestors = c("AAAA", "BBBB"), gen_size = 2,
               n_pop = 20)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 32)
})

test_that("n_pop and n_gen arguments of sim_pop interact correctly", {
            # Single ancestor
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 1, n_gen = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 1)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 2, n_gen = 2)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 3, n_gen = 1)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2)
  
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 33, n_gen = 10)
  expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 64)
})
