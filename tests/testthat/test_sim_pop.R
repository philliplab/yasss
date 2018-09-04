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
  x <- sim_pop(ancestors = c("AAAA", "CCCC"), 
               gen_size = 2,
               n_gen = 1)

  expect_true(all(unlist(check_genealogy(x))))
})

sim_pop_tester <- function(ancestors,
                           gen_size = 2,
                           n_gen = NULL,
                           n_pop = NULL){
  genea <- sim_pop(ancestors = ancestors, 
                   gen_size = gen_size, 
                   n_gen = n_gen, 
                   n_pop = n_pop)
  
  # correct genealogy structure
  expect_true(all(unlist(check_genealogy(genea))))

  if (is.null(n_gen)) {n_gen <- Inf}

  gen_count <- 0

  if (n_gen == Inf){
    c_pop <- length(ancestors)
    while (c_pop < n_pop){
      gen_count <- gen_count + 1
      c_pop <- c_pop + (c_pop * gen_size)
    }
  } else {
    gen_count <- n_gen
    c_pop <- length(ancestors) * sum(gen_size^(0:n_gen))
  }

  # Correct number of individuals who ever lived
  expect_equal(nrow(genea), c_pop)
  # Correct number of generations
  expect_true(all(sort(unique(genea$gen_num)) == c(0:gen_count)))
  # Correct number of individuals in last generation
  expect_equal(nrow(genea %>% filter(gen_num == max(gen_num))), 
               length(ancestors)*gen_size^gen_count)
}

test_that("n_gen argument of sim_pop works", {
  # Single ancestor
  sim_pop_tester(ancestors = c("AAAA"), gen_size = 2,
                 n_gen = 1)
  sim_pop_tester(ancestors = c("AAAA"), gen_size = 2,
                 n_gen = 2)
  sim_pop_tester(ancestors = c("AAAA"), gen_size = 2,
                 n_gen = 3)

  # Two ancestors
  sim_pop_tester(ancestors = c("AAAA", "CCCC"), gen_size = 2,
               n_gen = 1)
  sim_pop_tester(ancestors = c("AAAA", "CCCC"), gen_size = 2,
               n_gen = 2)
  sim_pop_tester(ancestors = c("AAAA", "CCCC"), gen_size = 2,
               n_gen = 3)

  # Three ancestors
  sim_pop_tester(ancestors = c("AAAA", "CCCC", "GGGG"), gen_size = 2,
               n_gen = 1)
  sim_pop_tester(ancestors = c("AAAA", "CCCC", "GGGG"), gen_size = 2,
               n_gen = 2)
  sim_pop_tester(ancestors = c("AAAA", "CCCC", "GGGG"), gen_size = 2,
               n_gen = 3)
})

test_that("n_pop argument of sim_pop works", {
  # One ancestor
  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 1)
  expect_equal(nrow(x), 1)
  expect_true(all(sort(unique(x$gen_num)) == c(0)))

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 2)
  expect_equal(nrow(x), 3)
  expect_true(all(sort(unique(x$gen_num)) == 0:1))

  x <- sim_pop(ancestors = c("AAAA"), gen_size = 2,
               n_pop = 3)
  expect_equal(nrow(x), 7)
  expect_true(all(sort(unique(x$gen_num)) == 0:2))
  
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
