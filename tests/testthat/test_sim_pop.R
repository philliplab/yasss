context('sim_pop')

library(yasss)

test_that("sim_pop checks arguments correctly", {
  N_GEN_N_POP_INF_NULL <- "Either n_gen or n_pop must be specified and at least one must be finite"
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2), 
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_gen = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_pop = Inf),
               N_GEN_N_POP_INF_NULL)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_gen = Inf, n_pop = Inf),
               N_GEN_N_POP_INF_NULL)

  N_GEN_N_POP_LESS_ONE <- "Neither n_gen nor n_pop may be set to less than one"
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_gen = 0),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_gen = -5),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_pop = 0.4),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_pop = -100),
               N_GEN_N_POP_LESS_ONE)
  expect_error(sim_pop(ancestors = c('AAA'), r0 = 2, n_gen = 0, n_pop = 0),
               N_GEN_N_POP_LESS_ONE)

  GEN_SIZE_VALID <- "r0 must be between 1 and 1e6"
  for (c_r0 in c(-10, 0, 0.4, 1e6+1, 2e7)){
    expect_error(sim_pop(ancestors = c('AAA'), r0 = c_r0, n_gen = 3, n_pop = 50),
                 GEN_SIZE_VALID, info = paste("Varying r0 ", c_r0, sep = ""))
  }
  expect_error(
    expect_warning(sim_pop(ancestors = c('AAA'), r0 = 'many', n_gen = 3, n_pop = 50),
                   "NAs introduced by coercion"),
               GEN_SIZE_VALID)
})

test_that("sim_pop output in the correct format", {
  x <- sim_pop(ancestors = c("AAAA", "CCCC"), 
               r0 = 2,
               n_gen = 1)

  expect_true(all(unlist(check_genealogy(x))))
})

sim_pop_expector <- function(ancestors,
                           r0 = 2,
                           n_gen = NULL,
                           n_pop = NULL){
  genea <- sim_pop(ancestors = ancestors, 
                   r0 = r0, 
                   n_gen = n_gen, 
                   n_pop = n_pop)
  
  # correct genealogy structure
  expect_true(all(unlist(check_genealogy(genea))))

  if (is.null(n_gen)) {n_gen <- Inf}
  if (is.null(n_pop)) {n_pop <- Inf}

  gen_count <- 0

  total_pop <- length(ancestors)
  c_pop <- length(ancestors)
  while (c_pop < n_pop & gen_count < n_gen){
    gen_count <- gen_count + 1
    c_pop <- (c_pop * r0)
    total_pop <- total_pop + c_pop
  }

  # Correct number of individuals who ever lived
  expect_equal(nrow(genea), total_pop)
  # Correct number of generations
  expect_true(all(sort(unique(genea$gen_num)) == c(0:gen_count)))
  # Correct number of individuals in last generation
  expect_equal(nrow(genea %>% filter(gen_num == max(gen_num))), 
               length(ancestors)*r0^gen_count)
}

test_that("n_gen argument of sim_pop works", {
  # Single ancestor
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
                 n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
                 n_gen = 2)
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
                 n_gen = 3)

  # Two ancestors
  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
               n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
               n_gen = 2)
  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
               n_gen = 3)

  # Three ancestors
  sim_pop_expector(ancestors = c("AAAA", "CCCC", "GGGG"), r0 = 2,
               n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA", "CCCC", "GGGG"), r0 = 2,
               n_gen = 2)
  sim_pop_expector(ancestors = c("AAAA", "CCCC", "GGGG"), r0 = 2,
               n_gen = 3)
})

test_that("n_pop argument of sim_pop works", {

  ancestors_list <- list(one = "AAAA",
                         two = c("AAAA", "CCCC"))
  for (ancestors_id in names(ancestors_list)){
    c_ancestors <- ancestors_list[[ancestors_id]]
    for (c_n_pop in c(1,2,3,33)){
      sim_pop_expector(ancestors = c_ancestors, r0 = 2,
                   n_pop = c_n_pop)
    }
  }



#  # One ancestor
#  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
#               n_pop = 1)
#  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
#               n_pop = 2)
#  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
#               n_pop = 3)
#  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
#               n_pop = 33)
#
#  # Two ancestors
#  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
#               n_pop = 1)
#  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
#               n_pop = 2)
#  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
#               n_pop = 3)
#  sim_pop_expector(ancestors = c("AAAA", "CCCC"), r0 = 2,
#               n_pop = 20)
})

test_that("n_pop and n_gen arguments of sim_pop interact correctly", {
  # Single ancestor
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
               n_pop = 1, n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
               n_pop = 2, n_gen = 2)
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
               n_pop = 3, n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA"), r0 = 2,
               n_pop = 33, n_gen = 10)
  
  # Dual ancestor
  sim_pop_expector(ancestors = c("AAAA", "TTTT"), r0 = 2,
               n_pop = 1, n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA", "TTTT"), r0 = 2,
               n_pop = 2, n_gen = 2)
  sim_pop_expector(ancestors = c("AAAA", "TTTT"), r0 = 2,
               n_pop = 3, n_gen = 1)
  sim_pop_expector(ancestors = c("AAAA", "TTTT"), r0 = 2,
               n_pop = 33, n_gen = 10)

  # And a few bigger ones
  sim_pop_expector(ancestors = c("AAAA"), r0 = 3,
               n_pop = 100, n_gen = 5)
#TODO: Uncomment
#  sim_pop_expector(ancestors = c("AAAA", "TTTT"), r0 = 3,
#               n_pop = 200, n_gen = 8)
#  sim_pop_expector(ancestors = c("AAAA", "TTTT", "CCCC"), r0 = 2,
#               n_pop = 200, n_gen = 10)
# Pretty slow
#  sim_pop_expector(ancestors = c("AAAA", "TTTT", "CCCC"), r0 = 5,
#               n_pop = 3000, n_gen = 10)
})
