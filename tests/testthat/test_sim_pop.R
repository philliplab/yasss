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
